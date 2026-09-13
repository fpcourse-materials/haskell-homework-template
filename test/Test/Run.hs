module Test.Run (NamedTests, testMain, nameTests) where

import Control.Monad (forM, unless)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv, getArgs)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit (Test (..), Counts (..))
import Test.HUnit.Base qualified as HU
import Test.Manifest

type NamedTests = [(String, Test)]
data TestStatus = TestPassed | TestPartial Float | TestFailed deriving Eq
data TestResult = TestResult { testName :: String, testStatus :: TestStatus }
type TestReport = [TestResult]

-- | Строка итогового отчёта: задача из манифеста либо запущенный тест вне манифеста.
data TaskResult = TaskResult { taskId :: TaskId, taskLevel :: Maybe Int, taskStatus :: TaskStatus }
type TaskReport = [TaskResult]

instance Semigroup Counts where
  c <> c' = Counts
    { errors = errors c + errors c'
    , failures = failures c + failures c'
    , tried = tried c + tried c'
    , cases = cases c + cases c'
    }

instance Monoid Counts where
  mempty = Counts 0 0 0 0

testMain :: NamedTests -> IO ()
testMain tests = do
  testFilters <- getTestFilters
  manifest <- fromMaybe (Manifest []) <$> readManifest "TASKS"
  -- Опечатка в TASKS дала бы задачу, которая вечно TODO, и уровень 1 никогда бы не закрылся.
  let unknown = filter (`notElem` map fst tests) $ map snd $ manifestTasks manifest
  unless (null unknown) do
    putStrLn $ "TASKS mentions tasks that have no tests: " <> unwords unknown
    exitFailure
  putStrLn $ "Executing: " <>
    if null testFilters then "all tests" else List.intercalate ", " testFilters
  (counts, report) <- runTests $ filterTests testFilters tests
  lookupEnv "HASKELL_TEST_REPORT" >>= maybe (pure ()) \filePath ->
    appendFile filePath $ showMachine report
  let taskReport = collectTaskReport manifest report
      closed = level1Closed manifest $ statusOf taskReport
      missing = level1Missing manifest $ statusOf taskReport
  lookupEnv "HASKELL_TEST_REPORT_JSON" >>= maybe (pure ()) \filePath ->
    writeFile filePath $ showJson taskReport closed
  putStrLn $ "\n" <> showCounts counts <> "\n"
  putStr $ showTaskReport taskReport
  putStrLn $ "Level 1: " <> if closed then "closed" else "open (missing: " <> unwords missing <> ")"
  if statusFromCounts counts == TestPassed then exitSuccess else exitFailure
  where
    getTestFilters :: IO [String]
    getTestFilters = concatMap words <$> getArgs

    filterTests :: [String] -> NamedTests -> NamedTests
    filterTests names = filter \(name, _) -> null names || name `elem `names

    statusOf :: TaskReport -> TaskId -> TaskStatus
    statusOf taskReport name = maybe TaskTodo taskStatus $ List.find ((== name) . taskId) taskReport

nameTests :: Int -> [Test] -> NamedTests
nameTests iBlock = map wrapToTestLabel . zipWith mkNamedTest [1 :: Int ..]
  where
    wrapToTestLabel (label, test) = (label, TestLabel label test)
    mkNamedTest iTask test = (show iBlock <> "." <> show iTask, test)

statusFromCounts :: Counts -> TestStatus
statusFromCounts Counts {..}
  | errors == 0 && failures == 0 = TestPassed
  | errors + failures < tried = TestPartial $
      1 - fromIntegral (errors + failures) / fromIntegral tried
  | otherwise = TestFailed

taskStatusFromTest :: TestStatus -> TaskStatus
taskStatusFromTest = \case
  TestPassed -> TaskDone
  TestPartial percent -> TaskPartial percent
  TestFailed -> TaskFailed

showCounts :: Counts -> String
showCounts Counts {..} = concat
  [ "Cases: ", show cases
  , "  Tried: ", show tried
  , "  Errors: ", show errors
  , "  Failures: ", show failures
  ]

showMachine :: TestReport -> String
showMachine = List.intercalate "\n" . map showResult
  where
    showResult TestResult {..} = testName <> "=" <> case testStatus of
      TestPassed -> "DONE"
      TestPartial percent -> "PARTIAL " <> show percent
      TestFailed -> "FAILED"

-- | Задачи манифеста в его порядке (незапущенные получают 'TaskTodo'),
-- затем запущенные тесты, которых в манифесте нет (без уровня).
collectTaskReport :: Manifest -> TestReport -> TaskReport
collectTaskReport manifest report = map fromManifest (manifestTasks manifest) ++ map fromRun unlisted
  where
    fromManifest (level, name) = TaskResult
      { taskId = name
      , taskLevel = Just level
      , taskStatus = maybe TaskTodo (taskStatusFromTest . testStatus) $
          List.find ((== name) . testName) report
      }
    unlisted = filter (\TestResult {..} -> testName `notElem` map snd (manifestTasks manifest)) report
    fromRun TestResult {..} = TaskResult
      { taskId = testName
      , taskLevel = Nothing
      , taskStatus = taskStatusFromTest testStatus
      }

showTaskReport :: TaskReport -> String
showTaskReport = unlines . map showTask
  where
    showTask TaskResult {..} = List.intercalate "  "
      [ padRight 6 taskId
      , "level " <> maybe "-" show taskLevel
      , showStatus taskStatus
      ]
    showStatus = \case
      TaskDone -> "DONE"
      TaskPartial percent -> "PARTIAL " <> show (floor $ percent * 100) <> "%"
      TaskFailed -> "FAILED"
      TaskTodo -> "TODO"
    padRight n s = s <> replicate (n - length s) ' '

-- | Без aeson: структура плоская, а идентификаторы состоят из цифр и точек.
showJson :: TaskReport -> Bool -> String
showJson taskReport closed = jsonObject
  [ ("tasks", jsonArray $ map showTask taskReport)
  , ("level1_closed", if closed then "true" else "false")
  ]
  where
    showTask TaskResult {..} = jsonObject $
      [ ("id", jsonString taskId)
      , ("level", maybe "null" show taskLevel)
      , ("status", jsonString $ statusName taskStatus)
      ] ++ case taskStatus of
        TaskPartial percent -> [("progress", show percent)]
        _ -> []
    statusName = \case
      TaskDone -> "DONE"
      TaskPartial _ -> "PARTIAL"
      TaskFailed -> "FAILED"
      TaskTodo -> "TODO"
    jsonObject fields = "{" <> List.intercalate "," [jsonString key <> ":" <> value | (key, value) <- fields] <> "}"
    jsonArray items = "[" <> List.intercalate "," items <> "]"
    jsonString s = "\"" <> concatMap escape s <> "\""
    escape = \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      c -> [c]

runTests :: NamedTests -> IO (Counts, TestReport)
runTests tests = collectReport <$> forM tests \(name, test) -> do
  putStrLn $ "Running test \"" <> name <> "\":"
  (counts, _) <- HU.performTest reportStart reportError reportFailure () test
  let result = TestResult { testName = name, testStatus = statusFromCounts counts }
  reportSummary counts
  pure (counts, result)
  where
    reportStart _ _ = pure () -- per test case
    reportError loc msg =
      -- Hack to distinguish not implemented cases
      let isTodo = "Not implemented" `List.isSubsequenceOf` msg in
      let prefix = if isTodo then "[TODO] " else "[ERROR] " in
      reportProblem prefix loc msg
    reportFailure = reportProblem "[FAILURE] "
    reportProblem prefix _ msg HU.State{..} () = putStr $ padLines 4 $
      prefix <> showPath path <> " " <> msg <> if '\n' `elem` msg then "\n" else ""
    reportSummary counts = putStr $ padLines 4 $ case statusFromCounts counts of
      TestPassed -> "Done :)"
      TestPartial percent -> "In progress, " <> show (floor $ percent * 100) <> "% tests remain :|"
      TestFailed -> "Nothing here :("

padLines :: Int -> String -> String
padLines nSpaces = unlines . map (replicate nSpaces ' ' ++) . lines

collectReport :: [(Counts, TestResult)] -> (Counts, TestReport)
collectReport = sequenceA

showPath :: HU.Path -> String
showPath = List.intercalate ":" . reverse . map \case
  HU.ListItem n -> show n
  HU.Label label -> label
