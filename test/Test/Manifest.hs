-- | Манифест задач домашки: файл @TASKS@ в корне пакета.
--
-- Грамматика (пустые строки и строки, начинающиеся с @--@, игнорируются):
--
-- > level 1
-- > task 1.1 (15 мин)
-- > task 1.2 (30 мин)
-- > rule any 2 of: 1.1 1.2 1.3
-- > level 2
-- > task 2.1 (10 мин)
module Test.Manifest
  ( TaskId, TaskStatus (..), Rule (..), Level (..), Manifest (..)
  , parseManifest, readManifest
  , manifestTasks, levelOf, level1Missing, level1Closed
  ) where

import Control.Exception (throwIO, try)
import Control.Monad (foldM, unless)
import Data.List qualified as List
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)

type TaskId = String

-- | Статус задачи в итоговом отчёте.
-- В отличие от результата прогона, у задачи из манифеста может не быть результата вовсе.
data TaskStatus = TaskDone | TaskPartial Float | TaskFailed | TaskTodo deriving (Eq, Show)

-- | @rule any k of: ids@ — из перечисленных задач достаточно закрыть @k@.
data Rule = AnyOf Int [TaskId] deriving (Eq, Show)

data Level = Level
  { levelNumber :: Int
  , levelTasks :: [TaskId]
  , levelRules :: [Rule]
  } deriving (Eq, Show)

newtype Manifest = Manifest { manifestLevels :: [Level] } deriving (Eq, Show)

parseManifest :: String -> Either String Manifest
parseManifest = fmap (Manifest . reverse) . foldM step [] . meaningful . zip [1 :: Int ..] . lines
  where
    meaningful = filter (\(_, line) -> not $ null line || "--" `List.isPrefixOf` line) . map (fmap strip)
    strip = List.dropWhileEnd isBlank . dropWhile isBlank
    isBlank c = c `elem` " \t\r"

    step levels (n, line) = case words line of
      ("level" : number : _) -> do
        level <- maybe (failAt n $ "bad level number: " <> number) pure $ readMaybe number
        pure $ Level { levelNumber = level, levelTasks = [], levelRules = [] } : levels
      ("task" : taskId : _) -> withCurrent n levels \level ->
        pure level { levelTasks = levelTasks level ++ [taskId] }
      ("rule" : "any" : count : "of:" : taskIds) -> withCurrent n levels \level -> do
        k <- maybe (failAt n $ "bad count: " <> count) pure $ readMaybe count
        unless (k >= 1 && k <= length taskIds) $
          failAt n $ "rule wants " <> show k <> " of " <> show (length taskIds) <> " tasks"
        case filter (`notElem` levelTasks level) taskIds of
          [] -> pure ()
          unknown -> failAt n $ "rule mentions tasks not in the current level: " <> unwords unknown
        pure level { levelRules = levelRules level ++ [AnyOf k taskIds] }
      _ -> failAt n $ "unrecognised line: " <> line

    withCurrent n levels update = case levels of
      [] -> failAt n "task or rule before any level"
      level : rest -> (: rest) <$> update level

    failAt n msg = Left $ "TASKS:" <> show n <> ": " <> msg

-- | Читает манифест из файла. Если файла нет — 'Nothing'; если он не разбирается — ошибка.
readManifest :: FilePath -> IO (Maybe Manifest)
readManifest path = try (readFile path) >>= \case
  Left e | isDoesNotExistError e -> pure Nothing
         | otherwise -> throwIO e
  Right contents -> either (ioError . userError) (pure . Just) $ parseManifest contents

-- | Все задачи манифеста в порядке объявления вместе с номером уровня.
manifestTasks :: Manifest -> [(Int, TaskId)]
manifestTasks = concatMap (\Level {..} -> map (levelNumber,) levelTasks) . manifestLevels

levelOf :: Manifest -> TaskId -> Maybe Int
levelOf manifest taskId = lookup taskId [(t, l) | (l, t) <- manifestTasks manifest]

-- | Задачи уровня 1, которые ещё не закрыты с учётом правил «любые k из n».
-- Задача считается закрытой, если она 'TaskDone' либо входит в правило,
-- у которого закрыто не меньше @k@ перечисленных задач.
level1Missing :: Manifest -> (TaskId -> TaskStatus) -> [TaskId]
level1Missing Manifest {..} statusOf = concatMap missing $ filter ((== 1) . levelNumber) manifestLevels
  where
    missing Level {..} = filter (not . satisfied levelRules) levelTasks
    satisfied rules taskId = done taskId || any (coveredBy taskId) rules
    coveredBy taskId (AnyOf k taskIds) = taskId `elem` taskIds && length (filter done taskIds) >= k
    done taskId = statusOf taskId == TaskDone

-- | Уровень 1 закрыт, когда все его задачи закрыты. Если уровня 1 нет — закрыт.
level1Closed :: Manifest -> (TaskId -> TaskStatus) -> Bool
level1Closed manifest = null . level1Missing manifest
