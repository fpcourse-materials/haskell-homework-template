module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStr, hPutStrLn, stderr)

import Lambda.Check (Session (..), checkFile, loadSession, prettyResults, reportOk)
import Lambda.Color
  ( ColorMode (..)
  , Palette (..)
  , detectPalette
  , parseColorMode
  , withCode
  )
import Lambda.Command
  ( CommandResult (..)
  , renderResult
  , replHelp
  , runLine
  )

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err           -> die err
    Right ActionHelp   -> putStr usage
    Right (ActionCheck file) -> runCheck file
    Right (ActionLoad load)  -> runLoad load

data Action
  = ActionHelp
  | ActionCheck FilePath
  | ActionLoad LoadArgs

data LoadArgs = LoadArgs FilePath [String] ColorMode

parseArgs :: [String] -> Either String Action
parseArgs [] = Left (usage ++ "\nlambda: missing command")
parseArgs ["-h"] = Right ActionHelp
parseArgs ["--help"] = Right ActionHelp
parseArgs ("check" : rest) = parseCheck rest
parseArgs ("load" : rest) = parseLoad rest
parseArgs (cmd : _) =
  Left ("lambda: unknown command ‘" ++ cmd ++ "’\n" ++ usage)

parseCheck :: [String] -> Either String Action
parseCheck ["-h"] = Right ActionHelp
parseCheck ["--help"] = Right ActionHelp
parseCheck [file] = Right (ActionCheck file)
parseCheck [] = Left "lambda check: missing FILE"
parseCheck _ = Left "lambda check: too many arguments"

parseLoad :: [String] -> Either String Action
parseLoad = go [] ColorAuto Nothing
  where
    go evals color file [] =
      case file of
        Nothing -> Left "lambda load: missing FILE"
        Just f  -> Right (ActionLoad (LoadArgs f evals color))
    go _ _ _ ["-h"] = Right ActionHelp
    go _ _ _ ["--help"] = Right ActionHelp
    go evals color file (flag : rest)
      | flag == "-h" || flag == "--help" = Right ActionHelp
      | flag == "--no-color" = go evals ColorNever file rest
      | flag == "-e" || flag == "--eval" = case rest of
          (cmd : rest') -> go (evals ++ [cmd]) color file rest'
          []            -> Left "lambda load: -e needs a command"
      | "--eval=" `isPrefixOf` flag =
          go (evals ++ [drop 7 flag]) color file rest
      | "--color=" `isPrefixOf` flag =
          setColor evals file rest (drop 8 flag)
      | flag == "--color" = case rest of
          (when_ : rest') -> setColor evals file rest' when_
          []              -> Left "lambda load: --color needs auto|always|never"
      | "-" `isPrefixOf` flag =
          Left ("lambda load: unknown option ‘" ++ flag ++ "’")
      | otherwise = case file of
          Just _  -> Left "lambda load: too many arguments"
          Nothing -> go evals color (Just flag) rest

    setColor evals file rest when_ =
      case parseColorMode when_ of
        Just c  -> go evals c file rest
        Nothing -> Left "lambda load: --color needs auto|always|never"

runCheck :: FilePath -> IO ()
runCheck file = do
  pal <- detectPalette ColorAuto
  result <- checkFile file
  case result of
    Left err -> do
      hPutStrLn stderr (withCode (palError pal) (palReset pal) (stripNL err))
      exitFailure
    Right tasks -> do
      putStrLn (colorStatuses pal (prettyResults tasks))
      if reportOk tasks then exitSuccess else exitFailure

colorStatuses :: Palette -> String -> String
colorStatuses pal text = intercalate "\n" (map paintLine (lines text))
  where
    paintLine l
      | ": DONE" `isSuffixOf` l = withCode (palOk pal) (palReset pal) l
      | ": FAILED" `isSuffixOf` l = withCode (palError pal) (palReset pal) l
      | ": PARTIAL" `isInfixOf` l = withCode (palFalse pal) (palReset pal) l
      | otherwise = l

runLoad :: LoadArgs -> IO ()
runLoad (LoadArgs path evals colorMode) = do
  pal <- detectPalette colorMode
  loaded <- loadSession path
  case loaded of
    Left err -> do
      hPutStr stderr (withCode (palError pal) (palReset pal) (stripNL err))
      hPutStrLn stderr ""
      exitFailure
    Right sess ->
      case evals of
        [] -> do
          putStrLn ("Loaded " ++ sessionFile sess)
          repl sess pal
        cmds -> runEvals sess pal cmds

stripNL :: String -> String
stripNL = reverse . dropWhile (== '\n') . reverse

runEvals :: Session -> Palette -> [String] -> IO ()
runEvals sess pal = go (sessionCtx sess)
  where
    go _ [] = return ()
    go ctx (cmd : rest) = do
      let (ctx', result) = runLine ctx cmd
          shown          = renderResult pal result
      case result of
        CommandQuit -> exitSuccess
        CommandErr _ -> do
          hPutStrLn stderr shown
          exitFailure
        _ -> do
          putStrLn shown
          go ctx' rest

repl :: Session -> Palette -> IO ()
repl sess pal = runInputT settings (loop (sessionCtx sess))
  where
    -- Filename completion would steal Tab; arrows and history still work.
    -- The prompt is uncolored so Haskeline's cursor width stays correct.
    settings = (defaultSettings :: Settings IO)
      { complete = noCompletion
      , autoAddHistory = True
      }
    loop ctx = handleInterrupt (loop ctx) $ do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just line ->
          case words line of
            [] -> loop ctx
            _  ->
              let (ctx', result) = runLine ctx line
                  shown          = renderResult pal result
              in  case result of
                    CommandQuit -> return ()
                    CommandErr _ -> do
                      liftIO (hPutStrLn stderr shown)
                      loop ctx
                    _ -> do
                      liftIO (putStrLn shown)
                      loop ctx'

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure

usage :: String
usage = unlines
  [ "Usage:"
  , "  lambda check FILE"
  , "  lambda load [OPTIONS] FILE"
  , ""
  , "check  Run every task of a .lam file and print DONE / PARTIAL / FAILED."
  , "load   Load the definitions of a .lam file and start a REPL."
  , ""
  , "load options:"
  , "  -e CMD, --eval CMD   run a REPL command and exit (repeatable)"
  , "  --color WHEN         auto (default), always, or never"
  , "  --no-color           same as --color=never"
  , ""
  , "File format: see FORMAT.md. Expressions:"
  , "  x y z           application (left-associative)"
  , "  \\x y. body      lambda (also λx y. body and \\x y -> body)"
  , "  (expr)          grouping"
  , "  3               Church numeral ⌜3⌝ (language pure) or Int literal (typed)"
  , "  ...             hole (incomplete)"
  , "  -- comment      to end of line"
  , ""
  , replHelp
  ]
