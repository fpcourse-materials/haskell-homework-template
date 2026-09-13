{-# LANGUAGE RankNTypes #-}
module Main (main) where

import Control.Monad (filterM, unless, when)
import Data.IORef
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

import Lambda

-- | The test data lives next to this package; when the checker is vendored
-- into a homework repository (as meta-utils/lambda), tests run from that
-- repository's root instead.
dataFile :: FilePath -> IO FilePath
dataFile name = do
  let candidates = [ prefix ++ name | prefix <- ["", "meta-utils/lambda/", "lambda/"] ]
  existing <- filterM doesFileExist candidates
  return (case existing of
    (path : _) -> path
    [] -> name)

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check label ok = unless ok $ do
        putStrLn ("FAIL: " ++ label)
        modifyIORef failures (+ 1)
      eq label expected actual = do
        when (expected /= actual) $ do
          putStrLn ("FAIL: " ++ label)
          putStrLn ("  expected: " ++ show expected)
          putStrLn ("  actual:   " ++ show actual)
        check label (expected == actual)

  parseTests eq check
  evalTests eq check
  typeTests eq check
  decodeTests eq
  fileTests eq check

  n <- readIORef failures
  if n == 0 then putStrLn "All tests passed." else do
    putStrLn (show n ++ " failure(s)")
    exitFailure

type Eq' = forall a. (Eq a, Show a) => String -> a -> a -> IO ()
type Check = String -> Bool -> IO ()

mustParse :: String -> Expr
mustParse src = case parseExpr src of
  Left err -> error ("parse failed: " ++ src ++ "\n" ++ err)
  Right e -> e

pure' :: String -> Expr
pure' = desugarNumerals . mustParse

nfOf :: String -> String
nfOf src = case normalForm defaultLimit (pure' src) of
  Left TooManySteps -> error ("step limit: " ++ src)
  Right e -> prettyExpr e

------------------------------------------------------------------------

parseTests :: Eq' -> Check -> IO ()
parseTests eq check = do
  eq "dot" (Lam "x" Nothing (Var "x")) (mustParse "\\x. x")
  eq "arrow" (Lam "x" Nothing (Var "x")) (mustParse "\\x -> x")
  eq "unicode lambda" (Lam "x" Nothing (Lam "y" Nothing (Var "y"))) (mustParse "λx y. y")
  eq "app assoc" (App (App (Var "f") (Var "x")) (Var "y")) (mustParse "f x y")
  eq "trailing lambda" (App (Var "f") (Lam "x" Nothing (Var "x"))) (mustParse "f \\x. x")
  eq "annotation" (Lam "x" (Just (TArr (TVar "a") (TVar "a"))) (Var "x")) (mustParse "\\x:(a -> a). x")
  eq "literal" (Lit 3) (mustParse "3")
  eq "numeral" (churchNumeral 2) (pure' "2")
  eq "subst" (Subst [("x", Var "S")] (App (Var "x") (Var "y"))) (mustParse "([x := S] x y)")
  eq "subst2" (Subst [("x", Var "S"), ("y", Var "K")] (Var "x")) (mustParse "([x := S, y := K] x)")
  eq "prime names" (Var "K'") (mustParse "K'")
  case mustParse "..." of
    Hole _ -> return ()
    other -> eq "hole" "Hole" (show other)
  check "keyword rejected" (either (const True) (const False) (parseExpr "expect x"))
  eq "pretty" "\\x y. x (y z)" (prettyExpr (mustParse "\\x. \\y. x (y z)"))
  eq "pretty parens" "(\\x. x) (\\y. y)" (prettyExpr (mustParse "(\\x. x) (\\y. y)"))
  eq "pretty annotation" "\\x:(a -> b) y:a. x y" (prettyExpr (mustParse "\\x:(a -> b) y:a. x y"))
  eq "type parse" (TArr (TArr (TVar "a") (TVar "b")) (TVar "a")) (either error id (parseType "(a -> b) -> a"))
  eq "type pretty" "(a -> b) -> a -> b" (prettyType (either error id (parseType "(a -> b) -> (a -> b)")))

------------------------------------------------------------------------

evalTests :: Eq' -> Check -> IO ()
evalTests eq check = do
  eq "id id" "\\y. y" (nfOf "(\\x. x) (\\y. y)")
  eq "capture" "\\y'. y" (nfOf "(\\x. \\y. x) y")
  eq "numerals" "\\s z. s (s (s z))" (nfOf "(\\n m s z. n s (m s z)) 1 2")
  check "omega diverges" $ case normalForm 100 (pure' "(\\x. x x) (\\x. x x)") of
    Left TooManySteps -> True
    Right _ -> False
  let ctx = pureCtx [("I", pure' "\\x. x"), ("K", pure' "\\x y. x")]
  eq "delta" "x" (either (error . show) prettyExpr (normalFormWith Lazy ctx 100 (pure' "K x I")))
  eq "alpha" True (alphaEq (pure' "\\x. x y") (pure' "\\z. z y"))
  eq "alpha free" False (alphaEq (pure' "\\x. x y") (pure' "\\z. z w"))
  eq "beta paths" 2 (length (betaPaths (pure' "(\\x. x) ((\\y. y) z)")))
  eq "unfoldings" 2 (length (unfoldings ctx (pure' "K I")))
  eq "expand" "\\x y. x" (prettyExpr (expandAll (ctxEnv ctx) (Var "K")))
  eq "eta" "f" (prettyExpr (etaReduce (pure' "\\x. f x")))
  -- strategies
  let term = pure' "(\\x. y) ((\\z. z) w)"
  eq "normal step" "y" (afterStep Lazy ctx term)
  eq "applicative step" "(\\x. y) w" (afterStep Applicative ctx term)
  eq "strict step" "(\\x. y) w" (afterStep Strict ctx term)
  -- shapes
  check "whnf" (isWhnf (pure' "\\x. (\\y. y) x"))
  check "not hnf" (not (isHnf (pure' "\\x. (\\y. y) x")))
  check "hnf" (isHnf (pure' "\\x. x ((\\y. y) x)"))
  check "not nf" (not (isNf (pure' "\\x. x ((\\y. y) x)")))
  check "normalizing" (normalizing ctx 1000 (pure' "K I I"))
  check "not normalizing" (not (normalizing ctx 1000 (pure' "(\\x. x x) (\\x. x x)")))
  -- β-equivalence
  let y = ("Y", pure' "\\f. (\\x. f (x x)) (\\x. f (x x))")
      ctxY = pureCtx [y, ("g", pure' "\\f m. m f"), ("F", pure' "Y g")]
  eq "betaEq nf" Equal (betaEq ctx 1000 (pure' "K x y") (pure' "x"))
  eq "betaEq Y" Equal (betaEq ctxY 2000 (pure' "F m") (pure' "m F"))
  check "betaEq differ" $ case betaEq ctx 1000 (pure' "K x y") (pure' "y") of
    Differ _ _ -> True
    _ -> False
  eq "reachable" True (any (alphaEq (pure' "x")) (reachable ctx 3 (pure' "K x (I I)")))
  where
    afterStep strat ctx e = case stepBeta strat ctx e of
      Stepped _ _ _ after -> prettyExpr after
      NoRedex _ -> "no redex"

------------------------------------------------------------------------

typeTests :: Eq' -> Check -> IO ()
typeTests eq check = do
  let infer src = either (error . show) prettyTypeGreek (inferType [] (mustParse src))
  eq "id" "α -> α" (infer "\\x. x")
  eq "K" "α -> β -> α" (infer "\\x y. x")
  eq "S" "(α -> β -> γ) -> (α -> β) -> α -> γ" (infer "\\x y z. x z (y z)")
  eq "annotation kept" "(a -> a) -> a -> a" (infer "\\x:(a -> a). x")
  check "omega untypable" (either (const True) (const False) (inferType [] (mustParse "\\x. x x")))
  check "renaming" (typesEqualUpToRenaming (ty "a -> b -> a") (ty "b -> a -> b"))
  check "not renaming" (not (typesEqualUpToRenaming (ty "a -> b -> a") (ty "a -> a -> a")))
  check "instance" (isInstanceOf (ty "a -> a -> a") (ty "a -> b -> a"))
  check "not instance" (not (isInstanceOf (ty "a -> b -> a") (ty "a -> a -> a")))
  let churchOk = checkChurch [] (mustParse "(\\x:(a -> a). x) (\\y:a. y)")
  eq "church" (Right "a -> a") (fmap prettyTypeGreek churchOk)
  check "church missing annotation" (either (const True) (const False) (checkChurch [] (mustParse "\\x. x")))
  check "inhabited" (inhabitable 4 (ty "(a -> a) -> a -> a"))
  check "empty type" (not (inhabitable 4 (ty "a")))
  check "peirce not inhabited" (not (inhabitable 4 (ty "((a -> b) -> a) -> a")))
  where
    ty = either error id . parseType

------------------------------------------------------------------------

decodeTests :: Eq' -> IO ()
decodeTests eq = do
  eq "numeral" "⌜3⌝" (prettyDecoded (pure' "3"))
  eq "true" "true" (prettyDecoded (pure' "\\a b. a"))
  eq "pair" "⟨⌜1⌝, ⌜2⌝⟩" (prettyDecoded (pure' "\\p. p 1 2"))
  eq "list" "[⌜1⌝, ⌜2⌝]" (prettyDecoded (pure' "\\c n. c 1 (c 2 n)"))
  eq "plain" "\\x. x" (prettyDecoded (pure' "\\x. x"))

------------------------------------------------------------------------

fileTests :: Eq' -> Check -> IO ()
fileTests eq check = do
  ok <- dataFile "test/data/ok.lam" >>= checkFile
  case ok of
    Left err -> check ("ok.lam loads: " ++ err) False
    Right tasks -> do
      eq "ok.lam tasks" 9 (length tasks)
      mapM_ (\t -> eq ("ok.lam " ++ trId t ++ " " ++ show (trChecks t)) Done (taskStatus t)) tasks
  typed <- dataFile "test/data/typed.lam" >>= checkFile
  case typed of
    Left err -> check ("typed.lam loads: " ++ err) False
    Right tasks -> mapM_ (\t -> eq ("typed.lam " ++ trId t ++ " " ++ show (trChecks t)) Done (taskStatus t)) tasks
  bad <- dataFile "test/data/bad.lam" >>= checkFile
  case bad of
    Left err -> check ("bad.lam loads: " ++ err) False
    Right tasks -> do
      let status tid = maybe (error tid) taskStatus (lookup tid [ (trId t, t) | t <- tasks ])
      eq "bad 1.1 hole" Failed (status "1.1")
      eq "bad 1.2 partial" (Partial 1 2) (status "1.2")
      eq "bad 1.3 wrong step" (Partial 1 3) (status "1.3")
      eq "bad 1.4 parens" Failed (status "1.4")
      eq "bad 1.5 needs delta" (Partial 1 2) (status "1.5")
      eq "bad 1.6 typo" Failed (status "1.6")
      eq "bad 1.7 omega" Failed (status "1.7")
  missing <- checkFile "test/data/does-not-exist.lam"
  check "missing file" (either (const True) (const False) missing)
