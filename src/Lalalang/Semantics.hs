module Lalalang.Semantics where

import Control.Monad
import Data.Foldable
import Data.Function
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe
import GHC.Stack
import Lalalang.Syntax
import Lalalang.Utils

-- | Значения в Lalalang.
data LaValue
  = LaUnitValue
  | LaNum Int
  | LaObj LaCtorName [LaValue]
  | LaClosure LaEnv [LaName] LaExpr
  | LaCont LaEnv K
  | LaHandlerId Int
  deriving stock Eq

instance Show LaValue where
  show = \case
    LaUnitValue -> "unit"
    LaNum value -> show value
    LaObj ctor values -> ctor <> showArgs values
    LaClosure env params body ->
      inParens $ show (LaLam params body) <> " | " <> show env
    LaCont env k ->
      inParens $ show k <> " | " <> show env
    LaHandlerId hid -> "h" <> show hid

unpackNum :: HasCallStack => LaValue -> Int
unpackNum = \case
  LaNum value -> value
  other -> error $ "TYPE_ERROR: Expected number, got " <> show other

unpackClosure :: HasCallStack => LaValue -> (LaEnv, [LaName], LaExpr)
unpackClosure = \case
  LaClosure env params body -> (env, params, body)
  other -> error $ "TYPE_ERROR: Expected function, got " <> show other

unpackHid :: HasCallStack => LaValue -> Int
unpackHid = \case
  LaHandlerId hid -> hid
  other -> error $ "TYPE_ERROR: Expected handler id, got " <> show other


-- | Локальное состояние.
type LaEnv = Map LaName LaValue

envLookup :: (?env :: LaEnv, HasCallStack) => LaName -> LaValue
envLookup name = case ?env !? name of
  Nothing -> error $ "ERROR: Undefined variable '" <> name <> "'"
  Just value -> value

updateLaEnv :: (?env :: LaEnv) => (LaEnv -> LaEnv) -> ((?env :: LaEnv) => a) -> a
updateLaEnv f comp = let ?env = f ?env in comp

setLaEnv :: LaEnv -> ((?env :: LaEnv) => a) -> a
setLaEnv env comp = let ?env = env in comp 


data Frame
  = BinOpL LaBinOp LaExpr | BinOpR LaBinOp LaValue
  | AppF [LaExpr] | AppArgs LaValue [LaValue] [LaExpr] | App LaValue [LaValue]
  | SetEnv LaEnv
  | LetIn LaName LaExpr
  | NewArgs LaCtorName [LaValue] [LaExpr] | New LaCtorName [LaValue]
  | Match [LaBranch]
  | HandleIn Int LaEnv LaHandler | Return LaValue
  | PerformArgs LaOpName [LaValue] [LaExpr] LaName | Perform LaOpName [LaValue] LaName
  deriving stock Eq

instance Show Frame where
  show = \case
    BinOpL op r -> "[] " <> show op <> " " <> show r
    BinOpR op l -> show l <> " " <> show op <> " []"
    AppF args -> "[]" <> showArgs args
    AppArgs f computed pending -> show f <> showArgsInProgress computed pending
    App f args -> show f <> showArgs args
    SetEnv _ -> "setenv"
    LetIn name body -> "let " <> name <> " = [] in " <> show body
    NewArgs ctor computed pending -> ctor <> showArgsInProgress computed pending
    New ctor args -> ctor <> showArgs args
    Match _ -> "match"
    HandleIn hid _ _ -> "handle " <> show hid
    Return _ -> "return"
    PerformArgs opName computed pending name -> 
      "perform " <> opName <> showArgsInProgress computed pending <> " to " <> name
    Perform opName args name ->
      "perform " <> opName <> showArgs args <> " to " <> name
    where
      showArgsInProgress computed pending = inParens $
        List.intercalate ", " (map show computed) <>
        (if null computed then "" else ", ") <> "[]" <> (if null pending then "" else ", ") <>
        List.intercalate ", " (map show pending)

type K = [Frame]


eval :: LaExpr -> LaValue
eval expr =
  let ?env = Map.empty in
  let ?freshHid = 0 in
  evalLaExpr expr []

evalWithInput :: LaExpr -> (Int -> Int)
evalWithInput expr input =
  let ?env = Map.singleton "input" (LaNum input) in
  let ?freshHid = 0 in
  unpackNum $ evalLaExpr expr []

type RuntimeState = (?env :: LaEnv, ?freshHid :: Int)

evalLaExpr :: (RuntimeState, HasCallStack) => LaExpr -> K -> LaValue
evalLaExpr expr k = case expr of
  LaUnit -> k `appK` LaUnitValue
  LaConst value -> k `appK` LaNum value
  LaVar name -> k `appK` envLookup name
  LaBinOp op l r -> evalLaExpr l (BinOpL op r : k)
  LaLam params body -> k `appK` LaClosure ?env params body
  LaApp f args -> evalLaExpr f (AppF args : k)
  LaLetIn name expr body -> evalLaExpr expr (LetIn name body : k)
  LaNew name args -> case args of
    [] -> (New name [] : k) `appK` LaUnitValue
    next : rest -> evalLaExpr next (NewArgs name [] rest : k)
  LaMatch scrutenee branches -> evalLaExpr scrutenee (Match branches : k)
  LaHandleIn name handler body ->
    let rec =
          updateLaEnv (Map.insert name $ LaHandlerId ?freshHid) $
          let ?freshHid = ?freshHid + 1 in
          evalLaExpr body in
    rec (HandleIn ?freshHid ?env handler : k)
  LaPerform opName args name -> case args of
    [] -> (Perform opName [] name : k) `appK` LaUnitValue
    next : rest -> evalLaExpr next (PerformArgs opName [] rest name : k)

appK :: (RuntimeState, HasCallStack) => K -> LaValue -> LaValue
appK k result = case k of
  [] -> result
  BinOpL op r : k' -> evalLaExpr r (BinOpR op result : k')
  BinOpR op l' : k' -> k' `appK` evalLaBinOp op l' result
  AppF [] : k' -> (App result [] : k') `appK` LaUnitValue
  AppF (arg : args) : k' -> evalLaExpr arg (AppArgs result [] args : k')
  AppArgs f computed pending : k' -> case pending of
    [] -> (App f (computed ++ [result]) : k') `appK` LaUnitValue
    next : rest -> evalLaExpr next (AppArgs f (computed ++ [result]) rest : k')
  App f args : k' ->
    let (env', params, body) = unpackClosure f in
    let envDiff = fold $ zipWith Map.singleton params args in
    setLaEnv (env' <> envDiff) evalLaExpr body (SetEnv ?env : k')
  LetIn name body : k' -> updateLaEnv (Map.insert name result) evalLaExpr body k'
  NewArgs ctor computed pending : k' -> case pending of
    [] -> (New ctor (computed ++ [result]) : k') `appK` LaUnitValue
    next : rest -> evalLaExpr next (NewArgs ctor (computed ++ [result]) rest : k')
  New ctor args : k' -> k' `appK` LaObj ctor args
  Match branches : k' -> case findMatching result branches of
    Nothing -> error $ "ERROR: Match failed for " <> show result
    Just (env', body) -> updateLaEnv (<> env') evalLaExpr body (SetEnv ?env : k')




--   -- HandleIn _ (LaHandler ret _) : k' -> evalLaExpr ret (Return result : k')
--   -- Return bodyResult : k' -> (AppR result : k') `appK` bodyResult
--   -- PerformArgs opName computed pending name : k' -> case pending of
--   --   [] -> (Perform opName (computed ++ [result]) name : k') `appK` LaUnit
--   --   next : rest -> evalLaExpr next (PerformArgs opName (computed ++ [result]) rest name : k')
--   -- Perform opName args name : k' -> case ?env !? name of
--   --   Nothing -> error $ "ERROR: Undefined variable '" <> name <> "'"
--   --   Just (unpackHid -> expectedHid) -> 
--   --     let (subcont, (hid, handler), metacont) = k' `splitWith` \case
--   --           HandleIn hid handler | hid == expectedHid -> Just (hid, handler)
--   --           _ -> Nothing in
--   --     let LaHandler _ ops = handler in
--   --     let LaOp _ params body = ops
--   --           & List.find (\(LaOp actualOpName _ _) -> opName == actualOpName)
--   --           & fromMaybe (error $ "ERROR: Operation " <> opName <> " not found") in
--   --     undefined

findMatching :: LaValue -> [LaBranch] -> Maybe (LaEnv, LaExpr)
findMatching value = asum . map \(LaBranch pat branch) ->
  (, branch) <$> value `matches` pat

-- Возвращает подстановку значений в паттерны-переменные в случае успеха.
matches :: LaValue -> LaPattern -> Maybe LaEnv
matches = curry \case
  (LaNum actual, LaConstPat expected) | actual == expected -> Just Map.empty
  (value, LaVarPat name) -> Just $ Map.singleton name value
  (LaObj actualCtor values, LaCtorPat expectedCtor pats)
    | actualCtor == expectedCtor, length values == length pats ->
      fold <$> zipWithM matches values pats
  _ -> Nothing

splitWith :: K -> (Frame -> Maybe a) -> (K, a, K)
splitWith k f = go [] k
  where
    go _ [] = error "Unable to find required frame in continuation"
    go subcont (frame : metacont) = case f frame of
      Nothing -> go (subcont ++ [frame]) metacont
      Just x -> (subcont, x, metacont)

-- | Интерпретатор бинарных операций.
evalLaBinOp :: LaBinOp -> LaValue -> LaValue -> LaValue
evalLaBinOp op = case op of
  LaPlus -> wrapNum (+)
  LaMinus -> wrapNum (-)
  LaMult -> wrapNum (*)
  LaDiv -> wrapNum div
  LaLt -> wrapBool (<)
  LaLe -> wrapBool (<=)
  LaEq -> wrapBool (==)
  where
    wrapNum op lhs rhs = LaNum $ unpackNum lhs `op` unpackNum rhs
    wrapBool op lhs rhs = if unpackNum lhs `op` unpackNum rhs then LaNum 1 else LaNum 0
