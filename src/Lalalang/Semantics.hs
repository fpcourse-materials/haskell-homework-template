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

-- | Значения в Lalalang.
data LaValue
  = LaUnit
  | LaNum Int
  | LaObj LaCtorName [LaValue]
  | LaClosure LaEnv LaName LaExpr
  | LaHandlerId Int
  deriving stock Eq

instance Show LaValue where
  show = \case
    LaUnit -> "unit"
    LaNum value -> show value
    LaObj name values -> 
      "(" <> name <> " " <> unwords (show <$> values) <> ")"
    LaClosure env name body ->
      "(" <> show (LaLam name body) <> " | " <> show env <> ")"
    LaHandlerId hid -> "h" <> show hid

unpackNum :: HasCallStack => LaValue -> Int
unpackNum = \case
  LaNum value -> value
  other -> error $ "TYPE_ERROR: Expected number, got " <> show other

unpackClosure :: HasCallStack => LaValue -> (LaEnv, LaName, LaExpr)
unpackClosure = \case
  LaClosure env name body -> (env, name, body)
  other -> error $ "TYPE_ERROR: Expected function, got " <> show other

unpackHid :: HasCallStack => LaValue -> Int
unpackHid = \case
  LaHandlerId hid -> hid
  other -> error $ "TYPE_ERROR: Expected handler id, got " <> show other


-- | Локальное состояние.
type LaEnv = Map LaName LaValue


data Frame
  = BinOpL LaBinOp LaExpr | BinOpR LaBinOp LaValue
  | AppL LaExpr | AppR LaValue
  | SetEnv LaEnv
  | LetIn LaName LaExpr
  | NewArgs LaCtorName [LaValue] [LaExpr] | New LaCtorName [LaValue]
  | Match [LaBranch]
  | HandleIn Int LaHandler | Return LaValue
  | PerformArgs LaOpName [LaValue] [LaExpr] LaName | Perform LaOpName [LaValue] LaName
  deriving stock Eq

instance Show Frame where
  show = \case
    BinOpL op r -> "[] " <> show op <> " " <> show r
    BinOpR op l -> show l <> " " <> show op <> " []"
    AppL r -> "[] " <> show r
    AppR l -> show l <> " []"
    SetEnv _ -> "setenv"
    LetIn name body -> "let " <> name <> " = [] in " <> show body
    NewArgs name computed pending -> name <> "(" <> showArgs computed pending <> ")"
    New name args -> name <> "(" <> List.intercalate ", " (map show args) <> ")"
    Match _ -> "match"
    HandleIn hid _ -> "handle " <> show hid
    PerformArgs opName computed pending name -> 
      "perform " <> opName <> "(" <> showArgs computed pending <> ") at " <> name
    Perform opName args name ->
      "perform " <> opName <> "(" <> List.intercalate ", " (map show args) <> ") at " <> name
    where
      showArgs computed pending =
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

evalLaExpr :: (HasCallStack, RuntimeState) => LaExpr -> K -> LaValue
evalLaExpr expr k = case expr of
  LaConst value -> k `appK` LaNum value
  LaVar name -> case ?env !? name of
    Nothing -> error $ "ERROR: Undefined variable '" <> name <> "'"
    Just value -> k `appK` value
  LaBinOp op l r -> evalLaExpr l (BinOpL op r : k)
  LaLam name body -> k `appK` LaClosure ?env name body
  LaApp f arg -> evalLaExpr f (AppL arg : k)
  LaLetIn name expr body -> evalLaExpr expr (LetIn name body : k)
  LaNew name [] -> (New name [] : k) `appK` LaUnit
  LaNew name (arg : args) -> evalLaExpr arg (NewArgs name [] args : k)
  LaMatch scrutenee branches -> evalLaExpr scrutenee (Match branches : k)
  LaHandleIn name handler body ->
    let rec =
          let ?freshHid = ?freshHid + 1 in
          let ?env = Map.insert name (LaHandlerId ?freshHid) ?env in
          evalLaExpr body in
    rec (HandleIn ?freshHid handler : k)
  LaPerform opName [] name -> (Perform opName [] name : k) `appK` LaUnit
  LaPerform opName (arg : args) name -> evalLaExpr arg (PerformArgs opName [] args name : k)

appK :: (HasCallStack, RuntimeState) => K -> LaValue -> LaValue
appK k result = case k of
  [] -> result
  BinOpL op r : k' -> evalLaExpr r (BinOpR op result : k')
  BinOpR op l' : k' -> k' `appK` evalLaBinOp op l' result
  AppL arg : k' -> evalLaExpr arg (AppR result : k')
  AppR f : k' ->
    let (env', name, body) = unpackClosure f in
    let env = ?env in
    let ?env = Map.insert name result env' in
    evalLaExpr body (SetEnv env : k')
  SetEnv env : k' -> let ?env = env in k' `appK` result
  LetIn name body : k' ->
    let ?env = Map.insert name result ?env in
    evalLaExpr body k'
  NewArgs name computed pending : k' -> case pending of
    [] -> (New name (computed ++ [result]) : k') `appK` LaUnit
    next : rest -> evalLaExpr next (NewArgs name (computed ++ [result]) rest : k')
  New name args : k' -> k' `appK` LaObj name args
  Match branches : k' -> case findMatching result branches of
    Nothing -> error $ "ERROR: Match failed for " <> show result
    Just (env', branch) ->
      let env = ?env in
      let ?env = env' in 
      evalLaExpr branch (SetEnv env : k')
  HandleIn _ (LaHandler ret _) : k' -> evalLaExpr ret (Return result : k')
  Return bodyResult : k' -> (AppR result : k') `appK` bodyResult
  PerformArgs opName computed pending name : k' -> case pending of
    [] -> (Perform opName (computed ++ [result]) name : k') `appK` LaUnit
    next : rest -> evalLaExpr next (PerformArgs opName (computed ++ [result]) rest name : k')
  Perform opName args name : k' -> case ?env !? name of
    Nothing -> error $ "ERROR: Undefined variable '" <> name <> "'"
    Just (unpackHid -> expectedHid) -> 
      let (subcont, (hid, handler), metacont) = k' `splitWith` \case
            HandleIn hid handler | hid == expectedHid -> Just (hid, handler)
            _ -> Nothing in
      let LaHandler _ ops = handler in
      let LaOp _ params body = ops
            & List.find (\(LaOp actualOpName _ _) -> opName == actualOpName)
            & fromMaybe (error $ "ERROR: Operation " <> opName <> " not found") in
      undefined

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
