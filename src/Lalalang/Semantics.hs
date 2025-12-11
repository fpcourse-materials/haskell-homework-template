module Lalalang.Semantics where

import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.List qualified as List
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
  | LetL LaName LaExpr | LetR LaName LaValue
  | NewArgs LaCtorName [LaValue] [LaExpr] | New LaCtorName [LaValue]
  | Match [LaBranch]
  | HandleIn Int LaHandler
  | PerformArgs LaOpName [LaValue] [LaExpr] | Perform LaOpName [LaValue]
  deriving stock Eq

instance Show Frame where
  show = \case
    BinOpL op r -> "[] " <> show op <> " " <> show r
    BinOpR op l -> show l <> " " <> show op <> " []"
    AppL r -> "[] " <> show r
    AppR l -> show l <> " []"
    SetEnv _ -> "setenv"
    LetL name body -> "let " <> name <> " = [] in " <> show body
    LetR name value -> "let " <> name <> " = " <> show value <> " in []"
    NewArgs name computed pending -> name <> "(" <> showArgs computed pending <> ")"
    New name args -> name <> "(" <> List.intercalate ", " (map show args) <> ")"
    Match _ -> "match"
    HandleIn hid _ -> "handle " <> show hid
    PerformArgs name computed pending -> 
      "perform " <> name <> "(" <> showArgs computed pending <> ")"
    Perform name args ->
      "perform " <> name <> "(" <> List.intercalate ", " (map show args) <> ")"
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
  LaLetIn name expr body -> evalLaExpr expr (LetL name body : k)
  LaNew name [] -> (New name [] : k) `appK` LaUnit
  LaNew name (arg : args) -> evalLaExpr arg (NewArgs name [] args : k)
  LaMatch scrutenee branches -> evalLaExpr scrutenee (Match branches : k)
  LaHandleIn name handler body ->
    let rec =
          let ?freshHid = ?freshHid + 1 in
          let ?env = Map.insert name (LaHandlerId ?freshHid) ?env in
          evalLaExpr body in
    rec (HandleIn ?freshHid handler : k)
  LaPerform name [] -> (Perform name [] : k) `appK` LaUnit
  LaPerform name (arg : args) -> evalLaExpr arg (PerformArgs name [] args : k)

appK :: (HasCallStack, RuntimeState) => K -> LaValue -> LaValue
appK k result = case k of
  [] -> result
  BinOpL op r : k' -> evalLaExpr r (BinOpR op result : k')
  BinOpR op l' : k' -> k' `appK` evalLaBinOp op l' result

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
