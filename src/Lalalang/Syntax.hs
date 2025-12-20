-- | Абстрактный синтаксис Lalalang.
module Lalalang.Syntax where

import Data.List qualified as List
import MetaUtils

-- | Перечень бинарных операций Lalalang.
data LaBinOp = LaPlus | LaMinus | LaMult | LaDiv | LaLt | LaLe | LaEq
  deriving stock Eq

instance Show LaBinOp where
  show = \case
    LaPlus -> "+"; LaMinus -> "-";
    LaMult -> "*"; LaDiv -> "/";
    LaLt -> "<"; LaLe -> "<="; LaEq -> "=="

-- | Имена переменных в Lalalang AST.
type LaName = String

-- | Имена effectful операций в Lalalang AST. 
type LaOpName = String

-- | Имя конструктора.
type LaCtorName = String

-- | Деревья выражений Lalalang.
data LaExpr
  = LaConst Int
  | LaVar LaName
  | LaBinOp LaBinOp LaExpr LaExpr
  | LaLam LaName LaExpr
  | LaApp LaExpr LaExpr
  | LaLetIn LaName LaExpr LaExpr
  | LaNew LaCtorName [LaExpr]
  | LaMatch LaExpr [LaBranch]
  | LaHandleIn LaName LaHandler LaExpr
  | LaPerform LaOpName [LaExpr]
  deriving stock Eq

instance Show LaExpr where
  show = \case
    LaConst value -> show value
    LaVar name -> name
    LaBinOp op l r -> parens $ show l <> " " <> show op <> " " <> show r
    LaLam name body -> parens $ "\\" <> name <> " -> " <> show body
    LaApp l r -> parens $ show l <> " " <> show r
    LaLetIn name expr body -> if name == "_" 
      then show expr <> "\n" <> show body
      else "let " <> name <> " = " <> show expr <> "\n" <> show body
    LaNew name args -> name <> "(" <> List.intercalate ", " (map show args) <> ")"
    LaMatch scrutenee branches -> 
      "match " <> show scrutenee <> 
      "{\n" <> List.intercalate "\n" (map show branches) <> "\n}"
    LaHandleIn name handler body -> 
      "handle " <> name <> " " <> show handler <> "\n" <> show body
    LaPerform name args -> 
      "perform " <> name <> "(" <> List.intercalate ", " (map show args) <> ")"
    where
      parens s = "(" <> s <> ")"

-- | Ветка паттерн-матчинга.
data LaBranch = LaBranch LaPattern LaExpr
  deriving stock Eq

instance Show LaBranch where
  show (LaBranch pat body) = "case " <> show pat <> " -> " <> show body

-- | Паттерны.
data LaPattern
  = LaConstPat Int
  | LaVarPat LaName
  | LaCtorPat LaCtorName [LaPattern]
  deriving stock Eq

instance Show LaPattern where
  show = \case
    LaConstPat n -> show n
    LaVarPat name -> name
    LaCtorPat name pats -> name <> "(" <> List.intercalate ", " (map show pats) <> ")"

-- | Хендлер: return ветка и определения операций.
data LaHandler = LaHandler LaExpr [LaOp]
  deriving stock Eq

instance Show LaHandler where
  show (LaHandler ret ops) = 
    "{\n" <> 
    "return = " <> show ret <> "\n" <> 
    List.intercalate "\n" (map show ops) <> 
    "}\n"

-- | Определение операций.
data LaOp = LaOp LaOpName [LaName] LaExpr
  deriving stock Eq

instance Show LaOp where
  show (LaOp name params body) = 
    "op " <> name <> "(" <> List.intercalate ", " params <> ")" <> " " <> show body
