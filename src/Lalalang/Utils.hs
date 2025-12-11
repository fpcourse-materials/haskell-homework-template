module Lalalang.Utils where

import Data.List qualified as List

inParens :: String -> String
inParens s = "(" <> s <> ")"

showParams :: [String] -> String
showParams = inParens . List.intercalate ", "

showArgs :: Show a => [a] -> String
showArgs = inParens . List.intercalate ", " . map show
