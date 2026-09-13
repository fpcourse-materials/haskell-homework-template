import SpecBlock1 qualified
import SpecBlock2 qualified
import SpecBlock3 qualified
import Test.Lambda (lambdaTests)
import Test.Prelude

-- | Задачи λ-домашки (если есть src/hw.lam) и задачи блоков на Haskell.
main :: IO ()
main = do
  lambda <- lambdaTests ["src/hw.lam"]
  testMain $ lambda ++ SpecBlock1.tests ++ SpecBlock2.tests ++ SpecBlock3.tests
