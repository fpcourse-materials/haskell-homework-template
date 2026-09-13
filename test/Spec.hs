import SpecLevel1 qualified
import SpecLevel2 qualified
import SpecLevel3 qualified
import Test.Lambda (lambdaTests)
import Test.Prelude

-- | Задачи λ-домашки (если есть src/hw.lam) и задачи трёх уровней на Haskell.
main :: IO ()
main = do
  lambda <- lambdaTests ["src/hw.lam"]
  testMain $ lambda ++ SpecLevel1.tests ++ SpecLevel2.tests ++ SpecLevel3.tests
