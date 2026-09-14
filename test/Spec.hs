import Data.Maybe (fromMaybe)
import SpecLevel1 qualified
import SpecLevel2 qualified
import SpecLevel3 qualified
import System.Environment (lookupEnv)
import Test.Lambda (lambdaTests)
import Test.Prelude

-- | Задачи λ-домашки (если есть src/hw.lam или файл из @LAMBDA_FILE@,
-- так `make check FILE=…` проверяет учебный файл) и задачи трёх уровней на Haskell.
main :: IO ()
main = do
  file <- fromMaybe "src/hw.lam" <$> lookupEnv "LAMBDA_FILE"
  lambda <- lambdaTests [file]
  testMain $ lambda ++ SpecLevel1.tests ++ SpecLevel2.tests ++ SpecLevel3.tests
