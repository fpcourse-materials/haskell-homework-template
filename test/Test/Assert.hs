module Test.Assert where

import Control.Exception
import Test.HUnit (assertFailure)

assertThrows :: forall exception a . Exception exception => a -> IO ()
assertThrows comp = try (evaluate comp) >>= \case
  Left e -> case fromException @exception e of
    Just _ -> pure ()
    Nothing -> throw e
  Right _ -> assertFailure "Expected exception, got result"
