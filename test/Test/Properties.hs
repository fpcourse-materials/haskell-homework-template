{-# LANGUAGE UndecidableInstances #-}

module Test.Properties (propertyToTest, defaultArgs, lawsToTest, filterLaws) where

import Control.Exception
import Data.List.NonEmpty qualified as NE
import Test.HUnit (Test (..), assertFailure)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Classes.Base qualified as QC
import MetaUtils

class PropertyToTest args where
  propertyToTest :: String -> args -> Test

instance {-# OVERLAPPING #-} QC.Testable prop => PropertyToTest (QC.Args, IO prop) where
  propertyToTest description (args, property) = TestCase $
    property >>= QC.quickCheckWithResult args >>= \case
      QC.Failure { theException = Just exception } ->
        case fromException @TodoException exception of
          Just _ -> throwIO exception
          Nothing -> assertFailure $ "tested " <> description <> ", but\n" <> show exception
      QC.Failure { theException = Nothing, .. } ->
        assertFailure $ "tested " <> description <> ", but\n" <> output
      _ -> pure ()

instance {-# OVERLAPPING #-} QC.Testable prop => PropertyToTest (IO prop) where
  propertyToTest description property =
    propertyToTest description (defaultArgs, property)

instance {-# OVERLAPPING #-} QC.Testable prop => PropertyToTest prop where
  propertyToTest description property =
    propertyToTest description (pure @IO property)

defaultArgs :: QC.Args
defaultArgs = QC.stdArgs { QC.chatty = False, QC.maxSuccess = 5000 }

lawsToTest :: QC.Laws -> Test
lawsToTest QC.Laws {..} = TestList $ uncurry mkTest <$> lawsProperties
  where
    mkTest name = propertyToTest (lawsTypeclass <> "." <> name <> " satisfied")

filterLaws :: (String -> Bool) -> QC.Laws -> QC.Laws
filterLaws p QC.Laws {..} = QC.Laws { lawsProperties = filter (p . fst) lawsProperties, .. }

deriving newtype instance Arbitrary a => Arbitrary (Id a)
deriving newtype instance Arbitrary (f a) => Arbitrary (Done f a)
deriving newtype instance Arbitrary (f a) => Arbitrary (Todo f a)
deriving newtype instance Arbitrary (f a) => Arbitrary (Dispatcher l f a)

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
  arbitrary = NE.fromList . QC.getNonEmpty <$> arbitrary
