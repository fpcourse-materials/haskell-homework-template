module Test.Prelude
  ( module Data.Coerce
  , module Data.Proxy
  , module Test.Assert
  , module Test.Gen
  , module Test.HUnit
  , module Test.Lazy
  , module Test.Properties
  , module Test.QuickCheck
  , module Test.QuickCheck.Arbitrary.Generic
  , module Test.QuickCheck.Classes.Base
  , module Test.Run
  , module MetaUtils
  ) where

import Data.Coerce (Coercible)
import Data.Proxy
import Test.HUnit (Test (..), assertFailure, assertEqual, assertBool)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Classes.Base
import Test.Assert
import Test.Gen
import Test.Run
import Test.Lazy
import Test.Properties
import MetaUtils
