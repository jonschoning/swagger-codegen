module Instances where

import Data.Text (Text, pack)
import Data.Char (isSpace)
import Data.List (sort)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Test.QuickCheck
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

import ApproxEq
import SwaggerPetstore.Model

instance Arbitrary ApiResponse where
  arbitrary = ApiResponse <$> arbitrary <*> arbitrary <*> arbitrary 

instance Arbitrary Category where
  arbitrary = Category <$> arbitrary <*> arbitrary 

instance Arbitrary Order where
  arbitrary = Order <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 

instance Arbitrary Pet where
  arbitrary = Pet <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 

instance Arbitrary Tag where
  arbitrary = Tag <$> arbitrary <*> arbitrary 

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (ModifiedJulianDay <$>) . shrink . toModifiedJulianDay

instance Arbitrary UTCTime where
  arbitrary =
    UTCTime <$> arbitrary <*> (secondsToDiffTime <$> choose (0, 86401))

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq Day where
  (=~) = (==)
