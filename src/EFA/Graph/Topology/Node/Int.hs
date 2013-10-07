module EFA.Graph.Topology.Node.Int (
   Storage      (..),
   Sink         (..),
   AlwaysSink   (..),
   Source       (..),
   AlwaysSource (..),
   Crossing     (..),
   DeadNode     (..),
   NoRestriction(..),
   ) where

import qualified Test.QuickCheck as QC

import Data.Word (Word)


newtype Storage       = Storage       Word deriving (Show, Eq, Ord)
newtype Sink          = Sink          Word deriving (Show, Eq, Ord)
newtype AlwaysSink    = AlwaysSink    Word deriving (Show, Eq, Ord)
newtype Source        = Source        Word deriving (Show, Eq, Ord)
newtype AlwaysSource  = AlwaysSource  Word deriving (Show, Eq, Ord)
newtype Crossing      = Crossing      Word deriving (Show, Eq, Ord)
newtype DeadNode      = DeadNode      Word deriving (Show, Eq, Ord)
newtype NoRestriction = NoRestriction Word deriving (Show, Eq, Ord)


instance Enum Storage where
   toEnum = Storage . toEnumGen
   fromEnum (Storage n) = fromEnumGen n

instance Enum Sink where
   toEnum = Sink . toEnumGen
   fromEnum (Sink n) = fromEnumGen n

instance Enum AlwaysSink where
   toEnum = AlwaysSink . toEnumGen
   fromEnum (AlwaysSink n) = fromEnumGen n

instance Enum Source where
   toEnum = Source . toEnumGen
   fromEnum (Source n) = fromEnumGen n

instance Enum AlwaysSource where
   toEnum = AlwaysSource . toEnumGen
   fromEnum (AlwaysSource n) = fromEnumGen n

instance Enum Crossing where
   toEnum = Crossing . toEnumGen
   fromEnum (Crossing n) = fromEnumGen n

instance Enum DeadNode where
   toEnum = DeadNode . toEnumGen
   fromEnum (DeadNode n) = fromEnumGen n

instance Enum NoRestriction where
   toEnum = NoRestriction . toEnumGen
   fromEnum (NoRestriction n) = fromEnumGen n


instance QC.Arbitrary Storage where
   arbitrary = fmap Storage QC.arbitrary
   shrink (Storage n) = map Storage $ QC.shrink n

instance QC.Arbitrary Sink where
   arbitrary = fmap Sink QC.arbitrary
   shrink (Sink n) = map Sink $ QC.shrink n

instance QC.Arbitrary AlwaysSink where
   arbitrary = fmap AlwaysSink QC.arbitrary
   shrink (AlwaysSink n) = map AlwaysSink $ QC.shrink n

instance QC.Arbitrary Source where
   arbitrary = fmap Source QC.arbitrary
   shrink (Source n) = map Source $ QC.shrink n

instance QC.Arbitrary AlwaysSource where
   arbitrary = fmap AlwaysSource QC.arbitrary
   shrink (AlwaysSource n) = map AlwaysSource $ QC.shrink n

instance QC.Arbitrary Crossing where
   arbitrary = fmap Crossing QC.arbitrary
   shrink (Crossing n) = map Crossing $ QC.shrink n

instance QC.Arbitrary DeadNode where
   arbitrary = fmap DeadNode QC.arbitrary
   shrink (DeadNode n) = map DeadNode $ QC.shrink n

instance QC.Arbitrary NoRestriction where
   arbitrary = fmap NoRestriction QC.arbitrary
   shrink (NoRestriction n) = map NoRestriction $ QC.shrink n



toEnumGen :: Int -> Word
toEnumGen n =
   if n >= 0
     then fromIntegral n
     else error "Node.Int.toEnum: negative number"

fromEnumGen :: Word -> Int
fromEnumGen n =
   if n <= fromIntegral (maxBound::Int)
     then fromIntegral n
     else error "Node.Int.fromEnum: number too big"
