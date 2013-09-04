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
