

module EFA.Graph.Topology.Node where

import Prelude hiding (String, Int, Show)
import qualified Prelude as P

class P.Show nty => Show nty where
      show :: nty -> P.String
      show = P.show

newtype Node = Node P.Int deriving (P.Show, Eq, Ord)

instance Enum Node where
         toEnum = Node
         fromEnum (Node n) = n

instance Show Node where
         show (Node x) = P.show x

newtype Int = Int P.Int deriving (P.Show, Eq, Ord)

instance Enum Int where
         toEnum = Int
         fromEnum (Int n) = n

instance Show Int

newtype String = String P.String deriving (P.Show, Eq, Ord)
instance Show String where
         show (String str) = str
