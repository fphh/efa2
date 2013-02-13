

module EFA.Graph.Topology.Node where

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)

import Prelude hiding (String, Int)
import qualified Prelude as P

class Ord node => C node where
   display :: Format output => node -> output
   subscript :: Format output => node -> output
   dotId :: node -> P.String

displayDefault :: (Show node, Format output) => node -> output
displayDefault = Format.literal . show

subscriptDefault :: (Show node, Format output) => node -> output
subscriptDefault = Format.literal . show

dotIdDefault :: (Enum node) => node -> P.String
dotIdDefault = show . fromEnum


newtype Int = Int P.Int deriving (Show, Eq, Ord)

instance Enum Int where
   toEnum = Int
   fromEnum (Int n) = n

instance C Int where
   display (Int n) = Format.integer $ fromIntegral n
   subscript (Int n) = Format.integer $ fromIntegral n
   dotId (Int n) = show n


newtype String = String P.String deriving (Show, Eq, Ord)

instance C String where
   display (String str) = Format.literal str
   subscript (String str) = Format.literal str
   dotId (String str) = str
