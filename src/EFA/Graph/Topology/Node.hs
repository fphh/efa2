module EFA.Graph.Topology.Node where

import qualified EFA.Graph.Topology.Node.Int as NodeInt

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)

import Data.Word (Word)

import qualified Test.QuickCheck as QC

import Prelude hiding (String, Int)
import qualified Prelude as P


{- |
The ordering imposed by the Ord instance
influences how GraphViz draws a graph.
Nodes with low numbers will by placed at the top of the picture..
-}
class Ord node => C node where
   display :: Format output => node -> output
   subscript :: Format output => node -> output
   dotId :: node -> P.String
   typ :: node -> Type

displayDefault :: (Show node, Format output) => node -> output
displayDefault = Format.literal . show

subscriptDefault :: (Show node, Format output) => node -> output
subscriptDefault = Format.literal . show

dotIdDefault :: (Enum node) => node -> P.String
dotIdDefault = show . fromEnum


data
   Type =
        Storage
      | Sink
      | AlwaysSink
      | Source
      | AlwaysSource
      | Crossing
      | DeadNode
      | NoRestriction
      deriving (Show, Eq, Ord)

formatType :: Format output => Type -> output
formatType t =
   case t of
      Storage       -> Format.nodeStorage
      Sink          -> Format.nodeSink
      AlwaysSink    -> Format.nodeAlwaysSink
      Source        -> Format.nodeSource
      AlwaysSource  -> Format.nodeAlwaysSource
      Crossing      -> Format.nodeCrossing
      DeadNode      -> Format.nodeDeadNode
      NoRestriction -> Format.nodeNoRestriction


isSink, isSource :: Type -> Bool
isSink t =
   case t of
      AlwaysSink -> True
      Sink -> True
      _ -> False

isSource t =
   case t of
      AlwaysSource -> True
      Source -> True
      _ -> False


instance QC.Arbitrary Type where
   arbitrary =
      QC.elements $
         Sink :
         AlwaysSink :
         Source :
         AlwaysSource :
         Crossing :
         DeadNode :
         NoRestriction :
         []

   shrink NoRestriction = []
   shrink AlwaysSink = [Sink]
   shrink AlwaysSource = [Source]
   shrink _ = [NoRestriction]


storage :: Type
storage = Storage


data
   Int =
        IntStorage       NodeInt.Storage
      | IntSink          NodeInt.Sink
      | IntAlwaysSink    NodeInt.AlwaysSink
      | IntSource        NodeInt.Source
      | IntAlwaysSource  NodeInt.AlwaysSource
      | IntCrossing      NodeInt.Crossing
      | IntDeadNode      NodeInt.DeadNode
      | IntNoRestriction NodeInt.NoRestriction
   deriving (Show, Eq, Ord)

instance C Int where
   display = intDisplay
   subscript = intDisplay
   dotId node =
      case node of
         IntStorage       (NodeInt.Storage       n) -> "st"  ++ show n
         IntSink          (NodeInt.Sink          n) -> "si"  ++ show n
         IntAlwaysSink    (NodeInt.AlwaysSink    n) -> "asi" ++ show n
         IntSource        (NodeInt.Source        n) -> "so"  ++ show n
         IntAlwaysSource  (NodeInt.AlwaysSource  n) -> "aso" ++ show n
         IntCrossing      (NodeInt.Crossing      n) -> "cr"  ++ show n
         IntDeadNode      (NodeInt.DeadNode      n) -> "dn"  ++ show n
         IntNoRestriction (NodeInt.NoRestriction n) -> "nr"  ++ show n
   typ node =
      case node of
         IntStorage       _ -> Storage
         IntSink          _ -> Sink
         IntAlwaysSink    _ -> AlwaysSink
         IntSource        _ -> Source
         IntAlwaysSource  _ -> AlwaysSource
         IntCrossing      _ -> Crossing
         IntDeadNode      _ -> DeadNode
         IntNoRestriction _ -> NoRestriction

intDisplay :: Format output => Int -> output
intDisplay node =
   let fmt :: Format output => output -> Word -> output
       fmt t n = Format.nodeInt t (Format.integer $ fromIntegral n)
   in  case node of
          IntStorage       (NodeInt.Storage       n) -> fmt Format.nodeStorage       n
          IntSink          (NodeInt.Sink          n) -> fmt Format.nodeSink          n
          IntAlwaysSink    (NodeInt.AlwaysSink    n) -> fmt Format.nodeAlwaysSink    n
          IntSource        (NodeInt.Source        n) -> fmt Format.nodeSource        n
          IntAlwaysSource  (NodeInt.AlwaysSource  n) -> fmt Format.nodeAlwaysSource  n
          IntCrossing      (NodeInt.Crossing      n) -> fmt Format.nodeCrossing      n
          IntDeadNode      (NodeInt.DeadNode      n) -> fmt Format.nodeDeadNode      n
          IntNoRestriction (NodeInt.NoRestriction n) -> fmt Format.nodeNoRestriction n

class ConsInt node where consInt :: node -> Int

instance ConsInt NodeInt.Storage       where consInt = IntStorage
instance ConsInt NodeInt.Sink          where consInt = IntSink
instance ConsInt NodeInt.AlwaysSink    where consInt = IntAlwaysSink
instance ConsInt NodeInt.Source        where consInt = IntSource
instance ConsInt NodeInt.AlwaysSource  where consInt = IntAlwaysSource
instance ConsInt NodeInt.Crossing      where consInt = IntCrossing
instance ConsInt NodeInt.DeadNode      where consInt = IntDeadNode
instance ConsInt NodeInt.NoRestriction where consInt = IntNoRestriction

intStorage, intSink, intAlwaysSink, intSource, intAlwaysSource,
   intCrossing, intDeadNode, intNoRestriction :: Word -> Int
intStorage       n = IntStorage       (NodeInt.Storage n)
intSink          n = IntSink          (NodeInt.Sink n)
intAlwaysSink    n = IntAlwaysSink    (NodeInt.AlwaysSink n)
intSource        n = IntSource        (NodeInt.Source n)
intAlwaysSource  n = IntAlwaysSource  (NodeInt.AlwaysSource n)
intCrossing      n = IntCrossing      (NodeInt.Crossing n)
intDeadNode      n = IntDeadNode      (NodeInt.DeadNode n)
intNoRestriction n = IntNoRestriction (NodeInt.NoRestriction n)

instance QC.Arbitrary Int where
   arbitrary =
      QC.oneof $
         fmap IntSink QC.arbitrary :
         fmap IntAlwaysSink QC.arbitrary :
         fmap IntSource QC.arbitrary :
         fmap IntAlwaysSource QC.arbitrary :
         fmap IntCrossing QC.arbitrary :
         fmap IntDeadNode QC.arbitrary :
         fmap IntNoRestriction QC.arbitrary :
         []

   shrink node =
      case node of
         IntNoRestriction _ -> []
         IntAlwaysSink    n -> [IntSink   $ toEnum $ fromEnum n]
         IntAlwaysSource  n -> [IntSource $ toEnum $ fromEnum n]
         IntStorage       n -> [IntNoRestriction $ toEnum $ fromEnum n]
         IntSink          n -> [IntNoRestriction $ toEnum $ fromEnum n]
         IntSource        n -> [IntNoRestriction $ toEnum $ fromEnum n]
         IntCrossing      n -> [IntNoRestriction $ toEnum $ fromEnum n]
         IntDeadNode      n -> [IntNoRestriction $ toEnum $ fromEnum n]
      ++
      case node of
         IntStorage       n -> map IntStorage       $ QC.shrink n
         IntSink          n -> map IntSink          $ QC.shrink n
         IntAlwaysSink    n -> map IntAlwaysSink    $ QC.shrink n
         IntSource        n -> map IntSource        $ QC.shrink n
         IntAlwaysSource  n -> map IntAlwaysSource  $ QC.shrink n
         IntCrossing      n -> map IntCrossing      $ QC.shrink n
         IntDeadNode      n -> map IntDeadNode      $ QC.shrink n
         IntNoRestriction n -> map IntNoRestriction $ QC.shrink n


data String = String (Type) P.String deriving (Show, Eq, Ord)

instance C String where
   display (String t str) =
      Format.nodeString (formatType t) (Format.literal str)
   subscript (String t str) =
      Format.nodeString (formatType t) (Format.literal str)
   dotId (String t str) = show t ++ "-" ++ str
   typ (String t _str) = t
