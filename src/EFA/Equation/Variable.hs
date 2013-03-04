{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.Variable where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue (FormatValue, formatValue)


data Signal a =
     Energy (Idx.Energy a)
   | Power (Idx.Power a)
   | Eta (Idx.Eta a)
   | DTime (Idx.DTime a)
   | X (Idx.X a)
   | Sum (Idx.Sum a)
     deriving (Show, Eq, Ord)

data Scalar a =
     MaxEnergy (Idx.MaxEnergy a)
   | Storage (Idx.Storage a)
     deriving (Show, Eq, Ord)



class Index t where
   type Type t :: * -> *
   index :: t a -> Type t a

instance Index Idx.Energy where type Type Idx.Energy = Signal; index = Energy
instance Index Idx.Power  where type Type Idx.Power  = Signal; index = Power
instance Index Idx.Eta    where type Type Idx.Eta    = Signal; index = Eta
instance Index Idx.DTime  where type Type Idx.DTime  = Signal; index = DTime
instance Index Idx.X      where type Type Idx.X      = Signal; index = X
instance Index Idx.Sum    where type Type Idx.Sum    = Signal; index = Sum

instance Index Idx.MaxEnergy where type Type Idx.MaxEnergy = Scalar; index = MaxEnergy
instance Index Idx.Storage   where type Type Idx.Storage   = Scalar; index = Storage



formatSectionNode ::
   (Format output, Node.C node) =>
   Idx.SecNode node -> output
formatSectionNode (Idx.SecNode s n) =
   Format.section s `Format.sectionNode` Node.subscript n

formatEdgeIndex ::
   (Format output, Node.C node) =>
   Idx.SecNode node -> Idx.SecNode node -> output
formatEdgeIndex x y =
   formatSectionNode x
   `Format.connect`
   formatSectionNode y

formatEdge ::
   (Format output, Node.C node) =>
   Format.EdgeVar -> Idx.SecNode node -> Idx.SecNode node -> output
formatEdge e x y =
   Format.subscript (Format.edgeIdent e) (formatEdgeIndex x y)


instance (Node.C node) => FormatValue (Signal node) where
   formatValue var =
      case var of
         Energy idx -> formatIndex idx
         Power idx -> formatIndex idx
         Eta idx -> formatIndex idx
         X idx -> formatIndex idx
         DTime idx -> formatIndex idx
         Sum idx -> formatIndex idx

instance (Node.C node) => FormatValue (Scalar node) where
   formatValue var =
      case var of
         MaxEnergy idx -> formatIndex idx
         Storage idx -> formatIndex idx


class FormatIndex idx where
   formatIndex :: (Format output) => idx -> output

instance (Node.C node) => FormatIndex (Idx.Energy node) where
   formatIndex (Idx.Energy x y) = formatEdge Format.Energy x y

instance (Node.C node) => FormatIndex (Idx.MaxEnergy node) where
   formatIndex (Idx.MaxEnergy x y) = formatEdge Format.MaxEnergy x y

instance (Node.C node) => FormatIndex (Idx.Power node) where
   formatIndex (Idx.Power x y) = formatEdge Format.Power x y

instance (Node.C node) => FormatIndex (Idx.Eta node) where
   formatIndex (Idx.Eta x y) = formatEdge Format.Eta x y

instance (Node.C node) => FormatIndex (Idx.X node) where
   formatIndex (Idx.X x y) = formatEdge Format.X x y

instance (Node.C node) => FormatIndex (Idx.DTime node) where
   formatIndex (Idx.DTime s) =
      Format.subscript Format.dtime $ Format.section s

instance (Node.C node) => FormatIndex (Idx.Sum node) where
   formatIndex (Idx.Sum dir x) =
      Format.subscript Format.var $
      Format.direction dir `Format.connect` formatSectionNode x

instance (Node.C node) => FormatIndex (Idx.Storage node) where
   formatIndex (Idx.Storage x) =
      Format.subscript Format.storage $
      formatSectionNode x
