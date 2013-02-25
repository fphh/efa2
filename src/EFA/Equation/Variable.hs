module EFA.Equation.Variable where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Symbolic.OperatorTree as OT
import qualified EFA.Symbolic.SumProduct as SP
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue (FormatValue, formatValue)


data Index a =
     Energy (Idx.Energy a)
   | MaxEnergy (Idx.MaxEnergy a)
   | Power (Idx.Power a)
   | Eta (Idx.Eta a)
   | DTime (Idx.DTime a)
   | X (Idx.X a)
   | Y (Idx.Y a)
   | Sum (Idx.Sum a)
   | Store (Idx.Storage a)
     deriving (Show, Eq, Ord)


class MkIdxC t where
   mkIdx :: t a -> Index a
   --mkIdx :: a n -> Index n


instance MkIdxC Idx.Energy where mkIdx = Energy
instance MkIdxC Idx.MaxEnergy where mkIdx = MaxEnergy
instance MkIdxC Idx.Power where mkIdx = Power
instance MkIdxC Idx.Eta where mkIdx = Eta
instance MkIdxC Idx.DTime where mkIdx = DTime
instance MkIdxC Idx.X where mkIdx = X
instance MkIdxC Idx.Y where mkIdx = Y
instance MkIdxC Idx.Sum where mkIdx = Sum
instance MkIdxC Idx.Storage where mkIdx = Store


class MkVarC term where
      mkVarCore :: a -> term a

instance MkVarC OT.Term where
         mkVarCore = OT.Atom

instance MkVarC SP.Term where
         mkVarCore = SP.Atom

mkVar :: (MkIdxC t, MkVarC term) => t a -> term (Index a)
mkVar = mkVarCore . mkIdx


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

instance (Node.C node) => FormatValue (Index node) where
   formatValue =
      formatGen
         (\e x y ->
            Format.subscript (Format.edgeIdent e) (formatEdgeIndex x y))


formatShort ::
   (Node.C node, Format output) =>
   Index node -> output
formatShort =
   formatGen
      (\e _x _y -> Format.edgeIdent e)



formatGen ::
   (Format output, Node.C node) =>
   (Format.EdgeVar ->
    Idx.SecNode node -> Idx.SecNode node -> output) ->
   Index node ->
   output
formatGen fmt idx =
   case idx of
      Energy (Idx.Energy x y) -> fmt Format.Energy x y
      MaxEnergy (Idx.MaxEnergy x y) -> fmt Format.MaxEnergy x y
      Power (Idx.Power x y) -> fmt Format.Power x y
      Eta (Idx.Eta x y) -> fmt Format.Eta x y
      X (Idx.X x y) -> fmt Format.X x y
      Y (Idx.Y x y) -> fmt Format.Y x y
      DTime (Idx.DTime s) ->
         Format.subscript (Format.delta Format.time) $
         Format.section s

      Sum (Idx.Sum dir x) ->
         Format.subscript Format.var $
         Format.direction dir `Format.connect` formatSectionNode x

      Store (Idx.Storage x) ->
         Format.subscript Format.storage $
         formatSectionNode x
