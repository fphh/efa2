module EFA.Equation.Variable where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Symbolic.OperatorTree as OT
import qualified EFA.Symbolic.SumProduct as SP
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format, FormatRecord)
import EFA.Report.FormatValue (FormatValue, formatValue)


data Index rec a =
     Energy (Idx.Energy rec a)
   | MaxEnergy (Idx.MaxEnergy rec a)
   | Power (Idx.Power rec a)
   | Eta (Idx.Eta rec a)
   | DTime (Idx.DTime rec a)
   | X (Idx.X rec a)
   | Y (Idx.Y rec a)
   | Sum (Idx.Sum rec a)
   | Store (Idx.Storage rec a)
     deriving (Show, Eq, Ord)


class MkIdxC t where
   mkIdx :: t rec a -> Index rec a
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

mkVar :: (MkIdxC t, MkVarC term) => t rec a -> term (Index rec a)
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

instance
   (Node.C node, FormatRecord rec) =>
      FormatValue (Index rec node) where
   formatValue =
      formatGen
         (\e r x y ->
            Format.record r $
            Format.subscript (Format.edgeIdent e) (formatEdgeIndex x y))


formatShort ::
   (Node.C node, FormatRecord record, Format output) =>
   Index record node -> output
formatShort =
   formatGen
      (\e r _x _y -> Format.record r $ Format.edgeIdent e)



formatGen ::
   (Format output, FormatRecord record, Node.C node) =>
   (Format.EdgeVar ->
    record -> Idx.SecNode node -> Idx.SecNode node -> output) ->
   Index record node ->
   output
formatGen fmt idx =
   case idx of
      Energy (Idx.Energy r x y) -> fmt Format.Energy r x y
      MaxEnergy (Idx.MaxEnergy r x y) -> fmt Format.MaxEnergy r x y
      Power (Idx.Power r x y) -> fmt Format.Power r x y
      Eta (Idx.Eta r x y) -> fmt Format.Eta r x y
      X (Idx.X r x y) -> fmt Format.X r x y
      Y (Idx.Y r x y) -> fmt Format.Y r x y
      DTime (Idx.DTime r s) ->
         Format.record r $
         Format.subscript (Format.delta Format.time) $
         Format.section s

      Sum (Idx.Sum r dir x) ->
         Format.record r $
         Format.subscript Format.var $
         Format.direction dir `Format.connect` formatSectionNode x

      Store (Idx.Storage r x) ->
         Format.record r $
         Format.subscript Format.storage $
         formatSectionNode x
