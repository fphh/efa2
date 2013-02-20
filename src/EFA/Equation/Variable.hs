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
   | DEnergy (Idx.DEnergy rec a)
   | MaxEnergy (Idx.MaxEnergy rec a)
   | DMaxEnergy (Idx.DMaxEnergy rec a)
   | Power (Idx.Power rec a)
   | DPower (Idx.DPower rec a)
   | Eta (Idx.Eta rec a)
   | DEta (Idx.DEta rec a)
   | DTime (Idx.DTime rec a)
   | X (Idx.X rec a)
   | DX (Idx.DX rec a)
   | Y (Idx.Y rec a)
   | DY (Idx.DY rec a)
   | Sum (Idx.Sum rec a)
   | Store (Idx.Storage rec a)
     deriving (Show, Eq, Ord)


class MkIdxC t where
   mkIdx :: t rec a -> Index rec a
   --mkIdx :: a n -> Index n


instance MkIdxC Idx.Energy where mkIdx = Energy
instance MkIdxC Idx.DEnergy where mkIdx = DEnergy
instance MkIdxC Idx.MaxEnergy where mkIdx = MaxEnergy
instance MkIdxC Idx.DMaxEnergy where mkIdx = DMaxEnergy
instance MkIdxC Idx.Power where mkIdx = Power
instance MkIdxC Idx.DPower where mkIdx = DPower
instance MkIdxC Idx.Eta where mkIdx = Eta
instance MkIdxC Idx.DEta where mkIdx = DEta
instance MkIdxC Idx.DTime where mkIdx = DTime
instance MkIdxC Idx.X where mkIdx = X
instance MkIdxC Idx.DX where mkIdx = DX
instance MkIdxC Idx.Y where mkIdx = Y
instance MkIdxC Idx.DY where mkIdx = DY
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
   (Format output, FormatRecord record, Node.C node) =>
   record -> Idx.SecNode node -> Idx.SecNode node -> output
formatEdgeIndex r x y =
   Format.record r $
      formatSectionNode x
      `Format.connect`
      formatSectionNode y

instance
   (Node.C node, FormatRecord rec) =>
      FormatValue (Index rec node) where
   formatValue =
      formatGen
         (\e r x y ->
            Format.subscript (Format.edgeIdent e) (formatEdgeIndex r x y))
         (\e r x y ->
            Format.subscript
               (Format.delta $ Format.edgeIdent e)
               (formatEdgeIndex r x y))


formatShort ::
   (Node.C node, FormatRecord record, Format output) =>
   Index record node -> output
formatShort =
   formatGen
      (\e _r _x _y -> Format.edgeIdent e)
      (\e _r _x _y -> Format.delta $ Format.edgeIdent e)



formatGen ::
   (Format output, FormatRecord record, Node.C node) =>
   (Format.EdgeVar ->
    record -> Idx.SecNode node -> Idx.SecNode node -> output) ->
   (Format.EdgeVar ->
    record -> Idx.SecNode node -> Idx.SecNode node -> output) ->
   Index record node ->
   output
formatGen absolute delta idx =
   case idx of
      Energy (Idx.Energy r x y) -> absolute Format.Energy r x y
      DEnergy (Idx.DEnergy r x y) -> delta Format.Energy r x y

      MaxEnergy (Idx.MaxEnergy r x y) ->
        absolute Format.MaxEnergy r x y
      DMaxEnergy (Idx.DMaxEnergy r x y) -> delta Format.MaxEnergy r x y

      Power (Idx.Power r x y) -> absolute Format.Power r x y
      DPower (Idx.DPower r x y) -> delta Format.Power r x y

      Eta (Idx.Eta r x y) -> absolute Format.Eta r x y
      DEta (Idx.DEta r x y) -> delta Format.Eta r x y

      X (Idx.X r x y) -> absolute Format.X r x y
      DX (Idx.DX r x y) -> delta Format.X r x y

      Y (Idx.Y r x y) -> absolute Format.Y r x y
      DY (Idx.DY r x y) -> delta Format.Y r x y

      DTime (Idx.DTime r s) ->
         Format.subscript (Format.delta Format.time) $
            Format.record r $ Format.section s

      Sum (Idx.Sum r dir x) ->
         Format.subscript Format.var $
            Format.record r $
            Format.direction dir `Format.connect` formatSectionNode x

      Store (Idx.Storage r x) ->
         Format.subscript Format.storage $
            Format.record r $ formatSectionNode x
