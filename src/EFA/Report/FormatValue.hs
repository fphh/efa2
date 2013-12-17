module EFA.Report.FormatValue where

import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Part.Index as PartIdx
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)

import qualified EFA.Equation.RecordIndex as RecIdx
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified Data.NonEmpty as NonEmpty
import Data.Foldable (Foldable, toList)
import Data.Ratio (Ratio)

class FormatValue a where
   formatValue :: Format output => a -> output

instance FormatValue a => FormatValue [a] where
   formatValue = Format.list . map formatValue

instance (FormatValue a, FormatValue b) => FormatValue (a,b) where
   formatValue (x,y) = Format.pair (formatValue x) (formatValue y)

instance (Foldable f, FormatValue a) => FormatValue (NonEmpty.T f a) where
   formatValue = formatValue . toList

instance FormatValue Double where
   formatValue = Format.real

instance (Integral a, Show a) => FormatValue (Ratio a) where
   formatValue = Format.ratioAuto

instance FormatValue Char where
   formatValue = formatChar

instance
   (Format.Record rec, FormatValue idx) =>
      FormatValue (RecIdx.Record rec idx) where
   formatValue (RecIdx.Record r idx) =
      Format.record r $ formatValue idx


instance
   (FormatSignalIndex idx, PartIdx.Format part, Node.C node) =>
      FormatValue (Idx.InPart part idx node) where
   formatValue (Idx.InPart s idx) = formatSignalIndex idx s

instance
   (FormatScalarIndex idx, Node.C node) =>
      FormatValue (Idx.ForStorage idx node) where
   formatValue (Idx.ForStorage idx n) =
      formatForStorage (formatScalarIndex idx) n

formatForStorage ::
   (Format output, Node.C node) =>
   (output, output) -> node -> output
formatForStorage (e, idx) n =
   Format.subscript e $ idx `Format.sectionNode` Node.subscript n


class FormatSignalIndex idx where
   formatSignalIndex ::
      (Node.C node, PartIdx.Format part, Format output) =>
      idx node -> part -> output

class FormatScalarIndex idx where
   formatScalarIndex :: (Format output) => idx -> (output, output)

formatPartNode ::
   (Format output, PartIdx.Format part, Node.C node) =>
   Idx.PartNode part node -> output
formatPartNode (Idx.PartNode s n) =
   PartIdx.format s `Format.sectionNode` Node.subscript n


formatTopologySubscript ::
   (Format output, Node.C node) =>
   TopoIdx.Position node -> output
formatTopologySubscript (TopoIdx.Position x y) =
   Node.subscript x `Format.link` Node.subscript y

formatTopologyPosition ::
   (Format output, Node.C node) =>
   output -> TopoIdx.Position node -> output
formatTopologyPosition e se =
   Format.subscript e $ formatTopologySubscript se

instance (Node.C node) => FormatValue (TopoIdx.Energy node) where
   formatValue (TopoIdx.Energy e) = formatTopologyPosition Format.energy e

instance (Node.C node) => FormatValue (TopoIdx.Power node) where
   formatValue (TopoIdx.Power e) = formatTopologyPosition Format.power e

instance (Node.C node) => FormatValue (TopoIdx.Eta node) where
   formatValue (TopoIdx.Eta e) = formatTopologyPosition Format.eta e

instance (Node.C node) => FormatValue (TopoIdx.X node) where
   formatValue (TopoIdx.X e) = formatTopologyPosition Format.xfactor e

instance (Node.C node) => FormatValue (TopoIdx.DTime node) where
   formatValue TopoIdx.DTime = Format.dtime

instance (Node.C node) => FormatValue (TopoIdx.One node) where
   formatValue TopoIdx.One = Format.integer 1

instance (Node.C node) => FormatValue (TopoIdx.Sum node) where
   formatValue (TopoIdx.Sum dir node) =
      Format.subscript Format.signalSum $
      TopoIdx.formatDirection dir `Format.connect` Node.subscript node


formatTopologySecEdge ::
   (Format output, PartIdx.Format part, Node.C node) =>
   output -> TopoIdx.Position node -> part -> output
formatTopologySecEdge e se s =
   Format.subscript e $
   PartIdx.format s `Format.sectionNode` formatTopologySubscript se


instance FormatSignalIndex TopoIdx.Energy where
   formatSignalIndex (TopoIdx.Energy e) = formatTopologySecEdge Format.energy e

instance FormatSignalIndex TopoIdx.Power where
   formatSignalIndex (TopoIdx.Power e) = formatTopologySecEdge Format.power e

instance FormatSignalIndex TopoIdx.Eta where
   formatSignalIndex (TopoIdx.Eta e) = formatTopologySecEdge Format.eta e

instance FormatSignalIndex TopoIdx.X where
   formatSignalIndex (TopoIdx.X e) = formatTopologySecEdge Format.xfactor e

instance FormatSignalIndex TopoIdx.DTime where
   formatSignalIndex TopoIdx.DTime s =
      Format.subscript Format.dtime $ PartIdx.format s

instance FormatSignalIndex TopoIdx.One where
   formatSignalIndex TopoIdx.One s =
      Format.subscript (Format.integer 1) $ PartIdx.format s

instance FormatSignalIndex TopoIdx.Sum where
   formatSignalIndex (TopoIdx.Sum dir n) s =
      Format.subscript Format.signalSum $
      TopoIdx.formatDirection dir `Format.connect`
         formatPartNode (Idx.PartNode s n)


instance FormatScalarIndex StorageIdx.Content where
   formatScalarIndex (StorageIdx.Content bnd) =
      (Format.storage, PartIdx.formatBoundary bnd)


formatCarryEdge ::
   (PartIdx.Format part, Format output) =>
   StorageIdx.Edge part -> output
formatCarryEdge (StorageIdx.Edge s0 s1) =
   PartIdx.formatInitOrOther s0 `Format.link` PartIdx.formatOtherOrExit s1

instance FormatScalarIndex StorageIdx.MaxEnergy where
   formatScalarIndex (StorageIdx.MaxEnergy e) = (Format.maxEnergy, formatCarryEdge e)

instance (PartIdx.Format part) => FormatScalarIndex (StorageIdx.Energy part) where
   formatScalarIndex (StorageIdx.Energy e) = (Format.energy, formatCarryEdge e)


formatCarryPosition ::
   (PartIdx.Format part, Format output) =>
   StorageIdx.Position part -> output
formatCarryPosition (StorageIdx.Position s0 s1) =
   PartIdx.formatAugmented s0 `Format.link` PartIdx.formatAugmented s1

instance (PartIdx.Format part) => FormatScalarIndex (StorageIdx.X part) where
   formatScalarIndex (StorageIdx.X e) = (Format.xfactor, formatCarryPosition e)


formatStSum ::
   (Format output) =>
   TopoIdx.Direction -> output -> (output, output)
formatStSum dir s =
   (Format.scalarSum, TopoIdx.formatDirection dir `Format.connect` s)

instance (PartIdx.Format part) => FormatScalarIndex (StorageIdx.InSum part) where
   formatScalarIndex (StorageIdx.InSum s) =
      formatStSum TopoIdx.In (PartIdx.formatOtherOrExit s)

instance (PartIdx.Format part) => FormatScalarIndex (StorageIdx.OutSum part) where
   formatScalarIndex (StorageIdx.OutSum s) =
      formatStSum TopoIdx.Out (PartIdx.formatInitOrOther s)


formatChar :: Format output => Char -> output
formatChar = Format.literal . (:[])

instance FormatValue a => FormatValue (Result a) where
   formatValue Undetermined = Format.undetermined
   formatValue (Determined a) = formatValue a


formatAssign ::
   (FormatValue var, FormatValue a, Format output) =>
   var -> a -> output
formatAssign var val =
   Format.assign (formatValue var) (formatValue val)
