module EFA.Report.FormatValue where

import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
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
      FormatValue (Idx.Record rec idx) where
   formatValue (Idx.Record r idx) =
      Format.record r $ formatValue idx


instance
   (FormatSignalIndex idx, Format.Part part, Node.C node) =>
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
      (Node.C node, Format.Part part, Format output) =>
      idx node -> part -> output

class FormatScalarIndex idx where
   formatScalarIndex :: (Format output) => idx -> (output, output)

formatPartNode ::
   (Format output, Format.Part part, Node.C node) =>
   Idx.PartNode part node -> output
formatPartNode (Idx.PartNode s n) =
   Format.part s `Format.sectionNode` Node.subscript n


formatTopologyLink ::
   (Format output, Node.C node) =>
   Idx.TopologyEdge node -> output
formatTopologyLink (Idx.TopologyEdge x y) =
   Node.subscript x `Format.link` Node.subscript y

formatTopologyEdge ::
   (Format output, Node.C node) =>
   output -> Idx.TopologyEdge node -> output
formatTopologyEdge e se =
   Format.subscript e $ formatTopologyLink se

instance (Node.C node) => FormatValue (Idx.Energy node) where
   formatValue (Idx.Energy e) = formatTopologyEdge Format.energy e

instance (Node.C node) => FormatValue (Idx.Power node) where
   formatValue (Idx.Power e) = formatTopologyEdge Format.power e

instance (Node.C node) => FormatValue (Idx.Eta node) where
   formatValue (Idx.Eta e) = formatTopologyEdge Format.eta e

instance (Node.C node) => FormatValue (Idx.X node) where
   formatValue (Idx.X e) = formatTopologyEdge Format.xfactor e

instance (Node.C node) => FormatValue (Idx.DTime node) where
   formatValue Idx.DTime = Format.dtime

instance (Node.C node) => FormatValue (Idx.Sum node) where
   formatValue (Idx.Sum dir node) =
      Format.subscript Format.signalSum $
      Format.direction dir `Format.connect` Node.subscript node


formatTopologySecEdge ::
   (Format output, Format.Part part, Node.C node) =>
   output -> Idx.TopologyEdge node -> part -> output
formatTopologySecEdge e se s =
   Format.subscript e $
   Format.part s `Format.sectionNode` formatTopologyLink se


instance FormatSignalIndex Idx.Energy where
   formatSignalIndex (Idx.Energy e) = formatTopologySecEdge Format.energy e

instance FormatSignalIndex Idx.Power where
   formatSignalIndex (Idx.Power e) = formatTopologySecEdge Format.power e

instance FormatSignalIndex Idx.Eta where
   formatSignalIndex (Idx.Eta e) = formatTopologySecEdge Format.eta e

instance FormatSignalIndex Idx.X where
   formatSignalIndex (Idx.X e) = formatTopologySecEdge Format.xfactor e

instance FormatSignalIndex Idx.DTime where
   formatSignalIndex Idx.DTime s =
      Format.subscript Format.dtime $ Format.part s

instance FormatSignalIndex Idx.Sum where
   formatSignalIndex (Idx.Sum dir n) s =
      Format.subscript Format.signalSum $
      Format.direction dir `Format.connect`
         formatPartNode (Idx.PartNode s n)


instance FormatScalarIndex StorageIdx.Content where
   formatScalarIndex (StorageIdx.Content bnd) =
      (Format.storage, Format.boundary bnd)


formatCarryEdge ::
   (Format.Part sec, Format output) =>
   Idx.CarryEdge sec -> output
formatCarryEdge (Idx.CarryEdge s0 s1) =
   Format.initOrOther s0 `Format.link` Format.otherOrExit s1

instance FormatScalarIndex StorageIdx.MaxEnergy where
   formatScalarIndex (StorageIdx.MaxEnergy e) = (Format.maxEnergy, formatCarryEdge e)

instance (Format.Part sec) => FormatScalarIndex (StorageIdx.Energy sec) where
   formatScalarIndex (StorageIdx.Energy e) = (Format.energy, formatCarryEdge e)


formatCarryBond ::
   (Format.Part sec, Format output) =>
   Idx.CarryBond sec -> output
formatCarryBond (Idx.CarryBond s0 s1) =
   Format.augmented s0 `Format.link` Format.augmented s1

instance (Format.Part sec) => FormatScalarIndex (StorageIdx.X sec) where
   formatScalarIndex (StorageIdx.X e) = (Format.xfactor, formatCarryBond e)


formatStSum ::
   (Format output) =>
   Idx.Direction -> output -> (output, output)
formatStSum dir s =
   (Format.scalarSum, Format.direction dir `Format.connect` s)

instance (Format.Part sec) => FormatScalarIndex (StorageIdx.InSum sec) where
   formatScalarIndex (StorageIdx.InSum s) =
      formatStSum Idx.In (Format.otherOrExit s)

instance (Format.Part sec) => FormatScalarIndex (StorageIdx.OutSum sec) where
   formatScalarIndex (StorageIdx.OutSum s) =
      formatStSum Idx.Out (Format.initOrOther s)


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
