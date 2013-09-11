module EFA.Report.FormatValue where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Equation.Result (Result(..))

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


instance FormatValue Idx.Absolute where
   formatValue Idx.Absolute = Format.empty

instance FormatValue Idx.Delta where
   formatValue d = Format.recordDelta d Format.empty

instance FormatValue rec => FormatValue (Idx.ExtDelta rec) where
   formatValue (Idx.ExtDelta d r) = Format.recordDelta d $ formatValue r


instance
   (FormatSignalIndex idx, Format.Part part, Node.C node) =>
      FormatValue (Idx.InPart part idx node) where
   formatValue (Idx.InPart s idx) = formatSignalIndex idx s

instance
   (FormatScalarIndex idx, Node.C node) =>
      FormatValue (Idx.ForNode idx node) where
   formatValue (Idx.ForNode idx n) = formatScalarIndex idx n


class FormatSignalIndex idx where
   formatSignalIndex ::
      (Node.C node, Format.Part part, Format output) =>
      idx node -> part -> output

class FormatScalarIndex idx where
   formatScalarIndex ::
      (Node.C node, Format output) =>
      idx node -> node -> output

formatBoundaryNode ::
   (Format output, Node.C node) =>
   Idx.BndNode node -> output
formatBoundaryNode (Idx.PartNode s n) =
   Format.boundary s `Format.sectionNode` Node.subscript n

formatTimeNode ::
   (Format output, Format.Part part, Node.C node) =>
   Idx.PartNode part node -> output
formatTimeNode (Idx.PartNode s n) =
   Format.part s `Format.sectionNode` Node.subscript n


formatStructureLink ::
   (Format output, Node.C node) =>
   Idx.StructureEdge node -> output
formatStructureLink (Idx.StructureEdge x y) =
   Node.subscript x `Format.link` Node.subscript y

formatStructureEdge ::
   (Format output, Node.C node) =>
   output -> Idx.StructureEdge node -> output
formatStructureEdge e se =
   Format.subscript e $ formatStructureLink se

instance (Node.C node) => FormatValue (Idx.Energy node) where
   formatValue (Idx.Energy e) = formatStructureEdge Format.energy e

instance (Node.C node) => FormatValue (Idx.Power node) where
   formatValue (Idx.Power e) = formatStructureEdge Format.power e

instance (Node.C node) => FormatValue (Idx.Eta node) where
   formatValue (Idx.Eta e) = formatStructureEdge Format.eta e

instance (Node.C node) => FormatValue (Idx.X node) where
   formatValue (Idx.X e) = formatStructureEdge Format.xfactor e


formatStructureSecEdge ::
   (Format output, Format.Part part, Node.C node) =>
   output -> Idx.StructureEdge node -> part -> output
formatStructureSecEdge e se s =
   Format.subscript e $
   Format.part s `Format.sectionNode` formatStructureLink se

formatStorageEdge ::
   (Format.Part sec, Format output, Node.C node) =>
   output -> Idx.StorageEdge sec node -> node -> output
formatStorageEdge e (Idx.StorageEdge s0 s1) n =
   Format.subscript e $
   (Format.initOrOther s0 `Format.link` Format.otherOrExit s1)
      `Format.sectionNode` Node.subscript n

formatStorageTrans ::
   (Format.Part sec, Format output, Node.C node) =>
   output -> Idx.StorageTrans sec node -> node -> output
formatStorageTrans e (Idx.StorageTrans s0 s1) n =
   Format.subscript e $
   (Format.augmented s0 `Format.link` Format.augmented s1)
      `Format.sectionNode` Node.subscript n


instance FormatSignalIndex Idx.Energy where
   formatSignalIndex (Idx.Energy e) = formatStructureSecEdge Format.energy e

instance FormatSignalIndex Idx.Power where
   formatSignalIndex (Idx.Power e) = formatStructureSecEdge Format.power e

instance FormatSignalIndex Idx.Eta where
   formatSignalIndex (Idx.Eta e) = formatStructureSecEdge Format.eta e

instance FormatSignalIndex Idx.X where
   formatSignalIndex (Idx.X e) = formatStructureSecEdge Format.xfactor e

instance FormatSignalIndex Idx.DTime where
   formatSignalIndex Idx.DTime s =
      Format.subscript Format.dtime $ Format.part s

instance FormatSignalIndex Idx.Sum where
   formatSignalIndex (Idx.Sum dir n) s =
      Format.subscript Format.signalSum $
      Format.direction dir `Format.connect`
         formatTimeNode (Idx.PartNode s n)


instance FormatScalarIndex Idx.MaxEnergy where
   formatScalarIndex (Idx.MaxEnergy e) = formatStorageEdge Format.maxEnergy e

instance FormatScalarIndex Idx.Storage where
   formatScalarIndex (Idx.Storage bnd) n =
      Format.subscript Format.storage $
      formatBoundaryNode (Idx.PartNode bnd n)

instance (Format.Part sec) => FormatScalarIndex (Idx.StEnergy sec) where
   formatScalarIndex (Idx.StEnergy e) = formatStorageEdge Format.energy e

instance (Format.Part sec) => FormatScalarIndex (Idx.StX sec) where
   formatScalarIndex (Idx.StX e) = formatStorageTrans Format.xfactor e

instance (Format.Part sec) => FormatScalarIndex (Idx.StInSum sec) where
   formatScalarIndex (Idx.StInSum s) n =
      formatStSum Idx.In (Format.otherOrExit s) n

instance (Format.Part sec) => FormatScalarIndex (Idx.StOutSum sec) where
   formatScalarIndex (Idx.StOutSum s) n =
      formatStSum Idx.Out (Format.initOrOther s) n

formatStSum ::
   (Format output, Node.C node) =>
   Idx.Direction -> output -> node -> output
formatStSum dir s n =
   Format.subscript Format.scalarSum $
   Format.direction dir `Format.connect`
      s `Format.sectionNode` Node.subscript n

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
