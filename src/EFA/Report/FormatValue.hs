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


instance (FormatSignalIndex idx, Node.C node) => FormatValue (Idx.InSection idx node) where
   formatValue (Idx.InSection s idx) = formatSignalIndex idx s

instance (FormatScalarIndex idx, Node.C node) => FormatValue (Idx.ForNode idx node) where
   formatValue (Idx.ForNode idx n) = formatScalarIndex idx n


class FormatSignalIndex idx where
   formatSignalIndex ::
      (Node.C node, Format output) =>
      idx node -> Idx.Section -> output

class FormatScalarIndex idx where
   formatScalarIndex ::
      (Node.C node, Format output) =>
      idx node -> node -> output

formatBoundaryNode ::
   (Format output, Node.C node) =>
   Idx.BndNode node -> output
formatBoundaryNode (Idx.TimeNode s n) =
   Format.boundary s `Format.sectionNode` Node.subscript n

formatSectionNode ::
   (Format output, Node.C node) =>
   Idx.SecNode node -> output
formatSectionNode (Idx.TimeNode s n) =
   Format.section s `Format.sectionNode` Node.subscript n


formatStructureEdge ::
   (Format output, Node.C node) =>
   Format.EdgeVar -> Idx.StructureEdge node -> output
formatStructureEdge e (Idx.StructureEdge x y) =
   Format.subscript (Format.edgeIdent e) $
   Node.subscript x `Format.link` Node.subscript y

instance (Node.C node) => FormatValue (Idx.Energy node) where
   formatValue (Idx.Energy e) = formatStructureEdge Format.Energy e

instance (Node.C node) => FormatValue (Idx.Power node) where
   formatValue (Idx.Power e) = formatStructureEdge Format.Power e

instance (Node.C node) => FormatValue (Idx.Eta node) where
   formatValue (Idx.Eta e) = formatStructureEdge Format.Eta e

instance (Node.C node) => FormatValue (Idx.X node) where
   formatValue (Idx.X e) = formatStructureEdge Format.X e


formatStructureSecEdge ::
   (Format output, Node.C node) =>
   Format.EdgeVar -> Idx.StructureEdge node -> Idx.Section -> output
formatStructureSecEdge e (Idx.StructureEdge x y) s =
   Format.subscript (Format.edgeIdent e) $
   Format.section s `Format.sectionNode`
      (Node.subscript x `Format.link` Node.subscript y)

formatStorageEdge ::
   (Format output, Node.C node) =>
   Format.EdgeVar -> Idx.StorageEdge node -> node -> output
formatStorageEdge e (Idx.StorageEdge s0 s1) n =
   Format.subscript (Format.edgeIdent e) $
   (Format.initOrSection s0 `Format.link` Format.sectionOrExit s1)
      `Format.sectionNode` Node.subscript n

formatStorageTrans ::
   (Format output, Node.C node) =>
   Format.EdgeVar -> Idx.StorageTrans node -> node -> output
formatStorageTrans e (Idx.StorageTrans s0 s1) n =
   Format.subscript (Format.edgeIdent e) $
   (Format.augmentedSection s0 `Format.link` Format.augmentedSection s1)
      `Format.sectionNode` Node.subscript n


instance FormatSignalIndex Idx.Energy where
   formatSignalIndex (Idx.Energy e) = formatStructureSecEdge Format.Energy e

instance FormatSignalIndex Idx.Power where
   formatSignalIndex (Idx.Power e) = formatStructureSecEdge Format.Power e

instance FormatSignalIndex Idx.Eta where
   formatSignalIndex (Idx.Eta e) = formatStructureSecEdge Format.Eta e

instance FormatSignalIndex Idx.X where
   formatSignalIndex (Idx.X e) = formatStructureSecEdge Format.X e

instance FormatSignalIndex Idx.DTime where
   formatSignalIndex Idx.DTime s =
      Format.subscript Format.dtime $ Format.section s

instance FormatSignalIndex Idx.Sum where
   formatSignalIndex (Idx.Sum dir n) s =
      Format.subscript Format.signalSum $
      Format.direction dir `Format.connect`
         formatSectionNode (Idx.TimeNode s n)


instance FormatScalarIndex Idx.MaxEnergy where
   formatScalarIndex (Idx.MaxEnergy e) = formatStorageEdge Format.MaxEnergy e

instance FormatScalarIndex Idx.Storage where
   formatScalarIndex (Idx.Storage bnd) n =
      Format.subscript Format.storage $
      formatBoundaryNode (Idx.TimeNode bnd n)

instance FormatScalarIndex Idx.StEnergy where
   formatScalarIndex (Idx.StEnergy e) = formatStorageEdge Format.Energy e

instance FormatScalarIndex Idx.StX where
   formatScalarIndex (Idx.StX e) = formatStorageTrans Format.X e

instance FormatScalarIndex Idx.StInSum where
   formatScalarIndex (Idx.StInSum s) n =
      formatStSum Idx.In (Format.sectionOrExit s) n

instance FormatScalarIndex Idx.StOutSum where
   formatScalarIndex (Idx.StOutSum s) n =
      formatStSum Idx.Out (Format.initOrSection s) n

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
