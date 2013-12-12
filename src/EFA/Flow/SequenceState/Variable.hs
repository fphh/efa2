{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.SequenceState.Variable where

import qualified EFA.Flow.Storage.Variable as StorageVar
import qualified EFA.Flow.Topology.Variable as TopoVar
import qualified EFA.Flow.Part.Index as PartIdx
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue
          (FormatValue, formatValue,
           FormatSignalIndex, formatSignalIndex,
           FormatScalarIndex)


data Any part node =
     Signal (InPartSignal part node)
   | Scalar (ForStorageScalar part node)
     deriving (Show, Eq, Ord)

type SectionAny = Any Idx.Section
type StateAny   = Any Idx.State


type InPartSignal part = Idx.InPart part TopoVar.Signal
type InSectionSignal   = Idx.InSection TopoVar.Signal
type InStateSignal     = Idx.InState TopoVar.Signal
type RecordInSectionSignal rec node =
        RecIdx.Record (Record.ToIndex rec) (InSectionSignal node)
type RecordInStateSignal rec node =
        RecIdx.Record (Record.ToIndex rec) (InStateSignal node)

type ForStorageScalar part   = Idx.ForStorage (StorageVar.Scalar part)
type ForStorageSectionScalar = ForStorageScalar Idx.Section
type ForStorageStateScalar   = ForStorageScalar Idx.State
type RecordForStorageSectionScalar rec node =
        RecIdx.Record (Record.ToIndex rec) (ForStorageSectionScalar node)
type RecordForStorageStateScalar rec node =
        RecIdx.Record (Record.ToIndex rec) (ForStorageStateScalar node)

class Index t where
   type Type t :: * -> *
   index :: t a -> Type t a

instance TopoVar.Index idx => Index (Idx.InPart part idx) where
   type Type (Idx.InPart part idx) = InPartSignal part
   index = Idx.liftInPart TopoVar.index

instance StorageVar.Index idx => Index (Idx.ForStorage idx) where
   type Type (Idx.ForStorage idx) = ForStorageScalar (StorageVar.Part idx)
   index = Idx.liftForStorage StorageVar.index


(<#>) ::
   (StorageVar.Index idx, StorageVar.Part idx ~ part) =>
   idx -> node -> ForStorageScalar part node
(<#>) idx node = Idx.ForStorage (StorageVar.index idx) node

(<~>) ::
   (TopoVar.Index idx) =>
   part -> idx node -> InPartSignal part node
(<~>) part idx = Idx.InPart part $ TopoVar.index idx


instance
   (PartIdx.Format part, Node.C node) =>
      FormatValue (Any part node) where
   formatValue (Signal (Idx.InPart s var)) = TopoVar.formatSignalValue var s
   formatValue (Scalar var) = formatValue var


class FormatIndex idx where
   formatIndex :: (Node.C node, Format output) => idx node -> output

instance
   (PartIdx.Format part, FormatSignalIndex idx) =>
      FormatIndex (Idx.InPart part idx) where
   formatIndex (Idx.InPart s idx) = formatSignalIndex idx s

instance FormatScalarIndex idx => FormatIndex (Idx.ForStorage idx) where
   formatIndex = formatValue


instance FormatIndex TopoVar.Signal where
   formatIndex = formatValue


checkedLookup ::
   (Node.C node, FormatIndex idx) =>
   String -> (idx node -> t -> Maybe b) -> idx node -> t -> b
checkedLookup name lk idx gr =
   maybe (error $ name ++ " " ++ Format.unUnicode (formatIndex idx)) id $
   lk idx gr
