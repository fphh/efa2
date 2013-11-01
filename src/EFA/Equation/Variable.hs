{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.Variable where

import qualified EFA.Flow.Storage.Variable as StorageVar
import qualified EFA.Flow.Topology.Variable as TopoVar
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
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

type ForStorageScalar part   = Idx.ForStorage (StorageVar.Scalar part)
type ForStorageSectionScalar = ForStorageScalar Idx.Section
type ForStorageStateScalar   = ForStorageScalar Idx.State

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
   (Format.Part part, Node.C node) =>
      FormatValue (Any part node) where
   formatValue (Signal var) = formatSignalValue var
   formatValue (Scalar var) = formatValue var

formatSignalValue ::
   (Format output, Format.Part part, Node.C node) =>
   InPartSignal part node -> output
formatSignalValue (Idx.InPart s var) =
   case var of
      TopoVar.Energy idx -> formatSignalIndex idx s
      TopoVar.Power idx -> formatSignalIndex idx s
      TopoVar.Eta idx -> formatSignalIndex idx s
      TopoVar.X idx -> formatSignalIndex idx s
      TopoVar.DTime idx -> formatSignalIndex idx s
      TopoVar.Sum idx -> formatSignalIndex idx s


class FormatIndex idx where
   formatIndex :: (Node.C node, Format output) => idx node -> output

instance
   (Format.Part part, FormatSignalIndex idx) =>
      FormatIndex (Idx.InPart part idx) where
   formatIndex (Idx.InPart s idx) = formatSignalIndex idx s

instance FormatScalarIndex idx => FormatIndex (Idx.ForStorage idx) where
--   formatIndex (Idx.ForStorage idx n) = formatScalarIndex idx n
   formatIndex = formatValue


instance FormatSignalIndex TopoVar.Signal where
   formatSignalIndex edge sec = formatSignalValue (Idx.InPart sec edge)


instance FormatIndex TopoVar.Signal where
   formatIndex = formatValue


checkedLookup ::
   (Node.C node, FormatIndex idx) =>
   String -> (idx node -> t -> Maybe b) -> idx node -> t -> b
checkedLookup name lk idx gr =
   maybe (error $ name ++ " " ++ Format.unUnicode (formatIndex idx)) id $
   lk idx gr
