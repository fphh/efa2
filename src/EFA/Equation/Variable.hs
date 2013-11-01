{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.Variable where

import qualified EFA.Flow.Storage.Variable as StorageVar
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue
          (FormatValue, formatValue,
           FormatSignalIndex, formatSignalIndex,
           FormatScalarIndex, formatScalarIndex)


data Any part node =
     Signal (InPartSignal part node)
   | Scalar (ForStorageScalar part node)
     deriving (Show, Eq, Ord)

type SectionAny = Any Idx.Section
type StateAny   = Any Idx.State


data Signal node =
     Energy (Idx.Energy node)
   | Power (Idx.Power node)
   | Eta (Idx.Eta node)
   | DTime (Idx.DTime node)
   | X (Idx.X node)
   | Sum (Idx.Sum node)
     deriving (Show, Eq, Ord)


type InPartSignal part = Idx.InPart part Signal
type InSectionSignal   = Idx.InSection Signal
type InStateSignal     = Idx.InState Signal

type ForStorageScalar part   = Idx.ForStorage (StorageVar.Scalar part)
type ForStorageSectionScalar = ForStorageScalar Idx.Section
type ForStorageStateScalar   = ForStorageScalar Idx.State

class Index t where
   type Type t :: * -> *
   index :: t a -> Type t a

instance SignalIndex idx => Index (Idx.InPart part idx) where
   type Type (Idx.InPart part idx) = InPartSignal part
   index = Idx.liftInPart signalIndex

instance ScalarIndex idx => Index (Idx.ForStorage idx) where
   type Type (Idx.ForStorage idx) = ForStorageScalar (ScalarPart idx)
   index = Idx.liftForStorage scalarIndex


class FormatSignalIndex t => SignalIndex t where
   signalIndex :: t a -> Signal a

instance SignalIndex Idx.Energy where signalIndex = Energy
instance SignalIndex Idx.Power  where signalIndex = Power
instance SignalIndex Idx.Eta    where signalIndex = Eta
instance SignalIndex Idx.DTime  where signalIndex = DTime
instance SignalIndex Idx.X      where signalIndex = X
instance SignalIndex Idx.Sum    where signalIndex = Sum


class FormatScalarIndex t => ScalarIndex t where
   type ScalarPart t :: *
   scalarIndex :: t -> StorageVar.Scalar (ScalarPart t)

instance ScalarIndex Idx.Storage where
   type ScalarPart Idx.Storage = Idx.Section
   scalarIndex = StorageVar.Storage

instance ScalarIndex Idx.MaxEnergy where
   type ScalarPart Idx.MaxEnergy = Idx.Section
   scalarIndex = StorageVar.MaxEnergy

instance (Format.Part part) => ScalarIndex (Idx.StEnergy part) where
   type ScalarPart (Idx.StEnergy part) = part
   scalarIndex = StorageVar.Energy

instance (Format.Part part) => ScalarIndex (Idx.StX part) where
   type ScalarPart (Idx.StX part) = part
   scalarIndex = StorageVar.X

instance (Format.Part part) => ScalarIndex (Idx.StInSum part) where
   type ScalarPart (Idx.StInSum part) = part
   scalarIndex = StorageVar.InSum

instance (Format.Part part) => ScalarIndex (Idx.StOutSum part) where
   type ScalarPart (Idx.StOutSum part) = part
   scalarIndex = StorageVar.OutSum


(<#>) ::
   (ScalarIndex idx, ScalarPart idx ~ part) =>
   idx -> node -> ForStorageScalar part node
(<#>) idx node = Idx.ForStorage (scalarIndex idx) node

(<~>) ::
   (SignalIndex idx) =>
   part -> idx node -> InPartSignal part node
(<~>) part idx = Idx.InPart part $ signalIndex idx


instance
   (Format.Part part, Node.C node) =>
      FormatValue (Any part node) where
   formatValue (Signal var) = formatSignalValue var
   formatValue (Scalar var) = StorageVar.formatScalarValue var

formatSignalValue ::
   (Format output, Format.Part part, Node.C node) =>
   InPartSignal part node -> output
formatSignalValue (Idx.InPart s var) =
   case var of
      Energy idx -> formatSignalIndex idx s
      Power idx -> formatSignalIndex idx s
      Eta idx -> formatSignalIndex idx s
      X idx -> formatSignalIndex idx s
      DTime idx -> formatSignalIndex idx s
      Sum idx -> formatSignalIndex idx s


class FormatIndex idx where
   formatIndex :: (Node.C node, Format output) => idx node -> output

instance
   (Format.Part part, FormatSignalIndex idx) =>
      FormatIndex (Idx.InPart part idx) where
   formatIndex (Idx.InPart s idx) = formatSignalIndex idx s

instance FormatScalarIndex idx => FormatIndex (Idx.ForStorage idx) where
   formatIndex (Idx.ForStorage idx n) = formatScalarIndex idx n


instance FormatSignalIndex Signal where
   formatSignalIndex edge sec = formatSignalValue (Idx.InPart sec edge)


instance FormatIndex Signal where
   formatIndex = formatValue

signalIdent :: Format output => Signal node -> output
signalIdent var =
   case var of
      Energy _idx -> Format.energy
      Power _idx -> Format.power
      Eta _idx -> Format.eta
      X _idx -> Format.xfactor
      DTime _idx -> Format.dtime
      Sum _idx -> Format.signalSum

instance Format.EdgeIdx Signal where
   edgeIdent = signalIdent


instance (Node.C node) => FormatValue (Signal node) where
   formatValue var =
      case var of
         Energy idx -> formatValue idx
         Power idx -> formatValue idx
         Eta idx -> formatValue idx
         X idx -> formatValue idx
         DTime idx -> formatValue idx
         Sum idx -> formatValue idx


instance Format.TopologyIdx Signal where
   structureIdent (Idx.InPart _part var) = signalIdent var


checkedLookup ::
   (Node.C node, FormatIndex idx) =>
   String -> (idx node -> t -> Maybe b) -> idx node -> t -> b
checkedLookup name lk idx gr =
   maybe (error $ name ++ " " ++ Format.unUnicode (formatIndex idx)) id $
   lk idx gr
