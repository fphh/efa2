{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.Variable where

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
   | Scalar (ForNodeScalar part node)
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

data Scalar part node =
     MaxEnergy (Idx.MaxEnergy node)
   | Storage (Idx.Storage node)
   | StEnergy (Idx.StEnergy part node)
   | StX (Idx.StX part node)
   | StInSum (Idx.StInSum part node)
   | StOutSum (Idx.StOutSum part node)
     deriving (Show, Eq, Ord)


type InPartSignal part = Idx.InPart part Signal
type InSectionSignal   = Idx.InSection Signal
type InStateSignal     = Idx.InState Signal

type ForNodeScalar part   = Idx.ForNode (Scalar part)
type ForNodeSectionScalar = ForNodeScalar Idx.Section
type ForNodeStateScalar   = ForNodeScalar Idx.State

class Index t where
   type Type t :: * -> *
   index :: t a -> Type t a

instance SignalIndex idx => Index (Idx.InPart part idx) where
   type Type (Idx.InPart part idx) = InPartSignal part
   index = Idx.liftInPart signalIndex

instance ScalarIndex idx => Index (Idx.ForNode idx) where
   type Type (Idx.ForNode idx) = ForNodeScalar (ScalarPart idx)
   index = Idx.liftForNode scalarIndex


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
   scalarIndex :: t a -> Scalar (ScalarPart t) a

instance ScalarIndex Idx.MaxEnergy where
   type ScalarPart Idx.MaxEnergy = Idx.Section
   scalarIndex = MaxEnergy

instance ScalarIndex Idx.Storage where
   type ScalarPart Idx.Storage = Idx.Section
   scalarIndex = Storage

instance (Format.Part part) => ScalarIndex (Idx.StEnergy part) where
   type ScalarPart (Idx.StEnergy part) = part
   scalarIndex = StEnergy

instance (Format.Part part) => ScalarIndex (Idx.StX part) where
   type ScalarPart (Idx.StX part) = part
   scalarIndex = StX

instance (Format.Part part) => ScalarIndex (Idx.StInSum part) where
   type ScalarPart (Idx.StInSum part) = part
   scalarIndex = StInSum

instance (Format.Part part) => ScalarIndex (Idx.StOutSum part) where
   type ScalarPart (Idx.StOutSum part) = part
   scalarIndex = StOutSum


instance
   (Format.Part part, Node.C node) =>
      FormatValue (Any part node) where
   formatValue (Signal var) = formatSignalValue var
   formatValue (Scalar var) = formatScalarValue var

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

formatScalarValue ::
   (Format output, Format.Part part, Node.C node) =>
   ForNodeScalar part node -> output
formatScalarValue (Idx.ForNode var n) =
   case var of
      MaxEnergy idx -> formatScalarIndex idx n
      Storage idx -> formatScalarIndex idx n
      StEnergy idx -> formatScalarIndex idx n
      StX idx -> formatScalarIndex idx n
      StInSum idx -> formatScalarIndex idx n
      StOutSum idx -> formatScalarIndex idx n


class FormatIndex idx where
   formatIndex :: (Node.C node, Format output) => idx node -> output

instance
   (Format.Part part, FormatSignalIndex idx) =>
      FormatIndex (Idx.InPart part idx) where
   formatIndex (Idx.InPart s idx) = formatSignalIndex idx s

instance FormatScalarIndex idx => FormatIndex (Idx.ForNode idx) where
   formatIndex (Idx.ForNode idx n) = formatScalarIndex idx n


instance FormatSignalIndex Signal where
   formatSignalIndex edge sec = formatSignalValue (Idx.InPart sec edge)

instance (Format.Part part) => FormatScalarIndex (Scalar part) where
   formatScalarIndex edge node = formatScalarValue (Idx.ForNode edge node)


instance Format.StructureIdx Signal where
   structureIdent (Idx.InPart _part var) =
      case var of
         Energy _idx -> Format.energy
         Power _idx -> Format.power
         Eta _idx -> Format.eta
         X _idx -> Format.xfactor
         DTime _idx -> Format.dtime
         Sum _idx -> Format.signalSum

instance Format.StorageIdx (Scalar part) where
   storageIdent (Idx.ForNode var _node) =
      case var of
         MaxEnergy _idx -> Format.maxEnergy
         StEnergy _idx -> Format.energy
         StX _idx -> Format.xfactor
         StInSum _idx -> Format.scalarSum
         StOutSum _idx -> Format.scalarSum
         Storage _idx -> Format.storage


checkedLookup ::
   (Node.C node, FormatIndex idx) =>
   String -> (idx node -> t -> Maybe b) -> idx node -> t -> b
checkedLookup name lk idx gr =
   maybe (error $ name ++ " " ++ Format.unUnicode (formatIndex idx)) id $
   lk idx gr
