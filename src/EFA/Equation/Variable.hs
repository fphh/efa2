{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.Variable where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import EFA.Report.Format (Format)
import EFA.Report.FormatValue
          (FormatValue, formatValue,
           FormatSignalIndex, formatSignalIndex,
           FormatScalarIndex, formatScalarIndex)


data Any a =
     Signal (InSectionSignal a)
   | Scalar (ForNodeScalar a)
     deriving (Show, Eq, Ord)

data Signal a =
     Energy (Idx.Energy a)
   | Power (Idx.Power a)
   | Eta (Idx.Eta a)
   | DTime (Idx.DTime a)
   | X (Idx.X a)
   | Sum (Idx.Sum a)
     deriving (Show, Eq, Ord)

data Scalar a =
     MaxEnergy (Idx.MaxEnergy a)
   | Storage (Idx.Storage a)
   | StEnergy (Idx.StEnergy Idx.Section a)
   | StX (Idx.StX Idx.Section a)
   | StInSum (Idx.StInSum Idx.Section a)
   | StOutSum (Idx.StOutSum Idx.Section a)
     deriving (Show, Eq, Ord)



type InSectionSignal = Idx.InSection Signal
type ForNodeScalar   = Idx.ForNode   Scalar

class Index t where
   type Type t :: * -> *
   index :: t a -> Type t a

instance SignalIndex idx => Index (Idx.InSection idx) where
   type Type (Idx.InSection idx) = InSectionSignal
   index (Idx.InSection s x) = Idx.InSection s (signalIndex x)

instance ScalarIndex idx => Index (Idx.ForNode idx) where
   type Type (Idx.ForNode idx) = ForNodeScalar
   index (Idx.ForNode x n) = Idx.ForNode (scalarIndex x) n


class SignalIndex t where
   signalIndex :: t a -> Signal a

instance SignalIndex Idx.Energy where signalIndex = Energy
instance SignalIndex Idx.Power  where signalIndex = Power
instance SignalIndex Idx.Eta    where signalIndex = Eta
instance SignalIndex Idx.DTime  where signalIndex = DTime
instance SignalIndex Idx.X      where signalIndex = X
instance SignalIndex Idx.Sum    where signalIndex = Sum


class ScalarIndex t where
   scalarIndex :: t a -> Scalar a

instance ScalarIndex Idx.MaxEnergy where scalarIndex = MaxEnergy
instance ScalarIndex Idx.Storage   where scalarIndex = Storage
instance (Idx.ToSection sec) => ScalarIndex (Idx.StEnergy sec) where
   scalarIndex (Idx.StEnergy (Idx.StorageEdge from to)) =
      StEnergy $ Idx.StEnergy $
      Idx.StorageEdge (fmap Idx.toSection from) (fmap Idx.toSection to)
instance (Idx.ToSection sec) => ScalarIndex (Idx.StX sec) where
   scalarIndex (Idx.StX (Idx.StorageTrans from to)) =
      StX $ Idx.StX $
      Idx.StorageTrans
         (fmap (fmap Idx.toSection) from) (fmap (fmap Idx.toSection) to)
instance (Idx.ToSection sec) => ScalarIndex (Idx.StInSum sec) where
   scalarIndex (Idx.StInSum sec) =
      StInSum $ Idx.StInSum $ fmap Idx.toSection sec
instance (Idx.ToSection sec) => ScalarIndex (Idx.StOutSum sec) where
   scalarIndex (Idx.StOutSum sec) =
      StOutSum $ Idx.StOutSum $ fmap Idx.toSection sec


instance (Node.C node) => FormatValue (Any node) where
   formatValue (Signal var) = formatSignalValue var
   formatValue (Scalar var) = formatScalarValue var

formatSignalValue ::
   (Format output, Node.C node) =>
   InSectionSignal node -> output
formatSignalValue (Idx.InSection s var) =
   case var of
      Energy idx -> formatSignalIndex idx s
      Power idx -> formatSignalIndex idx s
      Eta idx -> formatSignalIndex idx s
      X idx -> formatSignalIndex idx s
      DTime idx -> formatSignalIndex idx s
      Sum idx -> formatSignalIndex idx s

formatScalarValue ::
   (Format output, Node.C node) =>
   ForNodeScalar node -> output
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

instance FormatSignalIndex idx => FormatIndex (Idx.InSection idx) where
   formatIndex (Idx.InSection s idx) = formatSignalIndex idx s

instance FormatScalarIndex idx => FormatIndex (Idx.ForNode idx) where
   formatIndex (Idx.ForNode idx n) = formatScalarIndex idx n


instance FormatSignalIndex Signal where
   formatSignalIndex edge sec = formatSignalValue (Idx.InSection sec edge)

instance FormatScalarIndex Scalar where
   formatScalarIndex edge node = formatScalarValue (Idx.ForNode edge node)
