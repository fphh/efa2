module EFA.Flow.Topology.Variable where

import qualified EFA.Flow.Topology.Index as Idx
import qualified EFA.Flow.Part.Index as PartIdx
import qualified EFA.Flow.SequenceState.Index as FlowIdx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue
          (FormatValue, formatValue,
           FormatSignalIndex, formatSignalIndex)


data Signal node =
     Energy (Idx.Energy node)
   | Power (Idx.Power node)
   | Eta (Idx.Eta node)
   | DTime (Idx.DTime node)
   | One (Idx.One node)
   | X (Idx.X node)
   | Sum (Idx.Sum node)
     deriving (Show, Eq, Ord)

type RecordSignal rec node = RecIdx.Record (Record.ToIndex rec) (Signal node)


class FormatSignalIndex t => Index t where
   index :: t a -> Signal a

instance Index Idx.Energy where index = Energy
instance Index Idx.Power  where index = Power
instance Index Idx.Eta    where index = Eta
instance Index Idx.DTime  where index = DTime
instance Index Idx.One    where index = One
instance Index Idx.X      where index = X
instance Index Idx.Sum    where index = Sum


ident :: Format output => Signal node -> output
ident var =
   case var of
      Energy _idx -> Format.energy
      Power _idx -> Format.power
      Eta _idx -> Format.eta
      X _idx -> Format.xfactor
      DTime _idx -> Format.dtime
      One _idx -> Format.integer 1
      Sum _idx -> Format.signalSum

instance FlowIdx.Identifier Signal where
   identifier = ident


formatSignalValue ::
   (Format output, PartIdx.Format part, Node.C node) =>
   Signal node -> part -> output
formatSignalValue var s =
   case var of
      Energy idx -> formatSignalIndex idx s
      Power idx -> formatSignalIndex idx s
      Eta idx -> formatSignalIndex idx s
      X idx -> formatSignalIndex idx s
      DTime idx -> formatSignalIndex idx s
      One idx -> formatSignalIndex idx s
      Sum idx -> formatSignalIndex idx s


instance FormatSignalIndex Signal where
   formatSignalIndex = formatSignalValue


instance (Node.C node) => FormatValue (Signal node) where
   formatValue var =
      case var of
         Energy idx -> formatValue idx
         Power idx -> formatValue idx
         Eta idx -> formatValue idx
         X idx -> formatValue idx
         DTime idx -> formatValue idx
         One idx -> formatValue idx
         Sum idx -> formatValue idx


instance Idx.Identifier Signal where
   identifier = ident


class FormatIndex idx where
   formatIndex :: (Node.C node, Format output) => idx node -> output

instance FormatIndex Idx.Energy where
   formatIndex = formatValue

instance FormatIndex Idx.Power where
   formatIndex = formatValue

instance FormatIndex Idx.Eta where
   formatIndex = formatValue

instance FormatIndex Idx.X where
   formatIndex = formatValue

instance FormatIndex Idx.DTime where
   formatIndex = formatValue

instance FormatIndex Idx.One where
   formatIndex = formatValue

instance FormatIndex Idx.Sum where
   formatIndex = formatValue


checkedLookup ::
   (Node.C node, FormatIndex idx) =>
   String -> (idx node -> t -> Maybe b) -> idx node -> t -> b
checkedLookup name lk idx =
   maybe (error $
             "Topology." ++ name ++
             " " ++ Format.unUnicode (formatIndex idx)) id .
   lk idx
