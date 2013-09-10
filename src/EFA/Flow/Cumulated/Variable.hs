module EFA.Flow.Cumulated.Variable where

import qualified EFA.Flow.Cumulated.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue
          (FormatValue, formatValue, formatStructureLink)

import Data.Maybe (fromMaybe)


data Any node =
     Energy (Idx.Energy node)
   | Power (Idx.Power node)
   | Eta (Idx.Eta node)
   | DTime (Idx.DTime node)
   | X (Idx.X node)
   | Sum (Idx.Sum node)
     deriving (Show, Eq, Ord)


class Index t where
   index :: t a -> Any a

instance Index Idx.Energy where index = Energy
instance Index Idx.Power  where index = Power
instance Index Idx.Eta    where index = Eta
instance Index Idx.DTime  where index = DTime
instance Index Idx.X      where index = X
instance Index Idx.Sum    where index = Sum


instance (Node.C node) => FormatValue (Any node) where
   formatValue var =
      case var of
         Energy idx -> formatIndex idx
         Power idx -> formatIndex idx
         Eta idx -> formatIndex idx
         X idx -> formatIndex idx
         DTime idx -> formatIndex idx
         Sum idx -> formatIndex idx



class FormatIndex idx where
   formatIndex ::
      (Node.C node, Format output) =>
      idx node -> output

instance FormatIndex Idx.Energy where
   formatIndex (Idx.Energy d e) = formatEdge Format.energy d e

instance FormatIndex Idx.Power where
   formatIndex (Idx.Power d e) = formatEdge Format.power d e

instance FormatIndex Idx.X where
   formatIndex (Idx.X d e) = formatEdge Format.xfactor d e

instance FormatIndex Idx.Sum where
   formatIndex (Idx.Sum dir n) =
      Format.subscript Format.signalSum $
      Format.direction dir `Format.connect` Node.subscript n

instance FormatIndex Idx.DTime where
   formatIndex (Idx.DTime e) =
      Format.subscript Format.dtime $ formatStructureLink e

instance FormatIndex Idx.Eta where
   formatIndex (Idx.Eta se) =
      Format.subscript Format.eta $ formatStructureLink se


formatEdge ::
   (Format output, Node.C node) =>
   output -> Idx.Direction -> Idx.StructureEdge node -> output
formatEdge e d se =
   Format.subscript e $
   Format.direction d `Format.connect` formatStructureLink se


checkedLookup ::
   (Node.C node, FormatIndex idx) =>
   String -> (idx node -> t -> Maybe b) -> idx node -> t -> b
checkedLookup name lk idx gr =
   fromMaybe (error $ name ++ " " ++ Format.unUnicode (formatIndex idx)) $
   lk idx gr
