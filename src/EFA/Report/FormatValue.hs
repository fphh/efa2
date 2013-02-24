module EFA.Report.FormatValue where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Equation.Result (Result(..))

import Data.Ratio (Ratio)



class FormatValue a where
   formatValue :: Format output => a -> output


instance FormatValue a => FormatValue [a] where
   formatValue = Format.list . map formatValue

instance FormatValue Double where
   formatValue = Format.real

instance (Integral a, Show a) => FormatValue (Ratio a) where
   formatValue = Format.ratio

instance FormatValue Char where
   formatValue = formatChar

instance
   (Format.Record rec, FormatValue idx) =>
      FormatValue (Idx.Record rec idx) where
   formatValue (Idx.Record r idx) =
      Format.record r $ formatValue idx

formatChar :: Format output => Char -> output
formatChar = Format.literal . (:[])

instance FormatValue a => FormatValue (Result a) where
  formatValue Undetermined = Format.undetermined
  formatValue (Determined a) = formatValue a
