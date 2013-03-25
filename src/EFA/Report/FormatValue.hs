module EFA.Report.FormatValue where

import qualified EFA.Graph.Topology.Index as Idx
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

instance (Foldable f, FormatValue a) => FormatValue (NonEmpty.T f a) where
   formatValue = formatValue . toList

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


instance FormatValue Idx.Absolute where
   formatValue Idx.Absolute = Format.empty

instance FormatValue Idx.Delta where
   formatValue d = Format.recordDelta d Format.empty

instance FormatValue rec => FormatValue (Idx.ExtDelta rec) where
   formatValue (Idx.ExtDelta d r) = Format.recordDelta d $ formatValue r


formatChar :: Format output => Char -> output
formatChar = Format.literal . (:[])

instance FormatValue a => FormatValue (Result a) where
  formatValue Undetermined = Format.undetermined
  formatValue (Determined a) = formatValue a
