module EFA2.Report.FormatValue where

import qualified EFA2.Report.Format as Format
import EFA2.Report.Format (Format)

import qualified EFA2.Interpreter.Env as Env

import Data.Ratio (Ratio)



class FormatValue a where
   formatValue :: Format output => a -> output


instance FormatValue a => FormatValue [a] where
   formatValue = Format.list . map formatValue

instance FormatValue Env.Index where
   formatValue = Format.index

instance FormatValue Double where
   formatValue = Format.real

instance (Integral a, Show a) => FormatValue (Ratio a) where
   formatValue = Format.ratio

instance FormatValue Char where
   formatValue = formatChar

formatChar :: Format output => Char -> output
formatChar = Format.literal . (:[])
