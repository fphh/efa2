
-- | ASCII Import

module EFA.IO.ASCIIImport (modelicaASCIIImport) where

import EFA.IO.CSVParser (csvFileWithHeader, cellContent)
import Text.ParserCombinators.Parsec (parse)

import EFA.Signal.Record (Record(Record),SignalRecord, SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Zip as Zip
import qualified Data.Map as Map


makeASCIIRecord ::
  (SV.Storage v a, SV.FromList v) =>
  [NonEmpty.T (NonEmpty.T []) a] -> SignalRecord v a
makeASCIIRecord hs =
  let NonEmpty.Cons time sigs =
        Zip.transposeClip $ map NonEmpty.init hs
  in  Record
        (S.fromList time)
        (Map.fromList $
         zip
           (map (SigId . ("sig_" ++) . show) [(0::Int)..])
           (map S.fromList sigs))

-- | Main ASCII Import Function
modelicaASCIIImport ::
  (SV.Storage v a, SV.FromList v, Read a) =>
  FilePath -> IO (SignalRecord v a)
modelicaASCIIImport path = do
  text <- readFile path
  let parser =
        csvFileWithHeader (NonEmptyC.repeat cellContent) cellContent ' '
  case parse parser path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return $ makeASCIIRecord $ uncurry (:) table
