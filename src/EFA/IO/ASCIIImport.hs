
-- | ASCII Import

module EFA.IO.ASCIIImport (modelicaASCIIImport) where

import qualified EFA.IO.Parser as EFAParser
import EFA.IO.CSVParser (csvFile)
import Text.ParserCombinators.Parsec (Parser, parse)

import EFA.Signal.Record (Record(Record),SignalRecord, SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import qualified Data.Traversable as Trav
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Zip as Zip
import qualified Data.Map as Map


makeASCIIRecord ::
  (SV.Storage v a, SV.FromList v, Read a) =>
  NonEmpty.T [] (NonEmpty.T (NonEmpty.T []) String) ->
  Parser (SignalRecord v a)
makeASCIIRecord hs = do
  NonEmpty.Cons time sigs <-
    Trav.mapM (Trav.mapM EFAParser.cellContent) $
    Zip.transposeClip $ fmap NonEmpty.init $ NonEmpty.flatten hs
  return $
    Record
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
  case parse (makeASCIIRecord =<< csvFile ' ') path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return table
