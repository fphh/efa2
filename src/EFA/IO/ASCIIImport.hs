
-- | ASCII Import

module EFA.IO.ASCIIImport (modelicaASCIIImport) where

import EFA.IO.CSVParser (csvFile)

import EFA.Signal.Record (Record(Record),SignalRecord, SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import qualified Data.Traversable as Trav
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Zip as Zip
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec (Parser, parse)


parseCellContent ::
  (Read a) =>
  String -> Parser a
parseCellContent str =
  case reads str of
    [(a,"")] -> return a
    _ -> fail $ "could not parse cell content: " ++ show str

makeASCIIRecord ::
  (SV.Storage v a, SV.FromList v, Read a) =>
  NonEmpty.T [] (NonEmpty.T (NonEmpty.T []) String) ->
  Parser (SignalRecord v a)
makeASCIIRecord hs = do
  NonEmpty.Cons time sigs <-
    Trav.mapM (Trav.mapM parseCellContent) $
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
