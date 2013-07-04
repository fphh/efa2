-- | Modelica CSV Import

module EFA.IO.CSVImport (
  modelicaCSVImport,
  fortissCSVImport, filterWith, dontFilter,
  fortissCSVImportStruct, CSV.Header, CSV.Name(..), CSV.TrimmedName(..),
  ) where

import EFA.IO.CSVParser (csvFileWithHeader)
import Text.ParserCombinators.Parsec (parse)
import qualified EFA.IO.CSVParser as CSV

import EFA.Signal.Record(Record(Record),SignalRecord, SigId(SigId))
import EFA.Signal.Base (Val)

import qualified EFA.Signal.Signal as S

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Zip as Zip
import qualified Data.Map as Map


makeCSVRecord ::
  (NonEmpty.T (NonEmpty.T []) SigId,
   [NonEmpty.T (NonEmpty.T []) Val]) ->
  SignalRecord [] Val
makeCSVRecord (NonEmpty.Cons _timeStr sigNames, hs) =
  let NonEmpty.Cons time sigs =
        Zip.transposeClip $ fmap NonEmpty.init hs
  in  Record
        (S.fromList time)
        (Map.fromList $
         zip
           (NonEmpty.init sigNames)
           (map S.fromList sigs))

-- | Main Modelica CSV Import Function
modelicaCSVImport :: FilePath -> IO (SignalRecord [] Val)
modelicaCSVImport path = do
  text <- readFile path
  let checkTime str =
        case str of
          "time" -> Right $ SigId str
          _ -> Left "time column expected"
      cellContent "" = Right 0 -- needed for the right dummy column
      cellContent str = CSV.cellContent str
      parser =
        csvFileWithHeader
          (NonEmpty.Cons checkTime $ NonEmptyC.repeat (Right . SigId))
          cellContent ','
  case parse parser path text of
    Left err -> ioError . userError $ "Parse error in file " ++ show err
    Right table -> return $ makeCSVRecord table



type Filter = [[Val]] -> [[Val]]

filterWith :: Int -> (Val -> Bool) -> Filter
filterWith r p = filter (p . (!! r))

dontFilter :: Filter
dontFilter = id

fortissCSVRecord ::
  NonEmpty.T [] Int -> Filter ->
  ([SigId], [[Val]]) ->
  SignalRecord [] Val
fortissCSVRecord idx filt (ids, hs) =
  Record (S.fromList time) (fmap S.fromList $ Map.fromList ks)
  where ths = zip ids $ Zip.transposeClip $ filt hs
        NonEmpty.Cons (_, time) ks = fmap (ths !!) idx


-- | Main Fortiss CSV Import Function
fortissCSVImport ::
  FilePath ->
  NonEmpty.T [] Int -> Filter ->
  IO (SignalRecord [] Val)
fortissCSVImport path idx filt = do
  text <- readFile path
  let parser =
        csvFileWithHeader (NonEmptyC.repeat (Right . SigId)) CSV.cellContent ';'
  case parse parser path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return $ fortissCSVRecord idx filt table


fortissCSVImportStruct ::
  (CSV.Cells cols) =>
  FilePath -> IO (CSV.Header cols, [cols])
fortissCSVImportStruct path = do
  text <- readFile path
  let parser = CSV.csvFileWithStructure ';'
  case parse parser path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return table
