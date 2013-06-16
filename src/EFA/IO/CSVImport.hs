-- | Modelica CSV Import

module EFA.IO.CSVImport (modelicaCSVImport, fortissCSVImport, filterWith, dontFilter) where

import qualified EFA.IO.Parser as EFAParser
import EFA.IO.CSVParser (csvFile, csvFileWithHeader)
import Text.ParserCombinators.Parsec (Parser, parse)

import EFA.Signal.Record(Record(Record),SignalRecord, SigId(SigId))
import EFA.Signal.Base (Val)

import qualified EFA.Signal.Signal as S

import qualified Data.Traversable as Trav
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Zip as Zip
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Monad (when)


type Line = NonEmpty.T (NonEmpty.T []) String

makeCSVRecord ::
  (Line, [Line]) ->
  Parser (SignalRecord [] Val)
makeCSVRecord (NonEmpty.Cons timeStr sigNames, hs) = do
  when (timeStr /= "time") $
    fail $ "First column should be \"time\", but is " ++ show timeStr
  NonEmpty.Cons time sigs <-
    Trav.mapM (Trav.mapM EFAParser.cellContent) $
    Zip.transposeClip $ fmap NonEmpty.init hs
  return $
    Record
      (S.fromList time)
      (Map.fromList $
       zip
         (map SigId $ NonEmpty.init sigNames)
         (map S.fromList sigs))

-- | Main Modelica CSV Import Function
modelicaCSVImport :: FilePath -> IO (SignalRecord [] Val)
modelicaCSVImport path = do
  text <- readFile path
  case parse (makeCSVRecord =<< csvFileWithHeader ',') path text of
    Left err -> ioError . userError $ "Parse error in file " ++ show err
    Right table -> return table




filterWith :: Int -> (String -> Bool) -> [[String]] -> [Int]
filterWith r p cs = List.findIndices p (tail (cs !! r))

dontFilter :: [[String]] -> [Int]
dontFilter = filterWith 0 (const True)

fortissCSVRecord ::
  NonEmpty.T [] Int -> ([NonEmpty.T [] String] -> [Int]) ->
  NonEmpty.T [] [String] ->
  SignalRecord [] Val
fortissCSVRecord idx filt hs =
  Record (S.fromList $ getRows time) (Map.fromList js)
  where ths = Zip.transposeClip hs
        rowIdx = filt ths
        getRows as = map (read . (as !!)) rowIdx
        NonEmpty.Cons (NonEmpty.Cons _ time) ks = fmap (ths !!) idx
        js = map f ks
        f (NonEmpty.Cons ti xs) = (SigId ti, S.fromList $ getRows xs)


-- | Main Fortiss CSV Import Function
fortissCSVImport ::
  FilePath ->
  NonEmpty.T [] Int ->
  ([NonEmpty.T [] String] -> [Int]) ->
  IO (SignalRecord [] Val)
fortissCSVImport path idx filt = do
  text <- readFile path
  case parse (fmap (fortissCSVRecord idx filt) $ csvFile ';') path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return table
