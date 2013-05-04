-- | Modelica CSV Import

module EFA.IO.CSVImport (modelicaCSVImport, fortissCSVImport, filterWith, dontFilter) where

import qualified Data.Map as M
import qualified Data.List as L

import Text.ParserCombinators.Parsec (parse)

import EFA.Signal.Record(Record(Record),SignalRecord, SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import EFA.IO.CSVParser (csvFile)

import EFA.Signal.Base (Val)

import Debug.Trace


makeCSVRecord ::  [[String]] -> SignalRecord [] Val
makeCSVRecord [] = error "This is not possible!"
makeCSVRecord (h:hs) =
  Record (S.fromList time) (M.fromList $ zip sigIdents (map S.fromList sigs))
  where sigIdents = map SigId (tail h)
        time:sigs = SV.transpose (map (map read . init) hs)

-- | Main Modelica CSV Import Function
modelicaCSVImport :: FilePath -> IO (SignalRecord [] Val)
modelicaCSVImport path = do
  let ioerr = ioError . userError
  text <- readFile path
  case parse (csvFile ',') path text of
    Left err -> ioerr $ "Parse error in file " ++ show err
    Right table ->
      case table of
        (("time":_):_) -> return $ makeCSVRecord table
        [] -> ioerr $ "Empty CSV file " ++ show path
        _ -> ioerr $ "First column of " ++ show path ++ " is not \"time\""




filterWith :: Int -> (String -> Bool) -> [[String]] -> [Int]
filterWith r p cs = L.findIndices p (tail (cs !! r))

dontFilter :: [[String]] -> [Int]
dontFilter = filterWith 0 (const True)

fortissCSVRecord ::
  [Int] -> [[String]] ->
  ([[String]] -> [Int]) -> SignalRecord [] Val
fortissCSVRecord _ [] _ = error "This is not possible!"
fortissCSVRecord idx hs filt =
  Record (S.fromList $ getRows time) (M.fromList js)
  where ths = SV.transpose hs
        rowIdx = filt ths
        getRows as = map (read . (as !!)) rowIdx
        (_:time):ks = map (ths !!) idx
        js = map f ks
        f (ti:xs) = (SigId ti, S.fromList $ getRows xs)


-- | Main Fortiss CSV Import Function
fortissCSVImport ::
  FilePath ->
  [Int] ->
  ([[String]] -> [Int]) ->
  IO (SignalRecord [] Val)
fortissCSVImport path idx filt = do
  text <- readFile path
  case parse (csvFile ';') path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table -> return $ fortissCSVRecord idx table filt
