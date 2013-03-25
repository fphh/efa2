-- | Modelica CSV Import

module EFA.IO.CSVImport (modelicaCSVImport) where

import qualified Data.Map as M 
import Text.ParserCombinators.Parsec (parse)

import EFA.Signal.Record(Record(Record),SignalRecord, SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import EFA.IO.CSVParser (csvFile)

import EFA.Signal.Base (Val)


makeCSVRecord ::  [[String]] -> SignalRecord [] Val
makeCSVRecord [] = error "This is not possible!"
makeCSVRecord (h:hs) =
  Record (S.fromList time) (M.fromList $ zip sigIdents (map S.fromList sigs))
  where sigIdents = map SigId (tail h)
        time:sigs = SV.transpose (map (map read . init) hs)

-- | Main Modelica CSV Import Function
modelicaCSVImport ::   FilePath -> IO (SignalRecord [] Val)
modelicaCSVImport path = do 
  text <- readFile path
  case parse (csvFile ',') path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table ->
      case table of
        (("time":_):_) -> return $ makeCSVRecord table
        [] -> ioError $ userError $ "Empty CSV file " ++ show path
        _ -> ioError $ userError $ "First column of " ++ show path ++ " is not \"time\""
