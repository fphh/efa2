module EFA.IO.CSVImport (modelicaCSVImport) where

-- | Modelica CSV Import

import qualified Data.Map as M 
import Text.ParserCombinators.Parsec (parse)

import EFA.Signal.SequenceData (Record(Record), SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import EFA.IO.CSVParser (csvFile)



makeCSVRecord :: [[String]] -> Record
makeCSVRecord [] = error "This is not possible!"
makeCSVRecord (h:hs) =
  Record (S.fromList time) (M.fromList $ zip sigIdents (map S.fromList sigs))
  where sigIdents = map SigId (tail h)
        time:sigs = SV.transpose (map (map read . init) hs)

-- | Main Modelica CSV Import Function
modelicaCSVImport :: FilePath -> IO Record
modelicaCSVImport path = do 
  text <- readFile path
  case parse (csvFile ' ') path text of
    Left err ->
      ioError $ userError $ "Parse error in file " ++ show err
    Right table ->
      case table of
        (("time":_):_) -> return $ makeCSVRecord table
        [] -> ioError $ userError $ "Empty CSV file " ++ show path
        _ -> ioError $ userError $ "First column of " ++ show path ++ " is not \"time\""
