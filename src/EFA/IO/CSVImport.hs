module EFA.IO.CSVImport (modelicaCSVImport) where

-- | Modelica CSV Import

import qualified Data.Map as M 
import Text.ParserCombinators.Parsec (parse, ParseError)

import EFA.Signal.Record (SignalRecord(SignalRecord), SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import qualified EFA.Signal.Base as SB

import EFA.IO.CSVParser (csvFile)

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse (csvFile ',') "(unknown)" input

modelicaCSVParse :: String -> Either ParseError [[String]] -> SignalRecord [] SB.Val
modelicaCSVParse _ (Right strs@(("time":_):_)) = makeCSVSignalRecord strs
modelicaCSVParse path (Right []) = error ("Empty csv file: " ++ path)
modelicaCSVParse path (Right _) =
  error ("First column of " ++ path ++ " is not \"time\"")
modelicaCSVParse path (Left err) =
  error ("Parse error in file " ++ show path ++ ": " ++ show err)


makeCSVSignalRecord :: [[String]] -> SignalRecord [] SB.Val
makeCSVSignalRecord [] = error "This is not possible!"
makeCSVSignalRecord (h:hs) =
  SignalRecord (S.fromList time) (M.fromList $ zip sigIdents (map S.fromList sigs))
  where sigIdents = map SigId (tail h)
        time:sigs = SV.transpose (map (map read . init) hs)

-- | Main Modelica CSV Import Function
modelicaCSVImport :: FilePath -> IO (SignalRecord [] SB.Val)
modelicaCSVImport path = do 
  text <- readFile path
  return $ modelicaCSVParse path (parseCSV text)

