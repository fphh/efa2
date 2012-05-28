{-# LANGUAGE FlexibleInstances, TypeOperators, FlexibleContexts #-}

module EFA2.IO.Import (module EFA2.IO.Import) where

import qualified Data.Map as M 

import Data.Ratio

-- import EFA2.Signal.Sequence
-- import EFA2.Interpreter.Arith
import EFA2.Utils.Utils
-- import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData

--import Text.ParserCombinators.Parsec
import Data.List.Split

import EFA2.Signal.Vector
import EFA2.Signal.Signal
import EFA2.Signal.Data
import EFA2.Signal.Base


-- Modelica CSV Import -----------------------------------------------------------------  


{- Modlica CSV - Example:

"time","Sig2","Sig3",
0,1,2,
0,2,4,

-}

-- | Main Modelica CSV Import Function
modelicaCSVImport:: FilePath -> IO Record
modelicaCSVImport path = do 
  text <- readFile path
  return $ modelicaCSVParse text

-- | Parse modelica-generated CSV - files with signal logs   
modelicaCSVParse :: (FromToList (Data ([] :> Nil)) Val ) => String -> Record
modelicaCSVParse text = rec
  where csvlines = lines text -- read get all lines
        header =  csvParseHeaderLine $ head csvlines  -- header with labels in first line       
        sigIdents = map SigId (tail header) -- first column is "time" / use Rest
        columns = (map csvParseDataLine $ tail csvlines) -- rest of lines contains data / transpose from columns to lines
        time = if (head header) == "time" then map head columns else error $ "Error in csvImport - first column not time : " ++ (head header)
        sigs = map tail columns -- generate signals from rest of columns
        rec = Record (sfromList time)  (M.fromList $ zip sigIdents (map sfromList sigs)) -- generate Record with signal Map
        
-- | Parse CSV Header Line
csvParseHeaderLine :: String -> [String]  
csvParseHeaderLine line = init $ map read (splitOn "," line)   -- (init . tail) to get rid of Modelica " " quotes 

class ParseCVS a where
      csvParseDataLine :: String -> [a]


instance ParseCVS Double where
         -- | Parse CSV Data Line
         csvParseDataLine line = init $ map read (splitOn "," line)  -- another init to get rid of final , per line


instance ParseCVS (Ratio Integer) where
         csvParseDataLine line = init $ map (flip approxRational 0.001 . read) (splitOn "," line)

