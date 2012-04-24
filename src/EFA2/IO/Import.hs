module EFA2.IO.Import (module EFA2.IO.Import) where

import EFA2.Signal.Sequence
import EFA2.Interpreter.Arith
import EFA2.Utils.Utils
import EFA2.Signal.Sequence

import qualified Data.Map as M 
--import Text.ParserCombinators.Parsec
import Data.List.Split



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
-- TODO -- check whether Modelica always doubles the first and last time !!!  
modelicaCSVParse :: String -> Record
modelicaCSVParse text = rec
  where csvlines = lines text -- read get all lines
        header =  csvParseHeaderLine $ head csvlines  -- header with labels in first line       
        sigIdents = map SigId (tail header) -- first column is "time" / use Rest
        -- TODO improve Quick-Fix: cut away first and last csvLine with head and init 
        columns = transpose (map csvParseDataLine $ tail csvlines) -- rest of lines contains data / transpose from columns to lines
        time = if (head header) == "time" then head columns else error $ "Error in csvImport - first column not time : " ++ (head header)
        sigs = tail columns -- generate signals from rest of columns
        rec = Record time  (M.fromList $ zip sigIdents sigs) -- generate Record with signal Map
        
-- | Parse CSV Header Line
csvParseHeaderLine :: String -> [String]  
csvParseHeaderLine line = init $ map read (splitOn "," line)   -- (init . tail) to get rid of Modelica " " quotes 
  
-- | Parse CSV Data Line
csvParseDataLine :: String -> [Val]  
csvParseDataLine line = init $ map read (splitOn "," line)  -- another init to get rid of final , per line

