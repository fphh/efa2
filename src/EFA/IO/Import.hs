module EFA.IO.Import (modelicaCSVImport) where

-- Modelica CSV Import -----------------------------------------------------------------  


{- Modlica CSV - Example:

"time","Sig2","Sig3",
0,1,2,
0,2,4,

-}


import qualified Data.Map as M 

import EFA.Signal.SequenceData (Record(Record), SigId(SigId))

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV

import Text.ParserCombinators.Parsec


csvFile :: Parser [[String]]
csvFile = endBy line eol

line :: Parser [String]
line = sepBy cell (char ',')

cell :: Parser String
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell :: Parser String
quotedCell = 
    do _ <- char '"'
       content <- many quotedChar
       _ <- char '"' <?> "quote at end of cell"
       return content

quotedChar :: Parser Char
quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

modelicaCSVParse :: String -> Either ParseError [[String]] -> Record
modelicaCSVParse _ (Right strs@(("time":_):_)) = makeRecord strs
modelicaCSVParse path (Right []) = error ("Empty csv file: " ++ path)
modelicaCSVParse path (Right _) =
  error ("First column of " ++ path ++ " is not \"time\"")
modelicaCSVParse path (Left err) =
  error ("Parse error in file " ++ show path ++ ": " ++ show err)


makeRecord :: [[String]] -> Record
makeRecord [] = error "This is not possible!"
makeRecord (h:hs) =
  Record (S.fromList time) (M.fromList $ zip sigIdents (map S.fromList sigs))
  where sigIdents = map SigId (tail h)
        time:sigs = SV.transpose (map (map read . init) hs)

-- | Main Modelica CSV Import Function
modelicaCSVImport :: FilePath -> IO Record
modelicaCSVImport path = do 
  text <- readFile path
  return $ modelicaCSVParse path (parseCSV text)

{-
modelicaCSVImport' :: FilePath -> IO Record
modelicaCSVImport' path = do 
  text <- readFile path
  return $ modelicaCSVParse' text

-- | Parse modelica-generated CSV - files with signal logs   
modelicaCSVParse' :: String -> Record
modelicaCSVParse' text = rec
  where 
        -- read get all lines
        csvlines = lines text

        -- header with labels in first line
        header =  csvParseHeaderLine $ head csvlines

        -- first column is "time" / use Rest
        sigIdents = map SigId (tail header)

        -- rest of lines contains data / transpose from columns to lines
        columns = SV.transpose (map csvParseDataLine $ tail csvlines)
        time = if head header == "time"
                  then head columns
                  else error $ "Error in csvImport - first column not time : " ++ (head header)

        -- generate signals from rest of columns
        sigs = tail columns

        -- generate Record with signal Map
        rec = Record (S.fromList time)
                     (M.fromList $ zip sigIdents (map S.fromList sigs))
 
-- | Parse CSV Header Line
csvParseHeaderLine :: String -> [String]  
csvParseHeaderLine line = init $ map read (chop (','==) line)   -- (init . tail) to get rid of Modelica " " quotes 

class ParseCVS a where
      csvParseDataLine :: String -> [a]


instance ParseCVS Double where
         -- | Parse CSV Data Line
         csvParseDataLine line = init $ map read (chop (','==) line)  -- another init to get rid of final , per line

instance Integral int => ParseCVS (Ratio int) where
         csvParseDataLine line = init $ map (realToFrac . flip approxRational (0.001::Rational) . read) (chop (','==) line)
-}