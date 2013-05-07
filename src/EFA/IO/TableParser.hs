

module EFA.IO.TableParser (EFA.IO.TableParser.read, write) where

import qualified System.IO as Sys
import Control.Monad (forM_)

import qualified Data.List as L
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

import Control.Applicative
       (liftA, liftA2, (*>), (<*), (<$>), Applicative)

import Prelude as P

import EFA.IO.TableParserTypes (Map, T (..))

read :: FilePath -> IO (Map Double)
read file = readFile file >>= \txt ->
  case parse tables file txt of
       Right tb -> return tb
       Left err -> error (show err)

tables :: (Read a) => Parser (M.Map String (T a))
tables = liftA M.fromList $
  endBy table ((cst $ lookAhead double) <|> commentOrEol <|> eof)

table :: (Read a) => Parser (String, T a)
table = do
  _ <- many commentOrEol
  (str, dim) <- header
  ds <- many line
  return (str, T dim ds)

header :: Parser (String, (Int, Int))
header = liftA2 (,)
  (sp double *> sp tableName)
  (pair <* commentOrEol)

tableName :: Parser String
tableName = many1 $ noneOf "("

double :: Parser ()
double = string "double" >> separator

pair :: Parser (Int, Int)
pair = between (sp $ char '(') (sp $ char ')') $
  liftA2 (,) (sp number) (sp (char ',') *> sp number)

line :: (Read a) => Parser [a]
line = spacesNeol *> sepEndBy1 number spacesNeol <* commentOrEol

commentOrEol :: Parser ()
commentOrEol = spacesNeol >> (eol <|> comment)

comment :: Parser ()
comment = between (char '#') eol (cst $ many neol)

number :: (Read v) => Parser v
number = P.read <$> (many1 $ oneOf "0123456789+-eE.")

sp :: Parser a -> Parser a
sp = (spacesNeol >>)

spacesNeol :: Parser ()
spacesNeol = skipMany separator

separator :: Parser ()
separator = cst $ oneOf "\t "

neol :: Parser ()
neol = cst $ noneOf "\n\r"

eol :: Parser ()
eol = cst $
      try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"

cst :: (Applicative f) => f a -> f ()
cst = liftA (const ())


------------------

writeTable :: Sys.Handle -> (String, T Double) -> IO ()
writeTable hdl (name, T xy ds) = do
  let hd = "#1\ndouble " ++ name ++ show xy
      body = L.intercalate "\n" $ 
               map (L.intercalate " " . map show) ds
  Sys.hPutStr hdl ("\n" ++ hd ++ "\n" ++ body ++ "\n")

write :: FilePath -> Map Double -> IO ()
write file tm = Sys.withFile file Sys.WriteMode $
  forM_ (M.toList tm) . writeTable
