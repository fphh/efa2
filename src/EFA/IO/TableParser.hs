

module EFA.IO.TableParser where

import qualified Data.Map as M

import Text.ParserCombinators.Parsec

import Control.Applicative
       (liftA, liftA2, (*>), (<*), (<$>), Applicative)

import Prelude as P

data T a = T (Int, Int) [[a]] deriving (Show)

type Map a = M.Map String (T a)

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
