module EFA.IO.Parser where

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec (Parser)

import qualified Data.NonEmpty as NonEmpty

import Control.Applicative (Applicative, liftA2, (<*))


number :: (Read v) => Parser v
number = do
   str <- Parsec.many1 $ Parsec.oneOf "0123456789+-eE."
   case reads str of
      [(n,"")] -> return n
      _ -> fail $ "invalid number string: " ++ show str

cellContent ::
  (Read a) =>
  String -> Parser a
cellContent str =
  case reads str of
    [(a,"")] -> return a
    _ -> fail $ "could not parse cell content: " ++ show str


class Sequence f where
   sepBy :: Parser a -> Parser sep -> Parser (f a)
   many :: Parser a -> Parser (f a)

instance Sequence [] where
   sepBy = Parsec.sepBy
   many = Parsec.many

instance Sequence f => Sequence (NonEmpty.T f) where
   sepBy p sep = liftA2 NonEmpty.Cons p $ many (sep >> p)
   many p = liftA2 NonEmpty.Cons p $ many p

endBy :: Sequence f => Parser a -> Parser sep -> Parser (f a)
endBy p sep = many (p <* sep)
