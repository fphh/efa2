module EFA.IO.Parser where

import qualified Text.ParserCombinators.Parsec as Parsec


number :: (Read v) => Parsec.Parser v
number = do
   str <- Parsec.many1 $ Parsec.oneOf "0123456789+-eE."
   case reads str of
      [(n,"")] -> return n
      _ -> fail $ "invalid number string: " ++ show str
