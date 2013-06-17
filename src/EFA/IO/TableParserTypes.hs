

module EFA.IO.TableParserTypes where

import qualified Data.Map as Map

data T a = T (Int, Int) [[a]] deriving (Show)

type Map a = Map.Map String (T a)


