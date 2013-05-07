

module EFA.IO.TableParserTypes where

import qualified Data.Map as M

data T a = T (Int, Int) [[a]] deriving (Show)

type Map a = M.Map String (T a)


