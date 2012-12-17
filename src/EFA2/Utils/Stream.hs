module EFA2.Utils.Stream where

import Prelude (Show, Enum, succ)

{- |
Duplicate of "Data.Stream" with infix constructor.
We need this for simple pattern match on prefixes of a Stream.
-}
data Stream a = a :~ Stream a
   deriving (Show)

infixr 5 :~

enumFrom :: Enum a => a -> Stream a
enumFrom x = x :~ enumFrom (succ x)
