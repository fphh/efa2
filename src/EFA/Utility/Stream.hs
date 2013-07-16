module EFA.Utility.Stream where

import Prelude (Functor, fmap, Show, Enum, succ)

{- |
Duplicate of "Data.Stream" with infix constructor.
We need this for simple pattern match on prefixes of a Stream.
-}
data Stream a = a :~ Stream a
   deriving (Show)

instance Functor Stream where
   fmap f =
      let go (x :~ xs) = f x :~ go xs
      in  go


infixr 5 :~

enumFrom :: Enum a => a -> Stream a
enumFrom x = x :~ enumFrom (succ x)
