

module Main where

import EFA.Signal.Base
import EFA.Signal.Data
import Control.Applicative

u :: List [Val]
u = dfromList [[2, 3, 5]]

v :: List [Val]
v = dfromList [[7, 11, 13]]

w :: List [Val]
w = dfromList [[17, 19, 23]]

-- For now, his function looks only on the head of the list of lists...
-- It should be generalised to handle lists of lists of every length.
deltaMult :: List [Val] -> List [Val] -> List [Val]
d1 `deltaMult` d2 = dfromList [d3]
  where l1 = head $ dtoList d1
        l2 = head $ dtoList d2
        h1 = head l1
        t1 = tail l1
        d3 = h1:((*) <$> t1 <*> l2)

-- This seems ok for lists of lists of every length.
deltaAdd :: List [Val] -> List [Val] -> List [Val]
d1 `deltaAdd` d2 = dzipWith (++) d1 d2


main :: IO ()
main = do
  print u
  print v
  print w
  print (u `deltaMult` (v `deltaMult` w))
  print (u `deltaAdd` v)