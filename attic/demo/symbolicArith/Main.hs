

module Main where

import EFA.Signal.Base
import EFA.Signal.Data
import EFA.Signal.Vector

-- The following arithmetic should be implemented for InTerm.

-- Example terms...
data SimpleTerm = Const Int
                | Add SimpleTerm SimpleTerm
                | Mult SimpleTerm SimpleTerm deriving (Show)

one :: SimpleTerm
one = Const 1

two :: SimpleTerm
two = Const 2

three :: SimpleTerm
three = Const 3

four :: SimpleTerm
four = Const 4

u :: List SimpleTerm
u = dfromList [one, two]

v :: List SimpleTerm
v = dfromList [two, three]

s :: DVal SimpleTerm
s = Data (D0 four)


main :: IO ()
main = do
  print (dzipWith Add u v)
  print (dzipWith Mult u v)
  print (dzipWith Add s u)
