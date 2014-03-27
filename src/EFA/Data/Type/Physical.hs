module EFA.Data.Type.Physical where

import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)


data Power
data Energy
data Eta
data X
data Y
type Power2 = Power

data Type = 
  Power |
  Energy |
  Eta |
  X
  

