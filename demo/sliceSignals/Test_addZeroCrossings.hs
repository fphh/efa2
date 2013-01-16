{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs, OverlappingInstances, FlexibleContexts, ScopedTypeVariables #-}


import EFA.Equation.Env

import EFA.Signal.Sequence
import EFA.Signal.SequenceData
import EFA.Signal.Signal as S
import EFA.Signal.Base
import EFA.Signal.Typ
import EFA.Signal.Data
import EFA.Signal.Plot

import EFA.Utility

import qualified Data.Map as M

import Data.Monoid


t = S.fromList [0,1,2] :: TSigL
p1 = S.fromList [-1,1,1] :: PSigL
p2 = S.fromList [-1,3,3] :: PSigL
p3 = S.fromList [-1,6,-6] :: PSigL

pRec = PowerRecord t (M.fromList [(PPosIdx 0 1,p1),(PPosIdx 1 0, p2),(PPosIdx 1 2, p3)])
pRec0 = addZeroCrossings pRec

main = do
  putStrLn (show pRec)
  putStrLn (show pRec0)
  rPlot ("pRec0",pRec0)
