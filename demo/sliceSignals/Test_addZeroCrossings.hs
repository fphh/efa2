{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs, OverlappingInstances, FlexibleContexts, ScopedTypeVariables #-}


import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import  EFA2.Interpreter.Env
import EFA2.Utils.Utils
import EFA2.Signal.Signal

import qualified Data.Map as M 

import EFA2.Signal.Base
import EFA2.Signal.Typ
import EFA2.Signal.Data
import EFA2.Display.Plot

import Data.Monoid
import EFA2.Display.DispSignal


t = sfromList [0,1,2] :: TSigL
p1 = sfromList [-1,1,1] :: PSigL
p2 = sfromList [-1,3,3] :: PSigL
p3 = sfromList [-1,6,-6] :: PSigL

pRec = PowerRecord t (M.fromList [(PPosIdx 0 1,p1),(PPosIdx 1 0, p2),(PPosIdx 1 2, p3)])
pRec0 = addZeroCrossings pRec

main = do
  putStrLn (show pRec)
  putStrLn (show pRec0)
  rPlot "pRec0" pRec0
