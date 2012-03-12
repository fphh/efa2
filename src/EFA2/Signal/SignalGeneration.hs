{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts, NoMonomorphismRestriction, FunctionalDependencies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graph.CGraph
-- Copyright   :  (c) Dr. Heinrich Hördegen
-- 
-- Maintainer  : hoerdegen@funktional.info
--
-----------------------------------------------------------------------------


module EFA2.Signal.SignalGeneration where


import qualified Data.List as L

import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV

import System.Random

import EFA2.Signal.SignalData
import EFA2.Utils.Utils



-----------------------------------------------------------------------------------
-- Time Signal Generation

-- generate Timeline
genTime :: Val -> Val -> Time
genTime dt maxTime = Time ((UV.fromList [0 .. (maxTime/dt)]).*dt)

-- Time Signal Calculations 
sine :: Val -> Val -> Time -> TimeSignal
sine  w phi (Time vect) = PTSig (UV.map f vect)
  where
    f t = sin (w*t+phi)

-- Generate Zero Power Signal
nullPSig :: Time -> TimeSignal
nullPSig  (Time t) = PTSig t*0


etaFunct :: Double -> TimeSignal -> TimeSignal
etaFunct eta sig = smap f sig
  where f val | val > 0  = val*eta
        f val | val == 0 = 0   
        f val | val < 0  = val/eta  
