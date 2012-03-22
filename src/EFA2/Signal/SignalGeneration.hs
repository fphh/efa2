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


{-
-----------------------------------------------------------------------------------
-- Time Powersignal Generation

-- generate Timeline
genTime :: Val -> Val -> Time
genTime dt maxTime = Signal (uvmap f (UV.fromList [0 .. (maxTime/dt)]))
  where
    f timeIndex = TSample (timeIndex*dt)  


-- convert Timeline to Power Conversion
time2Power :: Time -> Power
time2Power time = dmap f time
  where
    f (TSample x) = (PSample x)

  -- Generate zero power signal
genNullPower :: Time -> Power
genNullPower time = dmap f time
  where
    f (TSample x) = (PSample 0)

  -- Generate one Power Signal
genOnePower :: Time -> Power
genOnePower time = dmap f time
  where
    f (TSample x) = (PSample 1)

list2Power :: [Val] -> Power 
list2Power list = dfromList (map toSample list) 

list2Time :: [Val] -> Time 
list2Time list = dfromList (map toSample list) 


--- Generation specific functions

sine :: Val -> Val -> Power -> Power
sine  w phi time = dmap f time where f x = sin (w.*.x.+.phi)

etaFunct :: Val -> Power -> Power
etaFunct eta sig = dmap f sig
  where f val | val > 0  = val.*.eta
        f val | val == 0 = 0   
        f val | val < 0  = val./.eta  






-}