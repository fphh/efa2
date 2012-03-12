{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}

module EFA2.Signal.SignalAnalysis where

import Control.Monad
import Data.List (zip5,zipWith4)

import qualified Data.Map as M
import qualified Data.Bimap as BM
import qualified Data.Foldable as F

import qualified Data.Vector.Unboxed as UV
import Data.Either

import Debug.Trace

-- import EFA2.Graph.GraphData

import EFA2.Signal.SignalData
import EFA2.Signal.Sequence

-- import EFA2.Signal.SignalGraph

import EFA2.Utils.Utils




-- check for NaN's of Infs in original Data
sampleCheck :: PSample -> Bool     
sampleCheck s = not (isNaN s)

-- check for same vector length
equalLengths :: (UV.Unbox a) => [UV.Vector a] -> Bool
equalLengths vec | length vec == 0 = True
equalLengths xs = and (map (== n) ns)
  where (n:ns) = map UV.length xs

 