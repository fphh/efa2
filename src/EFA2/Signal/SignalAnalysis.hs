{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module EFA2.Signal.SignalAnalysis where

import Control.Monad
import Data.List (zip5,zipWith4)

import qualified Data.Map as M
import qualified Data.Bimap as BM
import qualified Data.Foldable as F

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as GV

import Data.Either

import Debug.Trace

-- import EFA2.Graph.GraphData

import EFA2.Signal.SignalData
-- import EFA2.Signal.Sequence

-- import EFA2.Signal.SignalGraph

import EFA2.Utils.Utils


data Sign = PSign | ZSign | NSign deriving (Show, Eq)

-- Generische Funktion für Daten - Container
absd :: Sample a => Val -> Val -> Signal a -> Signal a
absd  w phi time = dmap f time where f x = abs x


-- check for NaN's 
sampleCheck :: (DataAll cont a) => cont a -> Bool     
sampleCheck d = dall (not . isNaN) d


-- check for same vector length
equalLengths :: (Sample a, Data (cont a) a,(DataLength cont a)) => [cont a] -> Bool
equalLengths list | length list == 0 = True
equalLengths xs = and (map (== n) ns)
  where (n:ns) = map dlength xs
        

-- determine Signal Sign  
sign :: (Eq a, Ord a, Num a) => a -> Sign
sign x | x > 0 = PSign
       | x == 0 = ZSign -- TODO add intervalls later on Zero - Detection       
       | x < 0 = NSign
  
              