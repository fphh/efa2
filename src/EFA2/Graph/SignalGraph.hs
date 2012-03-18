{-# LANGUAGE RankNTypes #-}

module EFA2.Graph.SignalGraph where

import qualified Data.Map as M

import Data.Graph.Inductive
import qualified Data.Vector.Unboxed as UV

import EFA2.Signal.SignalData
import EFA2.Signal.SignalGeneration

import EFA2.Graph.GraphData
import EFA2.Signal.SignalAnalysis

import EFA2.Signal.Sequence



-- -- function 
-- makeFlowEnv ::  Gr l n -> SignalMap -> Record -> PPosData ESample
-- makeFlowEnv g record = pSample2ESample dt sigs



-- makeEtaEnv :: (Sample a, Eta b, UV.Unbox a, UV.Unbox b, SameUnit a b) => Gr l n -> PPosData a -> EtaData b
-- makeEtaEnv g samples = M.fromList etaList
--   where ns = labEdges g
--         etaList = map f ns
--         f (x, y, _) = (mkIdx x y, UV.map toEta $ (spl y x) ./ (spl x y))
--         spl x y = UV.map fromSample $ samples M.! mkIdx x y


  