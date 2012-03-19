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


-- -- | generate List with MPointIndices from Graph in order to help to generate MPointData Map
-- genMPointIdxList :: Gr l n -> [MPointIdx]
-- genMPointIdxList :: g = sort (map MPointIdx (edges g) ++ map (MPointIdx . mPointIndexFlip) (edges g)) 


-- -- | Generate Signal Data linked to the Topology using the Record and a Mapping file 
-- genSigData :: Gr l n -> Mapping -> Record -> SigData
-- genSigData g mapping (time, sigs) = SigData {sdTime = time,   
--                                              sdSigs = M.toList (map f  idxList)  
--   where idxList = genMPointIdxList g
--         f idx = M.!
  

-- genFlowRecord :: Record -> FSigRecord
-- genFlowRecord (time, sigs) = (genDTime time) (M.map pSample2ESample  

-- -- function to calculate flow Values 
-- genFlowSigs ::  Gr l n -> Mapping -> Record -> MPointData (FSignal ESample)
-- genFlowSigs g record = pSample2ESample dt record

-- -- function to calculate Efficiency Values 
-- makeEtaEnv :: (Sample a, Eta b, UV.Unbox a, UV.Unbox b, SameUnit a b) => Gr l n -> PPosData a -> EtaData b
-- makeEtaEnv g samples = M.fromList etaList
--    where ns = labEdges g
--          etaList = map f ns
--          f (x, y, _) = (mkIdx x y, UV.map toEta $ (spl y x) ./ (spl x y))
--          spl x y = UV.map fromSample $ samples M.! mkIdx x y


  