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
import Data.List 

{-
-- | generate List with MPointIndices from Graph in order to help to generate MPointData Map
genMPointIdxList :: Gr l n -> [MPointIndex]
genMPointIdxList g = sort ((map f (edges g)) ++ (map h (edges g))) 
  where
    f (i1,i2) =  MPointIndex i1 i2
    h (i1,i2) =  MPointIndex i2 i1


-- | Generate Signal Data linked to the Topology using the Record and a Mapping file 
genSigData :: Gr l n -> Mapping -> Record -> SigData
genSigData g (Mapping mapp) (Record time sigs) = SigData {sdTime = time,   
                                                   sdSigs = MPointData (M.fromList (map f  idxList))}  
   where idxList = genMPointIdxList g
         f mpIdx = (mpIdx, h flip signal)
                    where sigIdent = fst (mapp M.! mpIdx) -- get signal Ident from signal Mapping with MPoint Index 
                          signal = sigs M.! sigIdent
                          flip = snd (map M.! mpIdx) -- get sign info
         
         -- correct signal sign                  
         h DontFlip s = s           
         h Flip (Signal vect) = UV.map (0.-.) vect 
 -}

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


  