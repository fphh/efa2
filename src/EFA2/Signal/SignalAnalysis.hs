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
import EFA2.Signal.SplitSignal

-- import EFA2.Signal.SignalGraph

import EFA2.Utils.Utils



psampleDTime :: UV.Vector Time -> UV.Vector DTime
psampleDTime time = UV.map (DTime . unTime) time

esampleDTime :: UV.Vector Time -> UV.Vector DTime
esampleDTime time = UV.map (DTime . unTime) $ UV.zipWith (-) (UV.tail time) time

class Integrate key a b where
      integrate :: SectionRecord (Section a) -> SectionRecord (IntSection key b)


instance Integrate PartialIntegration PSample PSample where
         integrate secs = res
           where res = fmap f secs
                 f (Section sid len _ time vecs) = IntSection sid len Nothing (psampleDTime time) vecs

instance Integrate FullIntegration PSample PSample where
         integrate secs = res
           where res = fmap f secs
                 f (Section sid len _ time vecs) = IntSection sid 1 Nothing (psampleDTime time) (map condense vecs)
                 condense = (fmap (UV.singleton . UV.sum))

instance Integrate PartialIntegration PSample ESample where
         integrate secs = res
           where res = fmap f secs
                 f (Section sid len _ time vecs) = IntSection sid (len-1) Nothing dtime res
                   where dtime = esampleDTime time
                         res = map (pSample2ESample dtime) vecs

instance Integrate FullIntegration PSample ESample where
         integrate secs = res
           where res = fmap f secs
                 f (Section sid len _ time vecs) = IntSection sid 1 Nothing dtimeacc res
                   where dtime = esampleDTime time
                         dtimeacc = UV.singleton (UV.sum dtime)
                         res = map (condense . pSample2ESample dtime) vecs
                         condense = (fmap (UV.singleton . UV.sum))


pSample2ESample :: UV.Vector DTime -> (SignalIdent, UV.Vector PSample) -> (SignalIdent, UV.Vector ESample)
pSample2ESample dtime (ident, power) = (ident, UV.zipWith3 f dtime (UV.init power) (UV.tail power))
  where f dt psi pst = toSample ((pt + pi) / 2*dt')
          where pt = fromSample pst
                pi = fromSample psi
                dt' = unDTime dt

    

step1 :: Integrate key a b => SectionRecord (Section a) -> SectionRecord (IntSection key b)
step1 = integrate

step2 :: (Ord a, SameUnit a b, UV.Unbox a, UV.Unbox b, Fractional a) 
         => TheGraph -> SectionRecord (IntSection key a) -> SectionRecord (EtaSection key a b)
step2 theGraph@(TheGraph topo maps tdata) rec = fmap (mkEtaSection theGraph) rec

mkEtaSection :: (Ord a, FromSample a, UV.Unbox a, Fractional a, SameUnit a b, UV.Unbox b) 
                => TheGraph -> IntSection key a -> EtaSection key a b
mkEtaSection theGraph@(TheGraph topo maps tdata) (IntSection sid len _ dtime vecs) =
  EtaSection sid len stateErr etaErr state inpercents outpercents vecs' etas
  where sigf = signalEnv maps
        etaErr = Nothing
        vecs' = attachSignals topo sigf (M.fromList vecs)
        state = calculateStateIndex theGraph vecs'
        etavecs idx = calculateEtas len tdata idx vecs'
        inperc idx = calculateInPercents (stateMaps tdata BM.! idx) vecs' topo
        outperc idx = calculateOutPercents (stateMaps tdata BM.! idx) vecs' topo
        (stateErr, etas, inpercents, outpercents) = f state
        f Nothing = (Just ImpossibleState, M.empty, M.empty, M.empty)
        f (Just idx) = (Nothing, etavecs idx, inperc idx, outperc idx)


step3 :: (Ord a, SameUnit a b, UV.Unbox a, UV.Unbox b, Fractional a, Show b, Show a) 
         => TheGraph -> IdentMap a -> SectionRecord (EtaSection key a b) -> SectionRecord (StorageSection key a b)
step3 theGraph m secs = SectionRecord $ reverse $ F.foldl (mkStSection theGraph m) [] secs

mkStSection :: (UV.Unbox a, Num a, Show a, Show b) => TheGraph -> IdentMap a -> [StorageSection key a b] -> EtaSection key a b -> [StorageSection key a b]
mkStSection theGraph m [] (EtaSection sid len serr eerr st@(Just idx) inperc outperc vecs etas) = [StorageSection sid len serr eerr st inperc outperc vecs etas stvec]
  where stvec = mkStorageVector theGraph idx (M.map UV.singleton m) vecs
mkStSection theGraph _ (s:ss) (EtaSection sid len serr eerr st@(Just idx) inperc outperc vecs etas) = (StorageSection sid len serr eerr st inperc outperc vecs etas stvec):s:ss
  where stvec = mkStorageVector theGraph idx (ssecStorage s) vecs

recordToEtaRecord :: (SameUnit a b, UV.Unbox b, UV.Unbox a, Ord a, Integrate key PSample a, Fractional a, Show b, Show a)
                     => TheGraph -> Start key a -> SectionRecord (StorageSection key a b)
recordToEtaRecord theGraph (Start rec stMap) = s3 
  where r = recordToSectionRecord rec
        s1 = step1 r
        s2 = step2 theGraph s1
        s3 = step3 theGraph stMap s2
