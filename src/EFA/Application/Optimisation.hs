{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Application.Optimisation where 

import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Signal (TC,Scalar)
import EFA.Signal.Data (Data(..), Nil, (:>), getData)
import EFA.Signal.Typ (Typ, F, T, A, Tt, UT)
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Application.EtaSys as ES
import qualified EFA.Application.Index as XIdx
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Data as Data
import EFA.Equation.Result (Result(..))
import qualified EFA.Application.Utility as EqUt
import qualified EFA.Equation.Environment as EqEnv

import qualified Data.Vector as V
import Control.Applicative (liftA2)

-- | 
doubleSweep :: (SV.Zipper v2,
                SV.Zipper v1,
                SV.Walker v2,
                SV.Walker v1,
                SV.Storage v2 (v1 a),
                SV.Storage v1 a,
                SV.Storage v2 (v1 b),
                SV.Storage v1 b,
                SV.FromList v2,
                SV.Convert v1 v1,
                SV.Storage v2 (v1 (TC (Sig.Arith s s) (Typ UT UT UT) (Data (v2 :> (v1 :> Nil)) b))),
                SV.Storage v1 (TC (Sig.Arith s s) (Typ UT UT UT) (Data (v2 :> (v1 :> Nil)) b)),
               Sig.Arith s s ~ Sig.Signal) =>
               (a -> a -> a -> a -> b) -> 
               TC s typ (Data (v2 :> v1 :> Nil) a) -> 
               TC s typ (Data (v2 :> v1 :> Nil) a) -> 
               TC s typ (Data (v2 :> v1 :> Nil) a) -> 
               TC s typ (Data (v2 :> v1 :> Nil) a) -> 
               Sig.UTSignal2 v2  v1 (Sig.UTSignal2 v2  v1 b) 
               
doubleSweep fsolve varOptX varOptY varX varY =
  Sig.untype $ Sig.zipWith f varX  varY
  where f x y =
          Sig.untype $ Sig.convert $ Sig.zipWith (fsolve x y) varOptX varOptY

{-
calcOptFunc ::
  Flow.RangeGraph Node ->
  Bool ->
  Double ->
  EnvDouble -> Double
calcOptFunc topo b socDrive env =
  if all (>0) [eCoal0, eCoal1, eTrans0, eTrans1] then res else nan
  where nan = 0/0
        lu idx = EqUt.checkDetermined (show idx) $
                   ES.lookupAbsEnergy "calcOptFunc" env idx
        eCoal      = lu $ XIdx.energy sec0 Coal Network
        eCoal0     = lu $ XIdx.energy sec0 Coal Network
        eCoal1     = lu $ XIdx.energy sec1 Coal Network
        eTrans0    = lu $ XIdx.energy sec0 Network LocalNetwork
        eTrans1    = lu $ XIdx.energy sec1 Network LocalNetwork

        eCharge    = lu $ XIdx.energy sec0 Water Network
        eDischarge = lu $ XIdx.energy sec1 Water Network

        Determined etaSys = ES.etaSys topo env
        res = etaSys + socDrive * (if b then eCharge else -eDischarge)


maxEta ::
  Flow.RangeGraph Node ->
  Sig.UTSignal2 V.Vector V.Vector EnvDouble ->
  (Double, Maybe EnvDouble)
maxEta topo sigEnvs = maxOpt topo True 0 sigEnvs



maxOpt ::
  Flow.RangeGraph Node ->
  Bool ->
  Double ->
  Sig.UTSignal2 V.Vector V.Vector EnvDouble ->
  (Double, Maybe EnvDouble)
maxOpt topo b socDrive sigEnvs = (etaMax, env)
  where etaSys = Sig.map (calcOptFunc topo b socDrive) sigEnvs
        etaMax = Sig.fromScalar $ Sig.maximum etaSys
        (xIdx, yIdx) = Sig.findIndex2 (== etaMax) etaSys
        env = liftA2 (Sig.getSample2D sigEnvs) xIdx yIdx

-}

combineOptimalMaps ::
  Sig.UTSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double
combineOptimalMaps state charge discharge =
  Sig.zipWith f state $ Sig.zip charge discharge
  where f s (c, d) = if s < 0.1 then c else d

envGetData ::
  (Ord node) =>
  EqEnv.Complete node (Result (Data va a)) (Result (Data vv v)) ->
  EqEnv.Complete node (Result (Data.Apply va a)) (Result (Data.Apply vv v))
envGetData =
  EqEnv.completeFMap (fmap getData) (fmap getData)



-- | 

noflip :: TIdx.InSection TIdx.Eta node -> TIdx.InSection TIdx.Power node
noflip (TIdx.InSection sec ( TIdx.Eta (TIdx.StructureEdge n1 n2))) = 
  TIdx.InSection sec (TIdx.Power (TIdx.StructureEdge n1 n2))


myflip :: TIdx.InSection TIdx.Eta node -> TIdx.InSection TIdx.Power node
myflip (TIdx.InSection sec ( TIdx.Eta (TIdx.StructureEdge n1 n2))) = 
  TIdx.InSection sec (TIdx.Power (TIdx.StructureEdge n2 n1))
