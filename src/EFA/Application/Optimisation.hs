{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Application.Optimisation where

import qualified EFA.Application.Utility as EqUt
import qualified EFA.Application.Absolute as EqGen
import qualified EFA.Application.EtaSys as ES
import qualified EFA.Application.Index as XIdx
import EFA.Application.Absolute ( (.=), (=.=) )

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Data as Data
import EFA.Signal.Signal (TC,Scalar)
import EFA.Signal.Data (Data(..), Nil, (:>), getData)
import EFA.Signal.Typ (Typ, F, T, A, Tt, UT)

import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Node as Node


import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Arithmetic as EqArith
import EFA.Equation.Result (Result(..))

import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import qualified Data.Vector as V
import Control.Applicative (liftA2)
import Data.Map (Map)

-- | Map a two dimensional load room (varX, varY) and find per load situation 
-- | the optimal solution in the 2d-solution room (two degrees of freevarOptX varOptY) 
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

calcOptfunc muss verallgemeinert werden. 

1. ein socDrive pro Speicher
2. wie kann ich automatisch alle Speicherinhalte kontrollieren
3. wie kriege ich das Vorzeichen (ein/ausspeichern ??)
4. undetermined / determined soll ich das weiterhin übernehmen oder was eigenes machen
5. 

calcOptFunc ::
  Flow.RangeGraph Node ->
  Bool ->
  Double ->
  EnvDouble -> Result Double
calcOptFunc topo b socDrive env = case  ES.etaSys topo env of 
   Determined etaSys -> etaSys + socDrive * (if b then eCharge else -eDischarge)
   Undetermined -> 
  
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

-- | Function to specifiy that an efficiency function in etaAssign is to be looked up with input power
etaOverPowerIn :: XIdx.Eta node -> XIdx.Power node
etaOverPowerIn =
   TIdx.liftInSection $ \(TIdx.Eta e) -> TIdx.Power e

-- | Function to specifiy that an efficiency function in etaAssign is to be looked up with output power
etaOverPowerOut :: XIdx.Eta node -> XIdx.Power node
etaOverPowerOut =
   TIdx.liftInSection $ \(TIdx.Eta e) -> TIdx.Power $ TIdx.flip e


type EtaAssignMap node =
        Map (XIdx.Eta node) (String, String, XIdx.Eta node -> XIdx.Power node)


-- | Generate given equations using efficiency curves or functions for a specified section 
makeEtaFuncGiven ::
   (Fractional a, Ord a, Show a, EqArith.Sum a,
    Data.Apply c a ~ v, Eq v, Data.ZipWith c, Data.Storage c a, Node.C node) =>
   (TIdx.Section -> EtaAssignMap node) ->
   TIdx.Section ->
   Map String (a -> a) ->
   EqGen.EquationSystem node s x (Data c a)

makeEtaFuncGiven etaAssign sec etaFunc = Fold.fold $ Map.mapWithKey f (etaAssign sec)
  where f n (strP, strN, g) =
          EqGen.variable n =.= EqGen.liftF (Data.map ef) (EqGen.variable $ g n)
          where ef x = if x >= 0 then fpos x else fneg x
                fpos = maybe (err strP) id (Map.lookup strP etaFunc)
                fneg = maybe (err strN) (\h -> recip . h . negate)
                                        (Map.lookup strN etaFunc)
                err str x = error ("not defined: " ++ show str ++ " for " ++ show x)

