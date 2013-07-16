{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Modules.Optimisation where

import qualified Modules.System as System
import Modules.System (Node(..))

import qualified EFA.Example.Absolute as EqGen
import qualified EFA.Example.Index as XIdx
--import qualified EFA.Example.Utility as EqUt
--import qualified EFA.Example.EtaSys as ES
import EFA.Example.Absolute ( (.=), (=%%=), (=.=) )

import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Arithmetic as EqArith
import EFA.Equation.Result (Result(..))

import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Graph.Topology.Node as TDNode
import qualified EFA.Graph.Topology as TD

--import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Typ(UT,Typ)
import EFA.Signal.Signal(TC(..))
import qualified EFA.Signal.Data as Data
--import qualified EFA.Signal.Base as Base
import qualified EFA.Signal.Vector as SV

import EFA.Signal.Data (Data(..), 
                        Nil, 
                        (:>))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified Data.Map as Map ; import Data.Map (Map)
-- import qualified Data.Vector as V
import qualified Data.Foldable as Fold
import Data.Monoid (mconcat, (<>))



--import Control.Applicative (liftA2)

sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0

type EtaAssignMap =
        Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)
        
type Env a = EqEnv.Complete Node (Data Nil a) (Data Nil a)        
type EnvResultData a = EqEnv.Complete Node (Result (Data Nil a)) (Result (Data Nil a))
type EnvResult a = EqEnv.Complete Node (Result a) (Result a)

type EqSystemData a =  (forall s. EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a))


type SolveFunc a =
--  (TIdx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  EqEnv.Complete
    Node
    (Result (Data Nil a))
    (Result (Data Nil a))

eqsys ::
         (a ~ EqArith.Scalar v, Eq a, Eq v,
          EqArith.Constant a, EqArith.Product v, EqArith.Integrate v) =>
         EqGen.EquationSystem Node s a v
eqsys = EqGen.fromGraph True (TD.dirFromSequFlowGraph (snd System.seqTopology))


solve' ::
  (Ord a, Fractional a, Show a, EqArith.Sum a,EqArith.Constant a) => 
  Flow.RangeGraph Node ->
  Env a ->
  TIdx.Section ->
  Map String (a -> a) ->
  a ->
  a ->
  a ->
  a ->
  EnvResult a
solve' seqTopology env sec etaFunc pHouse pNetload pWater pBattery = envGetData $ solve seqTopology env sec etaFunc (g pHouse) (g pNetload) (g pWater) (g pBattery)  
  where g = Data 


solve ::
  (Ord a, Fractional a, Show a, EqArith.Sum a,EqArith.Constant a) => 
  Flow.RangeGraph Node ->
  Env a ->
  TIdx.Section ->
  Map String (a -> a) ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  EnvResultData a
solve seqTopology env sec etaFunc pHouse pNetload pWater pBattery = 
  EqGen.solveSimple $
    EqGen.fromGraph True (TD.dirFromSequFlowGraph (snd seqTopology)) <>
    given env sec etaFunc pHouse pNetload pWater pBattery

given ::
  (Ord a, Fractional a, Show a, EqArith.Sum a) =>
  Env a->
  TIdx.Section ->
--  (TIdx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  EqSystemData a

given env sec etaFunc pHouse pNetload pWater pBattery =
   (((commonGiven <> etaGiven sec etaFunc) <>
   givenAverage env) <>) $
   mconcat $
   -- Actual Section 0 Charging to be varied and optimised
   (XIdx.power sec Hausnetz Verteiler .= pHouse) :
   (XIdx.power sec Netzlast Netz .= pNetload) :
   (XIdx.power sec Netz Wasser .= pWater) :
   (XIdx.power sec Verteiler Batterie .= pBattery) :
   []

commonGiven ::
  (EqArith.Sum a, Num a, Eq a) =>
  EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a)
commonGiven =
   mconcat $
   (XIdx.dTime sec0 .= Data 1) :
   (XIdx.dTime sec1 .= Data 1) :
   (XIdx.storage TIdx.initial Wasser .= Data 0) :
   (XIdx.energy sec0 Wasser Netz =%%= XIdx.energy sec1 Wasser Netz) :
   []

etaGiven ::
   (Fractional a, Ord a, Show a, EqArith.Sum a,
    Data.Apply c a ~ v, Eq v, Data.ZipWith c, Data.Storage c a) =>
--   EtaAssignMap ->
   TIdx.Section ->
   Map String (a -> a) ->
   EqGen.EquationSystem Node s x (Data c a)
etaGiven sec etaFunc = Fold.fold $ Map.mapWithKey f (etaAssign sec)
  where f n (strP, strN, g) =
          EqGen.variable n =.= EqGen.liftF (Data.map ef) (EqGen.variable $ g n)
          where ef x = if x >= 0 then fpos x else fneg x
                fpos = maybe (err strP) id (Map.lookup strP etaFunc)
                fneg = maybe (err strN) (\h -> recip . h . negate)
                                        (Map.lookup strN etaFunc)
                err str x = error ("not defined: " ++ show str ++ " for " ++ show x)

givenAverage ::(Eq a, EqArith.Sum a)=>
               Env a  -> 
               EqSystemData a
givenAverage env = EqGen.fromEnv env

 -- | To be moved to Utility Area   

envGetData ::
  (Ord node) =>
  EqEnv.Complete node (Result (Data va a)) (Result (Data vv v)) ->
  EqEnv.Complete node (Result (Data.Apply va a)) (Result (Data.Apply vv v))
envGetData =
  EqEnv.completeFMap (fmap getData) (fmap getData)


etaAssign ::
  TIdx.Section ->
  Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)
etaAssign sec = Map.fromList $
  (XIdx.eta sec Wasser Netz, ( "storage", "storage", myflip)) :
  (XIdx.eta sec Netz Wasser, ( "storage", "storage", noflip)) :

  (XIdx.eta sec Kohle Netz, ( "coal", "coal", myflip)) :
  (XIdx.eta sec Netz Kohle, ( "coal", "coal", noflip)) :

  (XIdx.eta sec Gas Netz, ( "gas", "gas", myflip)) :
  (XIdx.eta sec Netz Gas, ( "gas", "gas", noflip)) :

  (XIdx.eta sec Netz Netzlast, ( "last", "last", myflip)) :
  (XIdx.eta sec Netzlast Netz, ( "last", "last", noflip)) :

  []

noflip :: TIdx.InSection TIdx.Eta node -> TIdx.InSection TIdx.Power node
noflip (TIdx.InSection sec ( TIdx.Eta (TIdx.StructureEdge n1 n2))) = 
  TIdx.InSection sec (TIdx.Power (TIdx.StructureEdge n1 n2))


myflip :: TIdx.InSection TIdx.Eta node -> TIdx.InSection TIdx.Power node
myflip (TIdx.InSection sec ( TIdx.Eta (TIdx.StructureEdge n1 n2))) = 
  TIdx.InSection sec (TIdx.Power (TIdx.StructureEdge n2 n1))


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
              -- TC (Sig.Arith s s) (Typ UT UT UT) (Data (v2 :> v1 :> Nil) 
              --                                    (TC (Sig.Arith s s) (Typ UT UT UT) (Data (v2 :> v1 :> Nil) b)))
               Sig.UTSignal2 v2  v1 (Sig.UTSignal2 v2  v1 b) 
               
doubleSweep fsolve varOptX varOptY varX varY =
  Sig.untype $ Sig.zipWith f varX  varY
  where f x y =
          Sig.untype $ Sig.convert $ Sig.zipWith (fsolve x y) varOptX varOptY


{-
doubleSweep::(SV.Convert v1 v2, 
              SV.Walker v1,SV.FromList v2, SV.Convert v1 v1,
              SV.Storage v2 (v1 a),
              SV.Storage v1 a, 
              SV.Zipper v1,
              SV.Storage v2 (v2 (EnvResult a)),
              SV.Storage v1 (EnvResult a),
              SV.Storage v2 (v1 (Data Nil a)),
              SV.Storage v1 (Data Nil a),
              SV.Walker v2,
              SV.Storage v2 (v2 a),
              SV.Storage v2 a,
              SV.Zipper v2,
              SV.Storage v2 (EqEnv.Complete Node (Result (Data Nil a)) (Result (Data Nil a))),
              SV.Storage v2(v1 (EqEnv.Complete Node (Result (Data Nil a)) (Result (Data Nil a)))),
              SV.Storage v2(v2 (TC (Sig.Arith s s)(Typ UT UT UT)(Data(v2 :> (v2 :> Nil))(EqEnv.Complete
                                     Node (Result (Data Nil a)) (Result (Data Nil a)))))),
              SV.Storage v2(TC(Sig.Arith s s)(Typ UT UT UT)(Data(v2 :> (v2 :> Nil))
                              (EqEnv.Complete Node (Result (Data Nil a)) (Result (Data Nil a))))),
              SV.Storage v2 (v2 (Data Nil a)),
              SV.Storage v2 (Data Nil a)) =>
              (Data Nil a -> Data Nil a -> Data Nil a -> Data Nil a -> EnvResult a) ->
              TC s typ (Data (v2 :> v1 :> Nil) a) ->
              TC s typ (Data (v2 :> v1 :> Nil) a) ->
              TC s typ (Data (v2 :> v2 :> Nil) a) ->
              TC s typ (Data (v2 :> v2 :> Nil) a) ->
              TC (Sig.Arith s s) (Typ UT UT UT) 
              (Data (v2 :> v2 :> Nil) (TC (Sig.Arith s s) (Typ UT UT UT) 
                                       (Data (v2 :> v2 :> Nil) (EnvResult a))))
doubleSweep fsolve varOptX varOptY varX varY =
  Sig.untype $ Sig.zipWith f (Sig.map Data varX) (Sig.map Data varY)
  where f x y =
          Sig.untype $ Sig.convert $ Sig.zipWith (fsolve x y) 
          (Sig.map Data varOptX) 
          (Sig.map Data varOptY)
-}