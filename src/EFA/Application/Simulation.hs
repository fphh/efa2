{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module EFA.Application.Simulation where

--import qualified EFA.Application.Absolute as EqGen
--import qualified EFA.Application.Optimisation as AppOpt
--import qualified EFA.Application.Utility as AppUt
import qualified EFA.Application.AbsoluteState as EqGen

import EFA.Application.AbsoluteState ( (=.=) )

--import qualified EFA.Equation.Environment as EqEnv
--import qualified EFA.Equation.Record as EqRecord
--import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as EqArith
import EFA.Equation.Result(Result)

import qualified EFA.Graph.StateFlow.Environment as EqEnv
import qualified EFA.Graph.StateFlow.Index as StateIdx
import qualified EFA.Graph.StateFlow as StateFlow

import qualified EFA.Graph.Topology.Index as TIdx
--import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(..), Nil,(:>))

import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Base as Base
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.SequenceData as SD


import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Map (Map)
import Data.Monoid((<>))

{-
eqs :: EqGen.EquationSystem Node s (Data Nil Double) (Data ([] :> Nil) Double)
eqs = Simulation.givenSimulate etaAssign etaFunc $
             SD.SequData [SD.Section (TIdx.Section 0) undefined rec]
-}

type EtaAssignMap node = Map (StateIdx.Eta node) (String, String, StateIdx.Eta node -> StateIdx.Power node)

solve :: (Node.C node,
          Eq (v a),
          Fractional a,
          Ord a,
          Show a,
          EqArith.Constant a,
          SV.Zipper v,
          SV.Walker v,
          SV.Storage v a,
          SV.Singleton v,
          SV.Len (v a),
          SV.FromList v,
          Base.BSum a) =>
         TD.Topology node ->
         (TIdx.State ->  EtaAssignMap node) ->
         Map String (a -> a) ->
         Record.PowerRecord node v a ->
         EqEnv.Complete node (Result (Data Nil a)) (Result (Data (v :> Nil) a))
solve topology etaAssign etaFunc powerRecord = EqGen.solveSimple $
                                              givenSimulate stateFlowGraph etaAssign etaFunc powerRecord
  where
    -- | Build Sequenceflow graph for simulation
    stateFlowGraph = StateFlow.stateGraphActualStorageEdges $ fmap (flowStates !!) $ SD.fromList [0]
    flowStates = StateAnalysis.advanced topology


givenSimulate ::
 (Eq a, Show a, Fractional a, Ord a, Ord node, Node.C node,
  Base.BSum a, EqArith.Sum a, EqArith.Constant a,
  Eq (v a),
  SV.Zipper v,SV.FromList v,SV.Len (v a),
  SV.Singleton v,
  SV.Walker v,
  SV.Storage v a) =>
  TD.StateFlowGraph node ->
  (TIdx.State ->  EtaAssignMap node) ->
  Map String (a -> a) ->
  Record.PowerRecord node v a ->
  (forall s . EqGen.EquationSystem node s (Data Nil a) (Data (v :> Nil) a))

givenSimulate stateFlowGraph etaAssign etaFunc powerRecord =
--  (StateIdx.storage TIdx.initial Water EqGen..= Data 0) <>
   --Fold.fold (SD.mapWithSection f sf)
  (EqGen.fromGraph True $ TD.dirFromFlowGraph (stateFlowGraph)) <>
   f powerRecord
   where f (Record.Record t xs) =
           (StateIdx.dTime (TIdx.State 0) EqGen..=
             (Data  $ SV.fromList $ replicate (Sig.len t) 1))
           <> makeEtaFuncGiven etaAssign (TIdx.State 0) etaFunc
           <> Fold.fold (Map.mapWithKey g xs)
           where
             g (TIdx.PPos (TIdx.StructureEdge p0 p1)) p =
                   (StateIdx.power (TIdx.State 0)  p0 p1 EqGen..= Sig.unpack p)

-- | Generate given equations using efficiency curves or functions for a specified section
makeEtaFuncGiven ::
   (Fractional a, Ord a, Show a, EqArith.Sum a,
    Data.Apply c a ~ v, Eq v, Data.ZipWith c, Data.Storage c a, Node.C node) =>
   (TIdx.State -> EtaAssignMap node) ->
   TIdx.State ->
   Map String (a -> a) ->
   EqGen.EquationSystem node s x (Data c a)
makeEtaFuncGiven etaAssign state etaFunc = Fold.fold $ Map.mapWithKey f (etaAssign state)
  where f n (strP, strN, g) =
          EqGen.variable n =.= EqGen.liftF (Data.map ef) (EqGen.variable $ g n)
          where ef x = if x >= 0 then fpos x else fneg x
                fpos = maybe (err strP) id (Map.lookup strP etaFunc)
                fneg = maybe (err strN) (\h -> recip . h . negate)
                                        (Map.lookup strN etaFunc)
                err str x = error ("not defined: " ++ show str ++ " for " ++ show x)


{-
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

-}                
