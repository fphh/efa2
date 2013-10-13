{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Application.Optimisation where

import EFA.Application.Simulation (EtaAssignMap, makeEtaFuncGiven)

import qualified EFA.Application.AbsoluteState as EqGenState

import qualified EFA.Flow.State.Index as StateIdx

import EFA.Signal.Data (Data(Data), Nil)

import qualified EFA.Equation.Arithmetic as EqArith

import qualified EFA.Graph.StateFlow.Environment as EqEnvState
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Tuple.HT (snd3)
import Data.Monoid((<>),mempty)
import Data.Maybe (mapMaybe)

-- | TODO Functions below could eventually be moved to a module Application/Given


{- |
Function to specify that an efficiency function in etaAssign
is to be looked up with input power
-}
etaOverPowerIn ::
   Idx.InPart part Idx.Eta node -> Idx.InPart part Idx.Power node
etaOverPowerIn =
   Idx.liftInPart $ \(Idx.Eta e) -> Idx.Power $ Idx.flip e

{- |
Function to specify that an efficiency function in etaAssign
is to be looked up with output power
-}
etaOverPowerOut ::
   Idx.InPart part Idx.Eta node -> Idx.InPart part Idx.Power node
etaOverPowerOut =
   Idx.liftInPart $ \(Idx.Eta e) -> Idx.Power e


-- | Takes all non-energy and non-power values from an env,
-- | removes values in section x and generate given equations
givenAverageWithoutStateX ::
  (Eq v, EqArith.Sum v, Node.C node, Ord node,Eq a, EqArith.Sum a) =>
  Idx.State ->
  EqEnvState.Complete node a v  ->
  EqGenState.EquationSystem node s a v
givenAverageWithoutStateX stateToRemove (EqEnvState.Complete scalar signal) =
   (EqGenState.fromMap $ EqEnvState.dtimeMap signal) <>
   (EqGenState.fromMap $ Map.filterWithKey f $ EqEnvState.etaMap signal) <>
   (EqGenState.fromMap $ Map.filterWithKey f $ EqEnvState.xMap signal) <>
--   (EqGenState.fromMap $ EqEnvState.stEnergyMap scalar) <>
   (EqGenState.fromMap $ EqEnvState.stXMap scalar)
--   (EqGenState.fromMap $ EqEnvState.stInSumMap scalar) <>
--   (EqGenState.fromMap $ EqEnvState.stOutSumMap scalar)
   where f :: Idx.InState idx node -> v -> Bool
         f (Idx.InPart state _) _ = state /= stateToRemove


givenAverageWithoutState ::
  (Eq v, EqArith.Sum v, Node.C node, Ord node, Eq a, EqArith.Sum a) =>
  Idx.State ->
  EqEnvState.Complete node a v ->
  EqEnvState.Complete node a v
givenAverageWithoutState _stateToRemove (EqEnvState.Complete scalar signal) =
  EqEnvState.Complete
    ( mempty { EqEnvState.stXMap = EqEnvState.stXMap scalar } )
    ( mempty { EqEnvState.etaMap = EqEnvState.etaMap signal,
               EqEnvState.xMap   = EqEnvState.xMap signal,
               EqEnvState.dtimeMap = EqEnvState.dtimeMap signal } )
--  where f :: Idx.InState idx node -> v -> Bool
--        f (Idx.InPart state _) _ = state /= stateToRemove


givenForOptimisation ::
  (EqArith.Constant a, Node.C node, Fractional a,
  Ord a, Show a, EqArith.Sum a, Ord node) =>
  Topo.StateFlowGraph node ->
  EqEnvState.Complete node (Data Nil a) (Data Nil a)  ->
  EtaAssignMap node ->
  Map String (a -> a) ->
  Idx.State ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a) ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a) ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a) ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a)

givenForOptimisation stateFlowGraph env etaAssign etaFunc state commonGiven givenLoad givenDOF =
  commonGiven <>
  EqGenState.fromGraph True (Topo.dirFromFlowGraph stateFlowGraph) <>
  makeEtaFuncGiven etaAssign etaFunc <>
  givenAverageWithoutStateX state env <>
  givenLoad <>
  givenDOF


initialEnv ::
  (Ord node, Fractional d, Show node) =>
  node ->
  Topo.StateFlowGraph node ->
  EqEnvState.Complete node (Data Nil d) (Data Nil d)
initialEnv _xStorageEdgesNode g =
  EqEnvState.Complete
    ( mempty { EqEnvState.stXMap = Map.fromList stxs } )
    ( mempty { EqEnvState.etaMap = Map.fromList $ zip es $ repeat (Data 0.5),
               EqEnvState.xMap = Map.fromList xs,
               EqEnvState.dtimeMap = Map.fromList $ zip dts $ repeat (Data 1) } )
  where gdir = Topo.dirFromFlowGraph g

        es = mapMaybe f $ Graph.edges gdir
        state (Topo.FlowEdge (Topo.StructureEdge (Idx.InPart s _))) = Just s
        state _ = Nothing
        node (Idx.PartNode _ n) = n
        f e = fmap (\s -> StateIdx.eta s (node $ Graph.from e) (node $ Graph.to e)) (state e)


        nodestate (Idx.PartNode p _) =
           Idx.switchAugmented Nothing Nothing Just p

        ns = Graph.nodes gdir
        h n (ins, _, outs) =
          flip foldMap (nodestate n) $
            \st ->
              let x = StateIdx.x st (node n) . node
              in  map x $ filter ((nstate n ==) . nstate) $
                  Set.toList ins ++ Set.toList outs

        xs = foldMap xfactors $ Map.mapWithKey h ns

        -- @HT numerisch ok?
        xfactors ys = zip ys (repeat $ Data (1/(fromIntegral $ length ys)))

        sts = Map.filter (Topo.isStorage . snd3)
              $ Graph.nodes
              $ Graph.lefilter (\(e, ()) -> Topo.isStorageEdge e) gdir

        nstate (Idx.PartNode s _) = s

        hstx n (ins, _, outs) =
          let stx = StateIdx.stx
                    . flip Idx.PartNode (node n)
                    . Idx.StorageTrans (nstate n) . nstate
          in  map stx $ Set.toList ins ++ Set.toList outs
        stxs = foldMap xfactors $ Map.mapWithKey hstx sts

        dts = map StateIdx.dTime
              $ Set.toList
              $ Set.fromList
              $ mapMaybe nodestate
              $ Map.keys ns

{-
        stKeys = Map.foldWithKey q [] $ Graph.nodes gdir
        q (Idx.PartNode (Idx.NoExit Idx.Init) n) (_, _, outs) =
          (map (flip qf n) (Set.toList outs) ++)
        q _ _ = id

        qf (Idx.PartNode Idx.Exit _) =
          Idx.ForNode (Idx.StEnergy (Idx.StorageEdge Idx.Init Idx.Exit))
        qf (Idx.PartNode (Idx.NoExit (Idx.NoInit s)) _) =
          Idx.ForNode (Idx.StEnergy (Idx.StorageEdge Idx.Init (Idx.NoExit s)))
-}
