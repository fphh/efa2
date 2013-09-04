{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Application.Optimisation where

import qualified EFA.Application.AbsoluteState as EqGenState
import EFA.Application.AbsoluteState ( (=.=) )

import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.Sequence.Index as SeqIdx

import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(..), Nil) --,(:>))

import qualified EFA.Equation.Arithmetic as EqArith

import qualified EFA.Graph.StateFlow.Environment as EqEnvState
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Tuple.HT (snd3)
import Data.Monoid((<>),mempty)
import Data.Maybe (mapMaybe)

-- | TODO Functios below could ventually be moved to a module Application/Given


-- | Function to specifiy that an efficiency function in etaAssign is to be looked up with input power
etaOverPowerInState :: StateIdx.Eta node -> StateIdx.Power node
etaOverPowerInState =
   Idx.liftInState $ \(Idx.Eta e) -> Idx.Power $ Idx.flip e

-- | Function to specifiy that an efficiency function in etaAssign is to be looked up with output power
etaOverPowerOutState :: StateIdx.Eta node -> StateIdx.Power node
etaOverPowerOutState =
   Idx.liftInState $ \(Idx.Eta e) -> Idx.Power e

-- | Function to specifiy that an efficiency function in etaAssign is to be looked up with input power
etaOverPowerIn :: SeqIdx.Eta node -> SeqIdx.Power node
etaOverPowerIn =
   Idx.liftInSection $ \(Idx.Eta e) -> Idx.Power $ Idx.flip e

-- | Function to specifiy that an efficiency function
-- | in etaAssign is to be looked up with output power
etaOverPowerOut :: SeqIdx.Eta node -> SeqIdx.Power node
etaOverPowerOut =
   Idx.liftInSection $ \(Idx.Eta e) -> Idx.Power e

type EtaAssignMap node =
  Map (StateIdx.Eta node)
      (String, String, StateIdx.Eta node -> StateIdx.Power node)


-- | Generate given equations using efficiency curves or functions for a specified section
makeEtaFuncGiven ::
   (Fractional a, Ord a, Show a, EqArith.Sum a,
    Data.Apply c a ~ v, Eq v, Data.ZipWith c, Data.Storage c a, Node.C node) =>
   (Idx.State -> EtaAssignMap node) ->
   Idx.State ->
   Map String (a -> a) ->
   EqGenState.EquationSystem node s x (Data c a)
makeEtaFuncGiven etaAssign state etaFunc = Fold.fold $ Map.mapWithKey f (etaAssign state)
  where f n (strP, strN, g) =
          EqGenState.variable n =.= EqGenState.liftF (Data.map ef) (EqGenState.variable $ g n)
          where ef x = if x >= 0 then fpos x else fneg x
                fpos = maybe (err strP) id (Map.lookup strP etaFunc)
                fneg = maybe (err strN) (\h -> recip . h . negate)
                                        (Map.lookup strN etaFunc)
                err str x = error ("not defined: " ++ show str ++ " for " ++ show x)

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
  (Idx.State -> EtaAssignMap node) ->
  Map String (a -> a) ->
  Idx.State ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a) ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a) ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a) ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a)

givenForOptimisation stateFlowGraph env etaAssign etaFunc state commonGiven givenLoad givenDOF =
  commonGiven <>
  EqGenState.fromGraph True (Topo.dirFromFlowGraph stateFlowGraph) <>
  makeEtaFuncGiven etaAssign state etaFunc <>
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
