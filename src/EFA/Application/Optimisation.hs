{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Application.Optimisation where

import qualified EFA.Application.AbsoluteState as EqGenState

import qualified EFA.Graph.StateFlow.Index as StateIdx
import qualified EFA.Application.Index as XIdx
import EFA.Application.AbsoluteState ( (=.=) )

import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(..), Nil) --,(:>))

import qualified EFA.Equation.Arithmetic as EqArith

import qualified EFA.Graph.StateFlow.Environment as EqEnvState
import qualified EFA.Graph.StateFlow.Index as SFIdx
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Graph

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Monoid((<>),mempty)
import Data.Maybe (mapMaybe)

-- | TODO Functios below could ventually be moved to a module Application/Given


-- | Function to specifiy that an efficiency function in etaAssign is to be looked up with input power
etaOverPowerInState :: StateIdx.Eta node -> StateIdx.Power node
etaOverPowerInState =
   TIdx.liftInState $ \(TIdx.Eta e) -> TIdx.Power $ TIdx.flip e

-- | Function to specifiy that an efficiency function in etaAssign is to be looked up with output power
etaOverPowerOutState :: StateIdx.Eta node -> StateIdx.Power node
etaOverPowerOutState =
   TIdx.liftInState $ \(TIdx.Eta e) -> TIdx.Power e

-- | Function to specifiy that an efficiency function in etaAssign is to be looked up with input power
etaOverPowerIn :: XIdx.Eta node -> XIdx.Power node
etaOverPowerIn =
   TIdx.liftInSection $ \(TIdx.Eta e) -> TIdx.Power $ TIdx.flip e

-- | Function to specifiy that an efficiency function 
-- | in etaAssign is to be looked up with output power
etaOverPowerOut :: XIdx.Eta node -> XIdx.Power node
etaOverPowerOut =
   TIdx.liftInSection $ \(TIdx.Eta e) -> TIdx.Power e

type EtaAssignMap node =
  Map (StateIdx.Eta node)
      (String, String, StateIdx.Eta node -> StateIdx.Power node)


-- | Generate given equations using efficiency curves or functions for a specified section
makeEtaFuncGiven ::
   (Fractional a, Ord a, Show a, EqArith.Sum a,
    Data.Apply c a ~ v, Eq v, Data.ZipWith c, Data.Storage c a, Node.C node) =>
   (TIdx.State -> EtaAssignMap node) ->
   TIdx.State ->
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
  TIdx.State ->
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
   where f :: TIdx.InState idx node -> v -> Bool
         f (TIdx.InPart state _) _ = state /= stateToRemove


givenAverageWithoutState ::
  (Eq v, EqArith.Sum v, Node.C node, Ord node, Eq a, EqArith.Sum a) =>
  TIdx.State ->
  EqEnvState.Complete node a v ->
  EqEnvState.Complete node a v
givenAverageWithoutState _stateToRemove (EqEnvState.Complete scalar signal) =
  EqEnvState.Complete
    ( mempty { EqEnvState.stXMap = EqEnvState.stXMap scalar } )
    ( mempty { EqEnvState.etaMap = EqEnvState.etaMap signal,
               EqEnvState.xMap   = EqEnvState.xMap signal,
               EqEnvState.dtimeMap = EqEnvState.dtimeMap signal } )
--  where f :: TIdx.InState idx node -> v -> Bool
--        f (TIdx.InPart state _) _ = state /= stateToRemove


givenForOptimisation ::
  (EqArith.Constant a, Node.C node, Fractional a,
  Ord a, Show a, EqArith.Sum a, Ord node) =>
  TD.StateFlowGraph node ->
  EqEnvState.Complete node (Data Nil a) (Data Nil a)  ->
  (TIdx.State -> EtaAssignMap node) ->
  Map String (a -> a) ->
  TIdx.State ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a) ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a) ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a) ->
  EqGenState.EquationSystem node s (Data Nil a) (Data Nil a)

givenForOptimisation stateFlowGraph env etaAssign etaFunc state commonGiven givenLoad givenDOF =
  commonGiven <>
  EqGenState.fromGraph True (TD.dirFromFlowGraph stateFlowGraph) <>
  makeEtaFuncGiven etaAssign state etaFunc <>
  givenAverageWithoutStateX state env <>
  givenLoad <>
  givenDOF


initialEnv ::
  (Ord node, Fractional d, Show node) =>
  node ->
  TD.StateFlowGraph node ->
  EqEnvState.Complete node (Data Nil d) (Data Nil d)
initialEnv _xStorageEdgesNode g =
  EqEnvState.Complete
    ( mempty { EqEnvState.stXMap = Map.fromList stxs } )
    ( mempty { EqEnvState.etaMap = Map.fromList $ zip es $ repeat (Data 0.5),
               EqEnvState.xMap = Map.fromList xs,
               EqEnvState.dtimeMap = Map.fromList $ zip dts $ repeat (Data 1) } )
  where gdir = TD.dirFromFlowGraph g

        es = mapMaybe f $ Graph.edges gdir
        state (TD.FlowEdge (TD.StructureEdge (TIdx.InPart s _))) = Just s
        state _ = Nothing
        node (TIdx.PartNode _ n) = n
        f e = fmap (\s -> SFIdx.eta s (node $ Graph.from e) (node $ Graph.to e)) (state e)


        nodestate (TIdx.PartNode (TIdx.NoExit (TIdx.NoInit s)) _) = Just s
        nodestate _ = Nothing

        ns = Graph.nodes gdir
        h n (ins, _, outs) acc =
          flip (maybe acc) (nodestate n) $
            \st ->
              let x = SFIdx.x st (node n) . node
                  il = map x $ filter ((nstate n ==) . nstate) (Set.toList ins)
                  ol = map x $ filter ((nstate n ==) . nstate) (Set.toList outs) 
              in  filter (not . null) [il, ol] ++ acc

        xs = concatMap xfactors $ Map.foldWithKey h [] ns

        -- @HT numerisch ok?
        xfactors ys = zip ys (repeat $ Data (1/(fromIntegral $ length ys)))

        isStorage (_, nt, _) = TD.isStorage nt

        sts = Map.filter isStorage
              $ Graph.nodes
              $ Graph.lefilter (\(e, ()) -> TD.isStorageEdge e) gdir

        nstate (TIdx.PartNode s _) = s

        hstx n (ins, _, outs) acc =
          let stx = SFIdx.stx 
                    . flip TIdx.PartNode (node n)
                    . TIdx.StorageTrans (nstate n) . nstate
              il = map stx (Set.toList ins)
              ol = map stx (Set.toList outs) 
          in  filter (not . null) [il, ol] ++ acc
        stxs = concatMap xfactors $ Map.foldWithKey hstx [] sts

        dts = map SFIdx.dTime
              $ Set.toList
              $ Set.fromList
              $ mapMaybe nodestate
              $ Map.keys ns

{-
        stKeys = Map.foldWithKey q [] $ Graph.nodes gdir
        q (TIdx.PartNode (TIdx.NoExit TIdx.Init) n) (_, _, outs) =
          (map (flip qf n) (Set.toList outs) ++)
        q _ _ = id

        qf (TIdx.PartNode TIdx.Exit _) =
          TIdx.ForNode (TIdx.StEnergy (TIdx.StorageEdge TIdx.Init TIdx.Exit))
        qf (TIdx.PartNode (TIdx.NoExit (TIdx.NoInit s)) _) =
          TIdx.ForNode (TIdx.StEnergy (TIdx.StorageEdge TIdx.Init (TIdx.NoExit s)))
-}
