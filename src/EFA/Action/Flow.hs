
module EFA.Action.Flow where


import qualified EFA.Flow.Topology.Quantity as TopoQty

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~/))
import EFA.Equation.Result (Result)

import qualified EFA.Graph as Graph

import qualified Data.Map as Map

import Control.Applicative (liftA2)

import Data.Foldable (Foldable, foldMap)
import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Report.Format as Format

import qualified Data.Maybe as Maybe
import Control.Applicative (liftA2)
import Control.Monad(join)
import Data.Foldable (Foldable, foldMap)

import EFA.Utility(Caller,
                 merror,
               --    (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow"

nc :: FunctionName -> Caller
nc = genCaller modul


-- TODO :: old Code adjust to new needs
data Orientation = Dir | UnDir deriving Show

absoluteStateIndex ::
  (Node.C node) =>
  Graph.Graph node Graph.DirEdge nodeLabel1 a1 ->
  Graph.Graph node Graph.EitherEdge nodeLabel a ->
  Idx.AbsoluteState
absoluteStateIndex topo flowTopo =
  let tlabels = map unEitherEDir $ Map.keys $ Graph.edgeLabels topo

      flabels = Map.fromList $ map unEDir $ Map.keys $ Graph.edgeLabels flowTopo

      unEDir (Graph.EDirEdge (Graph.DirEdge f t)) = ((f, t), Dir)
      unEDir (Graph.EUnDirEdge (Graph.UnDirEdge f t)) = ((f, t), UnDir)

      unEitherEDir (Graph.DirEdge f t) = (f, t)

      g k@(f, t) =
        case (Map.lookup k flabels, Map.lookup (t, f) flabels) of
             (Just Dir, _) -> 0
             (Just UnDir, _) -> 1
             (_, Just Dir) -> 2
             (_, Just UnDir) -> 1
             _ -> error $ "EFA.Graph.Topology.flowNumber: edge not found "
                          ++ Format.showRaw (Node.display f :: Format.ASCII)
                          ++ "->"
                          ++ Format.showRaw (Node.display t :: Format.ASCII)

      toTernary xs = Idx.AbsoluteState $ sum $ zipWith (*) xs $ map (3^) [0 :: Int ..]

  in toTernary $ map g tlabels


-- | TODO: include storages with a reuse efficiency -- deliver two values -- with & without reuse efficiency
etaSys ::
   (Node.C n, Graph.Edge e, Ord (e n),
    Arith.Sum v, Arith.Product v,
    Functor f, Foldable f) =>
   f (Graph.Graph n e (TopoQty.Sums (Result v)) el) -> Result v
etaSys sq =
   let nodes = fmap Graph.nodeLabels sq
       sinks =
          fmap
             (Map.mapMaybe TopoQty.sumIn .
              Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node)) nodes
       sources =
          fmap
             (Map.mapMaybe TopoQty.sumOut .
              Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node)) nodes
       sumRes =
          foldl1 (liftA2 (~+)) . foldMap Map.elems

   in liftA2 (~/) (sumRes sinks) (sumRes sources)

getEtaValues ::
  (Functor f, Ord (e node), Node.C node, Graph.Edge e) =>
  f (Graph.Graph node e (TopoQty.Sums b) el) -> 
  (f (Map.Map node b), f (Map.Map node b), f (Map.Map node b))
getEtaValues sq =
   let nodes = fmap Graph.nodeLabels sq
       sinks =
          fmap
             (Map.mapMaybe TopoQty.sumIn .
              Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node)) nodes
       sources =
          fmap
             (Map.mapMaybe TopoQty.sumOut .
              Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node)) nodes

       storages =
          fmap
             (Map.mapMaybe TopoQty.sumOut .
              Map.filterWithKey (\node _ -> Node.isStorage $ Node.typ node)) nodes

   in (sources, sinks, storages)

etaSysVal caller state (sources, sinks, storages) lifeCycleEfficiencies = let
       sumRes = foldl1 (liftA2 (Arith.~+)) . foldMap Map.elems
       sinkStorages = Map.mapWithKey
                      (\node c -> fmap (applyGenerationEfficiency caller node state lifeCycleEfficiencies) c) storages
       sourceStorages = Map.mapWithKey 
                      (\node c -> fmap (applyUsageEfficiency caller node state lifeCycleEfficiencies) c) storages
          
      in liftA2 (Arith.~/) (sumRes sinks Arith.~+ sumRes sinkStorages) (sumRes sources Arith.~+ sumRes sourceStorages)     


newtype GenerationEfficiency a = GenerationEfficiency a
newtype UsageEfficiency a = UsageEfficiency a


         
-- | Indicates that values are Sign-Corrected for Storage-Sign-Convention positive == charging, negative == discharging
newtype StorageFlow a = StorageFlow a

newtype LifeCycleMap node a = 
  LifeCycleMap (Map.Map Idx.AbsoluteState (Map.Map node (GenerationEfficiency a,UsageEfficiency a)))

lookupLifeCycleEta :: (Ord node, Ord a, Arith.Constant a, Arith.Product a) =>
  LifeCycleMap node a -> 
  Idx.AbsoluteState -> 
  node -> 
  Maybe (GenerationEfficiency a, UsageEfficiency a) 
lookupLifeCycleEta (LifeCycleMap m) state node = join $ fmap (Map.lookup node) $ Map.lookup state m 

applyGenerationEfficiency:: 
  (Ord a, Ord node,Show node,
   Arith.Constant a, 
   Arith.Product a) => 
  Caller ->
  Idx.AbsoluteState ->
  node -> 
  LifeCycleMap node a -> 
  StorageFlow a -> a
applyGenerationEfficiency caller state node m (StorageFlow energy) = 
  if energy < Arith.zero then energy Arith.~/ eta else Arith.zero
  where (GenerationEfficiency eta,_) = Maybe.fromMaybe e $ lookupLifeCycleEta m state node 
        e = merror caller modul "applyGenerationEfficiency" 
            ("Node not in LifeCycleEfficiencyMap: " ++ show node)

applyUsageEfficiency:: 
  (Ord a, Ord node,Show node,
   Arith.Constant a, 
   Arith.Product a) => 
  Caller ->
  Idx.AbsoluteState ->
  node -> 
  LifeCycleMap node a -> 
  StorageFlow a -> a
applyUsageEfficiency caller state node m (StorageFlow energy) = 
  if energy < Arith.zero then energy Arith.~/ eta else Arith.zero
  where (_,UsageEfficiency eta) = Maybe.fromMaybe e $ lookupLifeCycleEta m state node 
        e = merror caller modul "applyUsageEfficiency" 
            ("Node not in LifeCycleEfficiencyMap: " ++ show node)

-- | TODO: write function for overall system loss -- deliver two values -- with & without reuse efficiency


