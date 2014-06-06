
module EFA.Action.Flow where


import qualified EFA.Flow.Topology.Quantity as FlowTopo

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


-- TODO : move in flow topology module
{-
getAbsFlowStatefromPowers :: (v Topology node v -> 
getAbsFlowStatefromPowers 
-}


{-
data AbsIdx = AbsIdx Int

getAbsFlowStatefromPowers::
-}  
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
   f (Graph.Graph n e (FlowTopo.Sums (Result v)) el) -> Result v
etaSys sq =
   let nodes = fmap Graph.nodeLabels sq
       sinks =
          fmap
             (Map.mapMaybe FlowTopo.sumIn .
              Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node)) nodes
       sources =
          fmap
             (Map.mapMaybe FlowTopo.sumOut .
              Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node)) nodes
       sumRes =
          foldl1 (liftA2 (~+)) . foldMap Map.elems

   in liftA2 (~/) (sumRes sinks) (sumRes sources)

-- | TODO: write function for overall system loss -- deliver two values -- with & without reuse efficiency

{- Old Template
checkGreaterZeroNotNaN ::
  (Arith.Constant a, Ord a,RealFloat a,
   Ord node,
   Monoid (sweep vec Bool), Node.C node,
   Sweep.SweepClass sweep vec a,
   Sweep.SweepClass sweep vec Bool,
   Sweep.SweepMap sweep vec a Bool) =>
  StateFlow.Graph node b (Result (sweep vec a)) ->
  Result (sweep vec Bool)
checkGreaterZeroNotNaN = fold . StateFlow.mapGraphWithVar
  (\_ _ -> Undetermined)
  (\(Idx.InPart _ var) v ->
     case var of
          TopoVar.Power _ ->
            case v of
                 (Determined w) -> Determined $ Sweep.map (\ x -> x > Arith.zero && not (isNaN x)) w
                 _ -> Undetermined
          _ -> Undetermined)


findBestIndex ::
  (Ord a, Arith.Constant a, UV.Unbox a,RealFloat a,
   Sweep.SweepVector UV.Vector a,
   Sweep.SweepClass sweep UV.Vector a,
   Sweep.SweepVector UV.Vector Bool,
   Sweep.SweepClass sweep UV.Vector Bool) =>
  (sweep UV.Vector Bool) ->
  (sweep UV.Vector a) ->
  (sweep UV.Vector a) ->
  Maybe (Int, a, a)
findBestIndex cond objVal esys =
  case UV.ifoldl' f start (UV.zip cs os) of
       (Just idx, o) -> Just (idx, o, es UV.! idx)
       _ -> Nothing
  where
        cs = Sweep.fromSweep cond
        es = Sweep.fromSweep esys
        os = Sweep.fromSweep objVal

        start = (Nothing, Arith.zero)

        f acc@(idx, o) i (c, onew) =
          if c && not (isNaN onew) && maybe True (const (onew > o)) idx
             then (Just i, onew)
             else acc



-}
