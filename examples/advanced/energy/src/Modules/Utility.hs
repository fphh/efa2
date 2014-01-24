{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules.Utility where

import qualified Modules.Types as Types

import qualified EFA.Flow.Topology as Topology
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.State as State
import qualified EFA.Flow.State.Quantity as StateQty

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Determined))

import qualified EFA.Graph as Graph

import qualified EFA.Signal.Vector as Vec
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data (Data, Nil, (:>))
import EFA.Signal.Typ (Typ)

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.List as List
import Data.Tuple.HT (fst3, snd3, thd3)

import Control.Monad ((>=>))

maxBy :: (Ord b) => (a -> b) -> a -> a -> a
maxBy f x y = if f y > f x then y else x


unzipMap :: Map k (a, b) -> (Map k a, Map k b)
unzipMap m = (Map.map fst m, Map.map snd m)

unzip3Map :: Map k (a, b, c) -> (Map k a, Map k b, Map k c)
unzip3Map m = (Map.map fst3 m, Map.map snd3 m, Map.map thd3 m)

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, a, _, _) = a

thd4 :: (a, b, c, d) -> c
thd4 (_, _, a, _) = a

frth4 :: (a, b, c, d) -> d
frth4 (_, _, _, a) = a

unzip4Map :: Map k (a, b, c, d) -> (Map k a, Map k b, Map k c, Map k d)
unzip4Map m = (Map.map fst4 m, Map.map snd4 m, Map.map thd4 m, Map.map frth4 m)

{-# INLINE mapQuadruple #-}
mapQuadruple :: (a -> u, b -> v, c -> w, d -> x) -> (a,b,c,d) -> (u,v,w,x)
mapQuadruple ~(f, g, h, i) ~(x, y, z, a) = (f x, g y, h z, i a)

nan :: (Arith.Constant a, Arith.Product a) => a
nan = Arith.zero Arith.~/ Arith.zero

nothing2Nan :: (Arith.Constant v, Arith.Product v) => Maybe (Result v) -> v
nothing2Nan (Just (Determined x)) = x
nothing2Nan _ = nan

resolveInvalidPts ::
  b -> (a -> b) -> Map k0 (Map k1 (Maybe a)) -> Map k0 (Map k1 b)
resolveInvalidPts dflt f m = Map.map (Map.map (maybe dflt f)) m

getMaxObj, getMaxEta ::
  (Arith.Constant a) =>
  Map k0 (Map k1 (Maybe (a, a, b))) -> Map k0 (Map k1 a)
getMaxObj = resolveInvalidPts nan fst3
getMaxEta = resolveInvalidPts nan snd3


getMaxPos ::
  (Arith.Constant a, StateQty.Lookup (Idx.InPart part qty), Ord node) =>
  Idx.InPart part qty node ->
  Map k0 (Map k1 (Maybe (a, a, Types.EnvResult node a))) -> Map k0 (Map k1 a)
getMaxPos pos = resolveInvalidPts nan (f . StateQty.lookup pos . thd3)
  where f (Just (Determined x)) = x
        f _ = nan

to2DMatrix ::
  (Vec.Storage v1 a, Vec.Storage v2 (v1 a),
   Vec.FromList v1, Vec.FromList v2, Ord b) =>
  Map [b] a ->
  Sig.TC tr (Typ x y z) (Data (v2 :> v1 :> Nil) a)
to2DMatrix =
  Sig.fromList2 . List.transpose . Map.elems .
  Map.mapKeysWith (++) f . fmap (:[])
  where f (x:_) = x
        f [] = error "to2DMatrix: empty list"

getFlowTopology ::
  Idx.State ->
  State.Graph node edge sectionLabel nodeLabel
              storageLabel edgeLabel carryLabel ->
  Maybe (Graph.Graph node edge nodeLabel edgeLabel)
getFlowTopology state =
  fmap Topology.topology . Map.lookup state . State.states

toPowerMatrix ::
  (Arith.Constant a) =>
  Map k0 (Map k1 (Maybe a)) -> Map k0 (Map k1 a)
toPowerMatrix = resolveInvalidPts nan id


flipPower ::
  Idx.InPart part TopoIdx.Power node -> Idx.InPart part TopoIdx.Power node
flipPower = Idx.liftInPart $
  \(TopoIdx.Power (TopoIdx.Position f t)) -> TopoIdx.Power (TopoIdx.Position t f)


nestM :: (Monad m) => Int -> (a -> m a) -> a -> m a
nestM n act = foldr (>=>) return (replicate n act)


