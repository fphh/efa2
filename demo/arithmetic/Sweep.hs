{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sweep where

import qualified EFA.Application.Utility as AppUt

import qualified EFA.Flow.Draw as Draw
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Storage.EquationSystem as StorageEqSys
import qualified EFA.Flow.State as State
import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.State.Absolute as StateEqSys
import EFA.Flow.State.Absolute ((.=), (=.=))
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.SequenceState.EquationSystem as SeqStateEqSys


import qualified EFA.Example.Topology.Tripod as Tripod
import EFA.Example.Topology.Tripod (source, sink, crossing, storage)

import qualified EFA.Graph.Topology as Topo

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~*), (~/), (~-))
import EFA.Equation.Result (Result(Determined))
import qualified EFA.Equation.Verify as Verify

import EFA.Utility.Stream (Stream((:~)))
import qualified EFA.Utility.Stream as Stream

import qualified EFA.Report.FormatValue as FormatValue

import EFA.Utility.Map (Caller)

import qualified Data.Vector.Unboxed as UV

import Data.Monoid (mconcat)
import qualified Data.Map as Map

import System.CPUTime (getCPUTime)
import Text.Printf (printf)



--newtype Sweep n a = Sweep { unSweep :: UV.Vector a } deriving (Show, Eq)

-- data mit strictness ist schneller als newtype.
data Sweep a = Sweep { unSweep :: !(UV.Vector a) } deriving (Show, Eq)


mapSweep ::
  (UV.Unbox b, UV.Unbox a) =>
  (a -> b) -> Sweep a -> Sweep b
mapSweep f (Sweep x) = Sweep (UV.map f x)


instance (Arith.Sum a, UV.Unbox a) => Arith.Sum (Sweep a) where
  (Sweep x) ~+ (Sweep y) = Sweep $ UV.zipWith (~+) x y
  {-# INLINE (~+) #-}

  (Sweep x) ~- (Sweep y) = Sweep $ UV.zipWith (~-) x y
  {-# INLINE (~-) #-}

  negate (Sweep x) = Sweep (UV.map Arith.negate x)
  {-# INLINE negate #-}

instance (Arith.Product a, UV.Unbox a) => Arith.Product (Sweep a) where
  (Sweep x) ~* (Sweep y) = Sweep $ UV.zipWith (~*) x y
  {-# INLINE (~*) #-}

  (Sweep x) ~/ (Sweep y) = Sweep $ UV.zipWith (~/) x y
  {-# INLINE (~/) #-}

  recip (Sweep x) = Sweep (UV.map Arith.recip x)
  {-# INLINE recip #-}

  constOne (Sweep x) = Sweep (UV.map Arith.constOne x)
  {-# INLINE constOne #-}


instance Arith.Integrate (Sweep a) where
  type Scalar (Sweep a) = (Sweep a)
  integrate = id
  {-# INLINE integrate #-}

instance (UV.Unbox a, Eq a, Arith.Constant a) =>
         Arith.ZeroTestable (Sweep a) where
  allZeros (Sweep x) = UV.and (UV.map (== Arith.zero) x)
  {-# INLINE allZeros #-}

  coincidingZeros (Sweep x) (Sweep y) =
    UV.or $ UV.zipWith (\a b -> a == Arith.zero && b == Arith.zero) x y

instance (FormatValue.FormatValue a, UV.Unbox a) =>
         FormatValue.FormatValue (Sweep a) where
  formatValue (Sweep x) = FormatValue.formatValue (UV.toList x)

customOne :: (UV.Unbox a, Arith.Constant a) => Sweep a
customOne = Sweep (UV.replicate 1000000 Arith.one)

---------------------------------------------------------------------------

flowStates :: [Topo.FlowTopology Tripod.Node]
flowStates =
   map (AppUt.identifyFlowState Tripod.topology) $
      [ [ AppUt.dirEdge source crossing,
          AppUt.dirEdge crossing storage,
          AppUt.dirEdge crossing sink ],
        [ AppUt.undirEdge source crossing,
          AppUt.dirEdge storage crossing,
          AppUt.dirEdge crossing sink] ]

storageEdgeXFactorsFromSignal ::
  (Fractional a, UV.Unbox a) =>
  Sweep a ->
  Rational -> Rational ->
  StateQty.Graph Tripod.Node (Result (Sweep a)) (Result (Sweep a)) ->
  StateQty.Graph Tripod.Node (Result (Sweep a)) (Result (Sweep a))
storageEdgeXFactorsFromSignal sig nout nin g = g { State.storages = tt }
   where tt = Map.map f $ State.storages g
         f gr = gr { Storage.edges = Map.map func $ Storage.edges gr }
         func x = x { StateQty.carryXOut = xout, StateQty.carryXIn = xin }
         xout = Determined (mapSweep (const (fromRational $ recip nout)) sig)
         xin = Determined (mapSweep (const (fromRational $ recip nin)) sig)

stateFlowGraph ::
  (Fractional a, UV.Unbox a) =>
  Sweep a ->
  StateQty.Graph Tripod.Node (Result (Sweep a)) (Result (Sweep a))
stateFlowGraph sig =
  storageEdgeXFactorsFromSignal sig 2 2 $ StateQty.graphFromStates flowStates

state0, state1, state2 :: Idx.State
state0 :~ state1 :~ state2 :~ _ = Stream.enumFrom $ Idx.state0



timeSweep :: Sweep Double -> Sweep Double
timeSweep sig =
  case sig of
       (Sweep x) -> Sweep (UV.iterateN (UV.length x) (+ 0.01) 0.01)


sourceSweep :: Sweep Double -> Sweep Double
sourceSweep sig = mapSweep (const (Arith.fromRational 1.2)) sig


eta :: Double -> Double
eta _ = 0.5

given ::
  Sweep Double -> Sweep Double ->
  StateEqSys.EquationSystemIgnore Tripod.Node s (Sweep Double) (Sweep Double)
given time sourceSig =
  mconcat $

  (StateIdx.dTime state0 .= time) :
  (StateIdx.dTime state1 .= mapSweep (+0.02) time) :

  (StateIdx.power state0 source crossing .= sourceSig) :

  ((StateEqSys.variable $ StateIdx.eta state0 source crossing) =.=
      StateEqSys.liftF (mapSweep eta)
                     (StateEqSys.variable $ StateIdx.power state0 source crossing)) :

  ((StateEqSys.variable $ StateIdx.eta state0 crossing storage) =.=
      StateEqSys.liftF (mapSweep eta)
                     (StateEqSys.variable $ StateIdx.power state0 crossing storage)) :

  ((StateEqSys.variable $ StateIdx.eta state0 crossing sink) =.=
      StateEqSys.liftF (mapSweep eta)
                     (StateEqSys.variable $ StateIdx.power state0 crossing sink)) :

  ((StateEqSys.variable $ StateIdx.x state0 crossing storage) =.=
      StateEqSys.liftF (mapSweep (const 0.2))
                  (StateEqSys.variable $ StateIdx.power state0 crossing source)) :

  ((StateEqSys.variable $ StateIdx.eta state1 storage crossing) =.=
      StateEqSys.liftF (mapSweep eta)
                     (StateEqSys.variable $ StateIdx.power state1 storage crossing)) :

  ((StateEqSys.variable $ StateIdx.eta state1 crossing sink) =.=
      StateEqSys.liftF (mapSweep eta)
                     (StateEqSys.variable $ StateIdx.power state1 crossing sink)) :
   []


lookupData :: Caller -> StateQty.Graph Tripod.Node a (Result d) -> Maybe d
lookupData name t =
  fmap (AppUt.checkDetermined name) $
  StateQty.lookup (StateIdx.energy state1 sink crossing) t


options ::
  (Arith.Sum a, Verify.LocalVar mode (Sweep a), UV.Unbox a, Arith.Constant a) =>
  StateEqSys.Options mode rec (Sweep a) (Sweep a)
options =
  StateEqSys.optionsBase
    SeqStateEqSys.equalStInOutSums
    (StorageEqSys.customOne customOne)

test :: Sweep Double -> Sweep Double -> Maybe Double
test tv sv =
  fmap (UV.sum . unSweep) $ lookupData "testSweep" $
  StateEqSys.solveOpts
    options
    (stateFlowGraph tv)
    (given tv sv)

showGraph :: Sweep Double -> Sweep Double -> IO ()
showGraph tv sv =
  let flowTopo =
        StateEqSys.solveOpts
          options
          (stateFlowGraph tv)
          (given tv sv)
  in Draw.xterm $ Draw.stateFlowGraph Draw.optionsDefault flowTopo


go :: String -> Maybe Double -> IO ()
go str x = do
  start <- getCPUTime
  putStrLn $ "Result of " ++ str ++ ":\t" ++ show x
  stop <- getCPUTime

  printf "CPU time:\t\t%6.6fs\n" (fromIntegral (stop - start) * 1e-12 :: Double)



main :: IO ()
main = do
  -- showGraph timeSweep sourceSweep

  go "test" (test (timeSweep customOne) (sourceSweep customOne))

