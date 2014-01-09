{-# LANGUAGE KindSignatures #-}

module Modules.Setting where

import qualified Modules.System as System; import Modules.System (Node)

import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import EFA.Application.Sweep (Sweep)

import qualified EFA.Application.DoubleSweep as DoubleSweep
import EFA.Application.Simulation (Name)

import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.ConvertTable as CT

import qualified EFA.Equation.Arithmetic as Arith

import qualified Data.NonEmpty as NonEmpty; import Data.NonEmpty ((!:))
import qualified Data.Empty as Empty
import qualified Data.Map as Map; import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV


local, rest, water, gas :: [Double]

local = [0.1, 0.2 .. 3]
rest =  [0.1, 0.2 .. 3]
water = [0.1, 0.2 .. 0.8]
gas =   [0.1, 0.2 .. 0.8]

type Reqs = NonEmpty.T (NonEmpty.T Empty.T)


type Dofs = NonEmpty.T (NonEmpty.T Empty.T)

--type ParamPair = DoubleSweep.Pair Reqs Dofs
type ParamPair =
  DoubleSweep.Pair (Sweep.List Sweep UV.Vector) (Sweep.List Sweep UV.Vector)

type Params node list sweep vec a =
  One.OptimalEnvParams node list sweep vec a

reqsPts :: Reqs [Double]
reqsPts = local !: rest !: Empty.Cons

dofsPts :: Dofs [Double]
dofsPts =  water !: gas !: Empty.Cons

sweepPts :: DoubleSweep.Points Reqs Dofs Double
sweepPts = DoubleSweep.Pair reqsPts dofsPts

{-
customOne ::
  (UV.Unbox a, Sweep.SweepVector vec a,
  Sweep.SweepClass sweep vec a, Arith.Constant a) =>
  sweep vec a
customOne = Sweep.fromList (replicate (Sweep.size dofsPts) Arith.one)
-}

sweepLength :: Int
sweepLength = Sweep.size dofsPts

noLegend :: Int -> String
noLegend =  (const "")

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"

scaleTableEta :: Map Name (Double, Double)
scaleTableEta = Map.fromList $
  (System.storage,     (4, 0.6)) :
  (System.gas,         (2, 0.8)) :
  (System.transformer, (3, 1)) :
  (System.coal,        (3, 1)) :
  (System.local,       (3, 1)) :
  (System.rest,        (3, 1)) :
  []


restScale, localScale :: Double
restScale = 0.3
localScale = 0.3

forcingMap ::
  Map Node (One.SocDrive Double)
forcingMap = Map.fromList $
  (System.Water, One.ChargeDrive (-0.12)) :
  []

varRestPower', varLocalPower' :: [[Double]]
(varLocalPower', varRestPower') = CT.varMat local rest

-- ist die x-Achse
varRestPower1D :: Sig.PSignal Vector Double
varRestPower1D = Sig.fromList rest

varRestPower :: Sig.PSignal2 Vector Vector Double
varRestPower = Sig.fromList2 varRestPower'

varLocalPower :: Sig.PSignal2 Vector Vector Double
varLocalPower = Sig.fromList2 varLocalPower'

dofs, reqs :: [TopoIdx.Position Node]

dofs = [ TopoIdx.ppos System.Network System.Water,
         TopoIdx.ppos System.LocalNetwork System.Gas ]

reqs = [ TopoIdx.ppos System.Rest System.Network,
         TopoIdx.ppos System.LocalRest System.LocalNetwork ]


initStorage :: (Arith.Constant a) => [(Node, a)]
initStorage = [(System.Water, Arith.fromRational $ 0.7*3600*1000)]

initStorageSeq :: (Arith.Constant a) => a
initStorageSeq = Arith.fromInteger 1000

------------------------------------------------------------------------

