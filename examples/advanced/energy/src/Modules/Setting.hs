{-# LANGUAGE KindSignatures #-}

module Modules.Setting where

import qualified Modules.System as System

import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep

import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs

import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.ConvertTable as CT

import qualified EFA.Equation.Arithmetic as Arith

import qualified Data.Map as Map; import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV



local, rest, water, gas :: [Double]


local = [0.1, 0.2 .. 3]
rest =  [0.1, 0.2 .. 3]
water = [0.1, 0.2 .. 0.8]
gas =   [0.1, 0.2 .. 0.8]

{-
local = [1,2]
rest =  [3,4]
water = [5,6]
gas =   [7,8]
-}

reqs :: ReqsAndDofs.Reqs (TopoIdx.Position System.Node, [Double])
reqs = ReqsAndDofs.Reqs $
  (TopoIdx.ppos System.LocalRest System.LocalNetwork, local) :
  (TopoIdx.ppos System.Rest System.Network,           rest) :
  []

dofs :: ReqsAndDofs.Dofs (TopoIdx.Position System.Node, [Double])
dofs = ReqsAndDofs.Dofs $
  (TopoIdx.ppos System.Network System.Water,    water) :
  (TopoIdx.ppos System.LocalNetwork System.Gas, gas) :
  []


sweepPair ::
  ReqsAndDofs.Pair ReqsAndDofs.Reqs ReqsAndDofs.Dofs
                   (TopoIdx.Position System.Node, [Double])
sweepPair = ReqsAndDofs.Pair reqs dofs


sweepPts ::
  Map [Double]
      (ReqsAndDofs.Pair (Sweep.List Sweep.Sweep UV.Vector)
                        (Sweep.List Sweep.Sweep UV.Vector) Double)
sweepPts = ReqsAndDofs.mkPts2 sweepPair

sweepLength :: Int
sweepLength = ReqsAndDofs.sweepLength sweepPair

noLegend :: Int -> String
noLegend =  (const "")

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"

scaleTableEta :: Map One.Name (Double, Double)
scaleTableEta = Map.fromList $
  (System.storage,     (4, 0.6)) :
  (System.gas,         (2, 0.7)) :
  (System.transformer, (3, 0.95)) :
  (System.coal,        (3, 0.45)) :
  (System.local,       (3, 1)) :
  (System.rest,        (3, 1)) :
  []


restScale, localScale :: Double
restScale = 0.3
localScale = 0.3

forcingMap ::
  Map System.Node (One.SocDrive Double)
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


initStorageState :: (Arith.Constant a) => One.InitStorageState System.Node a
initStorageState =
  One.InitStorageState $ Map.fromList [(System.Water, Arith.fromRational $ 0.7*3600*1000)]


initStorageSeq :: (Arith.Constant a) => One.InitStorageSeq System.Node a
initStorageSeq =
  One.InitStorageSeq $ Map.fromList [(System.Water, Arith.fromRational 1000)]

------------------------------------------------------------------------

