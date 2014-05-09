{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Modules.Input.Setting where

import qualified Modules.Input.System as System
import Modules.Input.System(Node)

import qualified EFA.Application.Optimisation.Balance as Balance
import qualified EFA.Application.Optimisation as AppOpt

import qualified EFA.Application.Optimisation.Params as Params
import EFA.Application.Optimisation.Params(Name)

import EFA.Application.Optimisation.Sweep(Sweep)
import qualified EFA.Application.Optimisation.Sweep as Sweep

import qualified EFA.Application.Optimisation.ReqsAndDofs as ReqsAndDofs

import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.State.Quantity as StateQty

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.ConvertTable as CT

import qualified EFA.Equation.Arithmetic as Arith

import qualified Data.Map as Map; import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV
--import Data.List (transpose)
import qualified EFA.Signal.Record as Record

--import qualified EFA.Signal.ConvertTable as CT
--import qualified EFA.IO.TableParser as Table
import qualified EFA.IO.TableParserTypes as TPT
import qualified Data.NonEmpty as NonEmpty; import Data.NonEmpty ((!:))
import qualified Data.Empty as Empty
import qualified EFA.Application.Optimisation.Base as Base
import qualified EFA.Equation.Result as Result

local, rest, water, gas :: [Double]

{-
local = [0.1, 0.2 .. 3]
rest =  [0.1, 0.2 .. 3]
water = [0.1, 0.2 .. 0.8]
gas =   [0.1, 0.2 .. 0.8]
-}

{-


local = [1,2]
rest =  [3,4]
water = [5,6]
gas =   [7,8]
-}


--local = [0.1,0.3 .. 2.1] -- auch ResidualHV
--rest = [0.1, 1 .. 6.1] -- auch ResidualLV

local = [0.1,0.6 .. 6.2]
rest = [0.1,0.3 .. 2.1]

water = [0.1, 0.2 .. 0.8]
gas =   [0.1, 0.2 .. 0.8]


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


scaleTableEta :: Map Name (Double, Double)
scaleTableEta = Map.fromList $
  (System.storage,     (1, 0.9)) :
  (System.gas,         (1, 0.7)) :
  (System.transformer, (3, 0.95)) :
  (System.coal,        (7, 0.45)) :
  (System.local,       (3, 1)) :
  (System.rest,        (3, 1)) :
  []

restScale, localScale :: Double
restScale = 0.3
localScale = 0.3

forcingMap ::
  Map System.Node (Balance.SocDrive Double)
forcingMap = Map.fromList $
  (System.Water, Balance.ChargeDrive (-0.12)) :
  []

varRestPower', varLocalPower' :: [[Double]]
(varLocalPower', varRestPower') = CT.varMat local rest

-- ist die x-Achse
varRestPower1D :: Sig.PSignal UV.Vector Double
varRestPower1D = Sig.fromList rest

varLocalPower1D :: Sig.PSignal UV.Vector Double
varLocalPower1D = Sig.fromList local

varRestPower :: Sig.PSignal2 Vector UV.Vector Double
varRestPower = Sig.fromList2 $ varRestPower'

varLocalPower :: Sig.PSignal2 Vector UV.Vector Double
varLocalPower = Sig.fromList2 $ varLocalPower'


initStorageState :: (Arith.Constant a) => Params.InitStorageState System.Node a
initStorageState =
  Params.InitStorageState $ Map.fromList [(System.Water, Arith.fromRational $ 1000)] --0.7*3600*1000)]


initStorageSeq :: (Arith.Constant a) => Params.InitStorageSeq System.Node a
initStorageSeq =
  Params.InitStorageSeq $ Map.fromList [(System.Water, Arith.fromRational 1000)]

initEnv :: StateQty.Graph Node
           (Result.Result (Sweep UV.Vector Double))
           (Result.Result (Sweep UV.Vector Double))
initEnv = AppOpt.storageEdgeXFactors optParams 3 3
          $ AppOpt.initialEnv optParams System.stateFlowGraph

etaMap :: TPT.Map Double -> 
          Map Name (Params.EtaFunction Double Double)

etaMap tabEta = Map.map Params.EtaFunction $
         Map.mapKeys Params.Name $
         CT.makeEtaFunctions2D
            (Map.mapKeys Params.unName scaleTableEta)
            tabEta

reqsRec :: (SV.FromList v, SV.Storage v Double,
            SV.Convert UV.Vector v) =>
           Map String (TPT.T Double) -> Record.PowerRecord Node v Double

reqsRec tabPower =
  let (time,
       NonEmpty.Cons l
          (NonEmpty.Cons r Empty.Cons)) =
        CT.getPowerSignalsWithSameTime tabPower
          ("rest" !: "local" !: Empty.Cons)

      ctime = Sig.convert time

      pLocal = Sig.offset 0.1 . Sig.scale 3 $ Sig.convert $ l
      pRest = Sig.offset 0.2 . Sig.scale 1.3 $ Sig.convert $  r

      rRec :: Record.PowerRecord Node UV.Vector Double
      rRec = Record.Record ctime (Map.fromList (zip reqsPos [pLocal,pRest]))
--      reqsRec = Record.scatterRnd rndGen 10 0.3 $ Record.Record ctime (Map.fromList (zip reqsPos [pLocal,pRest]))

      rRecStep :: Record.PowerRecord Node UV.Vector Double
      rRecStep = Record.makeStepped rRec
  in Record.makeStepped $ Base.convertRecord rRecStep

reqsPos :: [TopoIdx.Position Node]
reqsPos = ReqsAndDofs.unReqs $ ReqsAndDofs.reqsPos reqs

{-
supportPoints =
        Base.supportPoints
          [ TopoIdx.ppos System.LocalRest System.LocalNetwork,
            TopoIdx.ppos System.Rest System.Network ]
          (Base.convertRecord reqsRecStep)
          (map (Sig.findSupportPoints. Sig.untype)
               [ ModSet.varLocalPower1D, ModSet.varRestPower1D])
-}

------------------------------------------------------------------------

sysParams ::  TPT.Map Double -> Params.System Node Double
sysParams tabEta = Params.System {
  Params.systemTopology = System.topology,
  Params.labeledTopology = System.labeledTopology,
  Params.etaTable = tabEta,
  Params.etaAssignMap = System.etaAssignMap,
  Params.etaMap =etaMap tabEta,
  Params.scaleTableEta = scaleTableEta,
  Params.storagePositions = ([TopoIdx.ppos System.Water System.Network]),
  Params.initStorageState = initStorageState,
  Params.initStorageSeq = initStorageSeq }

optParams :: Params.Optimisation Node [] Sweep UV.Vector Double
optParams = Params.Optimisation {
  Params.reqsPos = (ReqsAndDofs.reqsPos reqs),
  Params.dofsPos = (ReqsAndDofs.dofsPos dofs),
  Params.points = sweepPts,
  Params.sweepLength = sweepLength,
  Params.etaToOptimise = Nothing,
  Params.maxEtaIterations = Params.MaxEtaIterations 5,
  Params.maxBalanceIterations = Params.MaxBalanceIterations 100,
  Params.initialBattForcing =
    Balance.ForcingMap
    $ Map.fromList [(System.Water, Balance.DischargeDrive 1)],
  Params.initialBattForceStep =
    Balance.ForcingMap
    $ Map.fromList [(System.Water, Balance.ChargeDrive 0.1)],
  Params.etaThreshold = Params.EtaThreshold 0.2,
  Params.balanceThreshold = Params.BalanceThreshold 0.5,
  Params.balanceForcingSeed = Balance.ChargeDrive 0.01 }

simParams :: Record.PowerRecord Node [] Double -> Params.Simulation Node [] Double
simParams rRec = Params.Simulation {
  Params.varReqRoomPower1D = Sig.convert $ varLocalPower1D,
  Params.varReqRoomPower2D = Sig.convert $ varRestPower ,
  Params.reqsRec = rRec, -- Base.convertRecord reqsRecStep,
  Params.requirementGrid = [ Sig.convert $ varLocalPower1D,
                             Sig.convert $ varRestPower1D ],
--  Params.activeSupportPoints =  supportPoints,
  Params.sequFilterTime=0.01,
  Params.sequFilterEnergy=0 }


