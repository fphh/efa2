{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Action.Optimisation.Cube.Sweep.Access where
import EFA.Utility(Caller,
                   --merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)
import qualified EFA.Action.Optimisation.Sweep as Sweep
--import qualified EFA.Graph as Graph 
--import qualified EFA.Data.Interpolation as Interp  
import qualified EFA.Value.State as ValueState
--import qualified EFA.Action.EtaFunctions as EtaFunctions
--import qualified EFA.Action.Flow.Optimality as FlowOpt
--import qualified EFA.Action.Flow.Balance as FlowBal
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND as ND
--import qualified EFA.Data.Axis.Strict as Strict
--import qualified EFA.Flow.Topology.Record as TopoRecord
--import qualified EFA.Flow.Topology as FlowTopoPlain
--import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
--import qualified EFA.Action.Flow.Topology.Check as FlowTopoCheck
--import qualified EFA.Action.Flow.Check as ActFlowCheck
-- import qualified EFA.Flow.Topology as FlowTopo
--import qualified EFA.Data.Interpolation as Interp
--import qualified EFA.Application.Utility as AppUt
import qualified EFA.Flow.SequenceState.Index as Idx
--import qualified EFA.Action.DemandAndControl as DemandAndControl
--import qualified EFA.Data.OD.Signal.Flow as SignalFlow
--import EFA.Application.Utility (quantityTopology)
--import qualified EFA.Application.Optimisation.Sweep as Sweep
--import EFA.Application.Optimisation.Params (Name)
--import qualified EFA.Application.Optimisation.Params as Params

--import qualified EFA.Flow.Topology.Absolute as EqSys
--import qualified EFA.Flow.Topology.Quantity as TopoQty
--import qualified EFA.Flow.Topology.Index as XIdx
--import qualified EFA.Flow.Topology.Variable as Variable
--import EFA.Flow.Topology.Absolute ( (.=), 
--                                    (=.=) )

--import qualified EFA.Flow.Absolute as EqAbs
--import qualified EFA.Flow.Absolute as EqAbs

--import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Equation.RecordIndex as RecIdx
--import qualified EFA.Equation.Verify as Verify
--import qualified EFA.Equation.Result as Result
--import qualified EFA.Flow.Topology.Record as TopoRecord

--import qualified EFA.Graph.Topology.Node as Node
--import qualified EFA.Graph.Topology as Topo
--import qualified EFA.Flow.Topology.Quantity as TopoQty

--import qualified EFA.Signal.Vector as SV
--import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Data as Data
--import EFA.Signal.Data (Data(Data), Nil,(:>))

--import qualified  UniqueLogic.ST.TF.System as ULSystem

import qualified Data.Map as Map
--import qualified Data.Foldable as Fold
-- import Data.Map as (Map)
--import Data.Monoid((<>))
--import qualified EFA.Data.OrdData as OrdData
--import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Equation.Result as Result
import qualified EFA.Data.Collection as Collection
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.ND.Cube.Grid as CubeGrid
--import qualified EFA.Flow.Topology.Quantity as TopoQty

-- import qualified EFA.Action.Optimisation.Sweep as Sweep
--import qualified EFA.Action.Optimisation.Cube.Solve as CubeSolve

--import qualified Data.Maybe as Maybe
--import Control.Applicative as Applicative

--import qualified EFA.Flow.Topology as FlowTopo
import qualified Data.Set as Set


-- TODO: Modul so verallgemeinern, dass mit verschiedenen Datentypen gesweept werden kann
modul :: ModuleName
modul = ModuleName "DoubleSweep"

nc :: FunctionName -> Caller
nc = genCaller modul


--
--type SweepFlowCube = 

collectionCubeToCubeCollection ::
  (DV.LookupUnsafe demVec (Collection.Collection key b), 
   DV.Walker demVec,
   DV.Storage demVec (Collection.Collection key b),
   DV.Storage demVec b, 
   Ord key, Show key, Collection.Unpack b) =>
  CubeMap.Cube inst demDim label demVec a (Collection.Collection key b) ->
  Collection.Collection key (CubeMap.Cube inst demDim label demVec a b)
collectionCubeToCubeCollection cube = Collection.Collection grid (Map.mapWithKey f m)
  where
  (Collection.Collection _ m) = CubeMap.lookupLinUnsafe cube (CubeGrid.LinIdx 0)
  f k _ = CubeMap.getData $ CubeMap.map (Collection.lookupUnsafe k) cube
  grid = CubeMap.getGrid cube
        
extractDemandData:: 
  (DV.Storage vec a,
   DV.LookupUnsafe vec b,
   DV.Length vec,
   ND.Dimensions dim) =>
  Caller ->
  CubeMap.Cube inst dim label vec a b ->
  CubeGrid.ExtractInfo dim ->
  [b] 
extractDemandData caller sweepCube extractInfo = map f  (CubeGrid.extractIndexList (caller |> nc "extractDemandData") grid extractInfo)
  where
    grid = CubeMap.getGrid sweepCube
    f linIdx = CubeMap.lookupLinUnsafe sweepCube linIdx


extractSearchData ::
  (DV.Walker vec,
 DV.Storage vec (CubeMap.Data inst1 dim1 vec1 c),
 DV.Storage vec c,
 DV.Storage vec2 a1,
 DV.LookupUnsafe vec1 c,
 DV.Length vec2,
 ND.Dimensions dim2) =>
 Caller ->
 CubeGrid.Grid inst2 dim2 label1 vec2 a1 ->
 CubeMap.Cube inst dim label vec a (CubeMap.Data inst1 dim1 vec1 c) ->
 CubeGrid.ExtractInfo dim2 ->
 [(CubeGrid.DimIdx dim2,
 CubeMap.Cube inst dim label vec a c)] 
extractSearchData caller grid sweepCube extractInfo = map f (CubeGrid.extractIndexList (caller |> nc "extractSearchData") grid extractInfo)
  where
    f linIdx = (CubeGrid.fromLinear grid linIdx, 
                CubeMap.map (flip CubeMap.lookupLinUnsafeData linIdx) sweepCube)


stateCubeToStateCubes ::
  (DV.Storage vec b,DV.Walker vec, DV.Storage vec (ValueState.Map b),DV.Storage vec (Maybe b)) =>
  CubeMap.Cube node inst dim vec a (ValueState.Map b) ->
  ValueState.Map (CubeMap.Cube node inst dim vec a (Maybe b))
stateCubeToStateCubes cube = ValueState.Map $ Map.fromList $ zip states $ map f states
  where 
    f st = CubeMap.map (flip ValueState.lookUp st) cube
    states = getAllStates cube
  
getAllStates :: 
  (DV.Walker vec, DV.Storage vec (ValueState.Map b)) => 
  CubeMap.Cube node inst dim vec a (ValueState.Map b) -> [Maybe (Idx.AbsoluteState)]
getAllStates cube = ValueState.states $ DV.foldl (ValueState.union) (ValueState.Map Map.empty) vec 
  where vec = CubeMap.getVector $ CubeMap.getData cube


findSweepOccurences ::
  (Ord b,
   DV.Walker vec1,
   DV.Walker vec,
   DV.Storage vec1 b,
   DV.Storage vec (CubeMap.Data inst1 dim1 vec1 b)) =>
  CubeMap.Cube inst dim label vec a (CubeMap.Data inst1 dim1 vec1 b) ->
  Set.Set b 
findSweepOccurences sweepCube = 
  CubeMap.foldl (\acc xs -> CubeMap.foldlData (\a x -> flip Set.insert a x) acc xs ) Set.empty sweepCube