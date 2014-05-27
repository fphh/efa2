{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE  TypeFamilies #-}

module EFA.Action.Optimisation.Cube.Sweep where
import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
  
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND as ND
import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology as FlowTopoPlain

import qualified EFA.Application.Utility as AppUt

import EFA.Application.Utility (quantityTopology)
--import qualified EFA.Application.Optimisation.Sweep as Sweep
import EFA.Application.Optimisation.Params (Name(Name))
import qualified EFA.Application.Optimisation.Params as Params

import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Topology.Variable as Variable
import EFA.Flow.Topology.Absolute ( (.=), 
                                    (=.=) )

--import qualified EFA.Flow.Absolute as EqAbs
import qualified EFA.Flow.Absolute as EqAbs

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Verify as Verify
--import EFA.Equation.Result (Result)
--import qualified EFA.Flow.Topology.Record as TopoRecord

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
--import qualified EFA.Flow.Topology.Quantity as TopoQty

--import qualified EFA.Signal.Vector as SV
--import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Data as Data
--import EFA.Signal.Data (Data(Data), Nil,(:>))

import qualified  UniqueLogic.ST.TF.System as ULSystem

import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Map (Map)
import Data.Monoid((<>))

import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Equation.Result as Result
import qualified EFA.Data.Collection as Collection
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.ND.Cube.Grid as CubeGrid

data Demand a
data Search a

-- TODO: Modul so verallgemeinern, dass mit verschiedenen Datentypen gesweept werden kann
modul :: ModuleName
modul = ModuleName "DoubleSweep"

nc :: FunctionName -> Caller
nc = genCaller modul


type DoubleSweepCollection inst inst1 dim dim1 label vec vec1 a =  
  CubeMap.Cube inst dim label vec a (Collection.Collection label (CubeMap.Cube inst1 dim1 label vec1 a a)) 


generateWithGrid :: 
  (Eq label, 
   Eq (vec1 a), 
   Ord label,
   DV.Walker vec,
   DV.Walker vec1,
   DV.Storage vec1 [a],
   DV.Storage vec1 (vec1 [a]),
   DV.Storage vec1 (ND.Data dim1 a),
   DV.Storage vec1 a,
   DV.Singleton vec1,
   DV.FromList vec1,
   DV.Storage vec [a],
   DV.Storage vec a,
   DV.Storage vec (vec [a]),
   DV.Storage vec (ND.Data dim a),
   DV.Storage
   vec
   (Collection.Collection
    label (CubeMap.Cube inst1 dim1 label vec1 a a)),
   DV.Singleton vec,
   DV.FromList vec) =>
  Caller ->
  (CubeGrid.Grid inst dim label vec a) -> 
  (CubeGrid.Grid inst1 dim1 label vec1 a) -> 
  CubeMap.Cube inst dim label vec a (Collection.Collection label (CubeMap.Cube inst1 dim1 label vec1 a a)) 
  
generateWithGrid caller demandGrid searchGrid = 
  if CubeGrid.haveNoCommonAxes demandGrid searchGrid then result else err
  where 
    err = merror (caller |> nc "generateWithGrid")  modul "generateWithGrid"
          "Demand and search grid must not have the same Variables"
    result = CubeMap.generateWithGrid (makeCollection) demandGrid
    makeCollection demandCoord = Collection.fromList  (nc "generateWithGrid") $ 
                                 ND.toList demandCubes ++ ND.toList searchCubes
                                       
      where
        demandCubes = ND.imap (\dimIdx axis -> (Strict.getLabel axis, 
                            CubeMap.generateWithGrid (const $ ND.unsafeLookup demandCoord dimIdx) searchGrid)) demandGrid 
        
        searchCubes = ND.imap (\dimIdx axis -> (Strict.getLabel axis, 
                            CubeMap.generateWithGrid (flip ND.unsafeLookup dimIdx) searchGrid)) searchGrid
