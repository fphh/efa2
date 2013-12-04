{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.SequenceState.EquationSystem where

import qualified EFA.Flow.Topology.EquationSystem as TopoEqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Storage.Quantity as StorageQuant
import qualified EFA.Flow.Storage.Variable as StorageVar
import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.Part.Index as PartIdx
import EFA.Flow.EquationSystem (System, (=&=))

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.SystemRecord (Expr, Record)
import EFA.Equation.Arithmetic
          (Sum, Product, (~/),
           Integrate, Scalar, Scale)

import Data.Foldable (foldMap)
import Data.Monoid ((<>))

import qualified Prelude as P
import Prelude hiding (sqrt)


data Options mode rec s a v =
   Options {
      optTopology :: TopoEqSys.Options mode rec s v,
      optStInOutSums ::
         Expr mode rec s a ->
         Expr mode rec s v ->
         System mode s
   }

optionsDefault ::
   (Verify.LocalVar mode a, Sum a, a ~ Scalar v,
    Verify.LocalVar mode v, Integrate v,
    Record rec) =>
   Options mode rec s a v
optionsDefault =
   Options {
      optTopology = TopoEqSys.optionsDefault,
      optStInOutSums = integrateSum
   }


equalInOutSums ::
   (Verify.LocalVar mode v, Record rec) =>
   Options mode rec s a v ->
   Options mode rec s a v
equalInOutSums opts =
   opts { optTopology = TopoEqSys.equalInOutSums $ optTopology opts }

independentInOutSums ::
   Options mode rec s a v ->
   Options mode rec s a v
independentInOutSums opts =
   opts { optTopology = TopoEqSys.independentInOutSums $ optTopology opts }


{- |
This option means that the sum of blue edges is integrated
in order to get the sum of red edges.
-}
integrateStInOutSums ::
   (Verify.LocalVar mode a, Sum a, a ~ Scalar v,
    Verify.LocalVar mode v, Integrate v,
    Record rec) =>
   Options mode rec s a v ->
   Options mode rec s a v
integrateStInOutSums opts =
   opts { optStInOutSums = integrateSum }

{- |
This option means that the sum of blue edges and the one of red edges must be equal.
This works only for values of the same type,
which are currently certainly only scalar types.
-}
equalStInOutSums ::
   (Verify.LocalVar mode a, Product a, Record rec) =>
   Options mode rec s a a ->
   Options mode rec s a a
equalStInOutSums opts =
   opts { optStInOutSums = (=&=) }


integrateSum ::
   (Verify.LocalVar mode a, Sum a, a ~ Scalar v,
    Verify.LocalVar mode v, Integrate v,
    Record rec,
    ra ~ Expr mode rec s a,
    rv ~ Expr mode rec s v) =>
   ra ->
   rv ->
   System mode s
integrateSum carrySum flowSum =
   carrySum =&= Arith.integrate flowSum

spreadSum ::
   (Verify.LocalVar mode a, Product a, a ~ Scalar v,
    Verify.LocalVar mode v, Sum v, Scale v,
    Record rec,
    ra ~ Expr mode rec s a,
    rv ~ Expr mode rec s v) =>
   rv ->
   ra ->
   rv ->
   System mode s
spreadSum dtime carrySum flowSum =
   Arith.scale (carrySum ~/ Arith.integrate dtime) dtime
   =&=
   flowSum


fromStorageSums ::
   (Verify.LocalVar mode a,
    Verify.LocalVar mode v,
    Record rec,
    ra ~ Expr mode rec s a,
    rv ~ Expr mode rec s v) =>
   Options mode rec s a v ->
   FlowTopo.Sums (ra, rv) ->
   System mode s
fromStorageSums opts sums =
   foldMap (uncurry $ optStInOutSums opts) (FlowTopo.sumIn sums)
   <>
   foldMap (uncurry $ optStInOutSums opts) (FlowTopo.sumOut sums)


checkedLookupOutSum ::
   (Node.C node, Ord part, PartIdx.Format part,
    part ~ StorageQuant.CarryPart carry, StorageQuant.Carry carry) =>
   StorageQuant.Graph carry a ->
   node -> PartIdx.Init part -> a
checkedLookupOutSum sg node state =
   StorageVar.checkedLookup "fromStorageSequences inStorages"
      StorageQuant.lookup node (StorageIdx.OutSum state) sg

checkedLookupInSum ::
   (Node.C node, Ord part, PartIdx.Format part,
    part ~ StorageQuant.CarryPart carry, StorageQuant.Carry carry) =>
   StorageQuant.Graph carry a ->
   node -> PartIdx.Exit part -> a
checkedLookupInSum sg node state =
   StorageVar.checkedLookup "fromStorageSequences outStorages"
      StorageQuant.lookup node (StorageIdx.InSum state) sg
