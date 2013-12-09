{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.SequenceState.EquationSystem where

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import EFA.Flow.EquationSystem (System, (=&=))

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.SystemRecord (Expr, Record)
import EFA.Equation.Arithmetic
          (Sum, Product, (~/), ZeroTestable,
           Integrate, Scalar, Scale)

import Data.Foldable (foldMap)
import Data.Monoid ((<>))

import qualified Prelude as P
import Prelude hiding (sqrt)


data Options mode rec s a v =
   Options {
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
      optStInOutSums = integrateSum
   }


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
   (Verify.LocalVar mode a, Product a, ZeroTestable a, a ~ Scalar v,
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
