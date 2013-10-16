{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module EFA.Application.Absolute (
   module EFA.Application.Absolute,
   (=.=),
   ) where

import qualified EFA.Flow.Sequence.Index as SeqIdx

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Verify as Verify
import EFA.Equation.System ((=.=))
import EFA.Equation.Result (Result)


import qualified EFA.Signal.Record as SigRecord
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Signal as Signal
import EFA.Signal.Data (Data, Nil, (:>))

import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Equation.Arithmetic as Arith

import EFA.Report.FormatValue (FormatValue)

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System.Simple as Sys

import qualified Data.Map as Map ; import Data.Map (Map)
import Control.Applicative (liftA, liftA2)
import Data.Foldable (fold)
import Data.Monoid (mempty, (<>))


type EquationSystem = EqGen.EquationSystem Verify.Ignore Record.Absolute

type Expression node s a v x = EqGen.Expression Verify.Ignore Record.Absolute node s a v x


solve ::
   (Arith.Constant a, a ~ Arith.Scalar v,
    Arith.Product v, Arith.Integrate v,
    Node.C node) =>
   Flow.RangeGraph node ->
   (forall s. EquationSystem node s a v) ->
   Env.Complete node (Result a) (Result v)
solve graph sys =
   Env.completeFMap Record.unAbsolute Record.unAbsolute $
   EqGen.solve graph sys

solveFromMeasurement ::
   (Arith.Constant a, a ~ Arith.Scalar v,
    Arith.Product v, Arith.Integrate v,
    Node.C node) =>
   Flow.RangeGraph node ->
   (forall s. EquationSystem node s a v) ->
   Env.Complete node (Result a) (Result v)
solveFromMeasurement graph sys =
   Env.completeFMap Record.unAbsolute Record.unAbsolute $
   EqGen.solveFromMeasurement graph sys


solveSimple ::
   (Node.C node) =>
   (forall s. EquationSystem node s a v) ->
   Env.Complete node (Result a) (Result v)
solveSimple sys =
   Env.completeFMap Record.unAbsolute Record.unAbsolute $
   EqGen.solveSimple sys


fromGraph ::
  (Arith.Constant a, a ~ Arith.Scalar v, Arith.Product v,
   Arith.Product a, Arith.Integrate v, Node.C node) =>
  Bool ->
  Topo.DirSeqFlowGraph node -> EquationSystem node s a v
fromGraph = EqGen.fromGraph

constant :: x -> Expression node s a v x
constant = EqGen.constant

variable ::
   (Arith.Sum x, x ~ Env.Element idx a v,
    Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   idx node -> Expression node s a v x
variable = EqGen.variable . Idx.absolute


liftF ::
  (Arith.Sum y) =>
  (x -> y) ->
  Expression node s a v x ->
  Expression node s a v y
liftF = liftA . Expr.fromRule2 . Sys.assignment2

liftF2 ::
  (Arith.Sum z) =>
  (x -> y -> z) ->
  Expression node s a v x ->
  Expression node s a v y ->
  Expression node s a v z
liftF2 = liftA2 . Expr.fromRule3 . Sys.assignment3


infix 0 .=, =%%=

(.=) ::
  (Arith.Sum x, x ~ Env.Element idx a v,
   Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   idx node -> x ->
   EquationSystem node s a v
evar .= val  =  variable evar =.= EqGen.constant val



(=%%=) ::
  (Arith.Sum x, x ~ Env.Element idx a v,
  Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
  idx node -> idx node -> EquationSystem node s a v
(=%%=) = (EqGen.=%%=)


envFromFlowRecord ::
   (Ord node) =>
   Sequ.List (SigRecord.DTimeFlowRecord node v a) ->
   Env.Signal node (Data (v :> Nil) a)
envFromFlowRecord =
   fold .
   Sequ.mapWithSection
      (\section (SigRecord.Record times signals) ->
         mempty {
            Env.dtimeMap =
               Map.singleton (SeqIdx.dTime section) (Signal.unpack times),
            Env.powerMap =
               Map.mapKeys (SeqIdx.powerFromPPos section) $
               fmap Signal.unpack signals
         })


fromEnvScalar ::
   (Arith.Sum a, Node.C node) =>
   Env.Scalar node a ->
   EquationSystem node s a v
fromEnvScalar = EqGen.fromEnvScalar . fmap Record.Absolute

fromEnvSignal ::
   (Arith.Sum v, Node.C node) =>
   Env.Signal node v ->
   EquationSystem node s a v
fromEnvSignal = EqGen.fromEnvSignal . fmap Record.Absolute

fromEnv ::
   (Arith.Sum a, Arith.Sum v, Node.C node) =>
   Env.Complete node a v ->
   EquationSystem node s a v
fromEnv (Env.Complete envScalar envSignal) =
   fromEnvScalar envScalar <> fromEnvSignal envSignal

fromMap ::
   (Arith.Sum x, x ~ Env.Element idx a v,
    Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   Map (idx node) x ->
   EquationSystem node s a v
fromMap =
   EqGen.fromMap . fmap Record.Absolute
