{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module EFA.Application.AbsoluteState (
   module EFA.Application.AbsoluteState,
   (=.=),
   ) where

-- import as  import qualified EFA.Application.AbsoluteState as AbsoluteState

import qualified EFA.Application.Symbolic as Symbolic

import qualified EFA.Equation.Record as Record
--import qualified EFA.Equation.System as EqGen
--import qualified EFA.Equation.Environment as Env
import qualified EFA.Graph.StateFlow.EquationSystem as EqGen
import qualified EFA.Graph.StateFlow.Environment as EqEnv

--import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Verify as Verify
--import EFA.Equation.System ((=.=))
import EFA.Graph.StateFlow.EquationSystem ((=.=))

import EFA.Equation.Result(Result)

import qualified EFA.Symbolic.Variable as SymVar

--import qualified EFA.Signal.Record as SigRecord
--import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Signal.Signal as Signal
--import EFA.Signal.Data (Data, Nil, (:>))

--import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Utility (Pointed)

import EFA.Report.FormatValue (FormatValue)

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System.Simple as Sys

--import qualified Data.Map as Map ;
import Data.Map (Map)
import Control.Applicative (liftA, liftA2)
--import Data.Foldable (fold)
import Data.Monoid ((<>)) --mempty


type EquationSystem = EqGen.EquationSystem Verify.Ignore Record.Absolute

type Expression node s a v x = EqGen.Expression Verify.Ignore Record.Absolute node s a v x


solve ::
   (Eq a, Arith.Constant a, a ~ Arith.Scalar v,
    Eq v, Arith.Product v, Arith.Integrate v,
    Node.C node) =>
--   Flow.RangeGraph node ->
   Topo.StateFlowGraph node ->
  (forall s. EquationSystem node s a v) ->
   EqEnv.Complete node (Result a) (Result v)
solve graph sys =
   EqEnv.completeFMap Record.unAbsolute Record.unAbsolute $
   EqGen.solve graph sys


solveSimple ::
   (Node.C node) =>
   (forall s. EquationSystem node s a v) ->
   EqEnv.Complete node (Result a) (Result v)
solveSimple sys =
   EqEnv.completeFMap Record.unAbsolute Record.unAbsolute $
   EqGen.solveSimple sys


fromGraph:: (Arith.Constant a, a ~ Arith.Scalar v, Eq v,
             Eq a, Arith.Product v,
             Arith.Product a, Arith.Integrate v, Node.C node) =>
            Bool ->
            Topo.DirStateFlowGraph node ->
            EquationSystem node s a v
fromGraph = EqGen.fromGraph

constant :: x -> Expression node s a v x
constant = EqGen.constant

variable ::
   (Eq x, Arith.Sum x, x ~ EqEnv.Element idx a v,
    EqEnv.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
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



type SignalTerm term node = Symbolic.SignalTerm Idx.Absolute term node
type ScalarTerm term node = Symbolic.ScalarTerm Idx.Absolute term node
type ScalarAtom term node = Symbolic.ScalarAtom Idx.Absolute term node

type VarTerm var term node = SymVar.VarTerm var Idx.Absolute term node

type
   SymbolicEquationSystem node s term =
      Symbolic.EquationSystem Verify.Ignore Record.Absolute node s term

symbol ::
   (SymVar.Symbol var, Pointed term) =>
   var node -> VarTerm var term node
symbol = SymVar.symbol . Idx.absolute

{-
givenSymbol ::
  (t ~ VarTerm var term node,
   Eq t, Arith.Sum t,
   t ~ EqEnv.Element idx (ScalarTerm term node) (SignalTerm term node),
   Ord (idx node), FormatValue (idx node), Pointed term,
   Var.Type idx ~ var, SymVar.Symbol var, EqEnv.AccessMap idx) =>
  idx node ->
  SymbolicEquationSystem node s term
givenSymbol idx =
   idx .= symbol (Var.index idx)
-}
{-
infixr 6 =<>

(=<>) ::
  (t ~ VarTerm var term node,
   Eq t, Arith.Sum t,
   t ~ EqEnv.Element idx (ScalarTerm term node) (SignalTerm term node),
   Ord (idx node), FormatValue (idx node), Pointed term,
   Var.Type idx ~ var, SymVar.Symbol var, EqEnv.AccessMap idx) =>
   idx node ->
   SymbolicEquationSystem node s term ->
   SymbolicEquationSystem node s term
idx =<> eqsys = givenSymbol idx <> eqsys
-}

infix 0 .=, =%%=

(.=) ::
  (Eq x, Arith.Sum x, x ~ EqEnv.Element idx a v,
   EqEnv.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   idx node -> x ->
   EquationSystem node s a v
evar .= val  =  variable evar =.= EqGen.constant val



(=%%=) ::
  (Eq x, Arith.Sum x, x ~ EqEnv.Element idx a v,
  EqEnv.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
  idx node -> idx node -> EquationSystem node s a v
(=%%=) = (EqGen.=%%=)


{-
envFromFlowRecord ::
   (Ord node) =>
   SD.SequData (SigRecord.DTimeFlowRecord node v a) ->
   EqEnv.Signal node (Data (v :> Nil) a)
envFromFlowRecord =
   fold .
   SD.mapWithSection
      (\section (SigRecord.Record times signals) ->
         mempty {
            EqEnv.dtimeMap =
               Map.singleton (XIdx.dTime section) (Signal.unpack times),
            EqEnv.powerMap =
               Map.mapKeys
                  (\(Idx.PPos x) -> Idx.InPart section $ Idx.Power x) $
               fmap Signal.unpack signals
         })

-}
fromEnvScalar ::
   (Eq a, Arith.Sum a, Node.C node) =>
   EqEnv.Scalar node a ->
   EquationSystem node s a v
fromEnvScalar = EqGen.fromEnvScalar . fmap Record.Absolute

fromEnvSignal ::
   (Eq v, Arith.Sum v, Node.C node) =>
   EqEnv.Signal node v ->
   EquationSystem node s a v
fromEnvSignal = EqGen.fromEnvSignal . fmap Record.Absolute

fromEnv ::
   (Eq a, Arith.Sum a, Eq v, Arith.Sum v, Node.C node) =>
   EqEnv.Complete node a v ->
   EquationSystem node s a v
fromEnv (EqEnv.Complete envScalar envSignal) =
   fromEnvScalar envScalar <> fromEnvSignal envSignal

fromMap ::
   (Arith.Sum x, x ~ EqEnv.Element idx a v,
    EqEnv.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   Map (idx node) x ->
   EquationSystem node s a v
fromMap =
   EqGen.fromMap . fmap Record.Absolute
