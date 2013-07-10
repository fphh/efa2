{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
module EFA.Example.Absolute (
   module EFA.Example.Absolute,
   (=.=),
   ) where

import qualified EFA.Example.Index as XIdx
import qualified EFA.Example.Utility as Utility

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Variable as Var
import qualified EFA.Symbolic.Variable as SymVar
import EFA.Equation.System ((=.=))
import EFA.Equation.Result(Result(..))

import qualified EFA.Signal.Record as SigRecord
import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Signal as Signal
import EFA.Signal.Data (Data, Nil, (:>))

import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Utility (Pointed)

import EFA.Report.FormatValue (FormatValue)

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System.Simple as Sys

import qualified Data.Map as Map ; import Data.Map (Map)
import Control.Applicative (liftA, liftA2)
import Data.Foldable (fold)
import Data.Monoid (mempty, (<>))


type EquationSystem = EqGen.EquationSystem Utility.Ignore Record.Absolute

type Expression node s a v x = EqGen.Expression Utility.Ignore Record.Absolute node s a v x


solve ::
   (Eq a, Arith.Constant a, a ~ Arith.Scalar v,
    Eq v, Arith.Product v, Arith.Integrate v,
    Node.C node) =>
   Flow.RangeGraph node ->
   (forall s. EquationSystem node s a v) ->
   Env.Complete node (Result a) (Result v)
solve graph sys =
   Env.completeFMap Record.unAbsolute Record.unAbsolute $
   EqGen.solve graph sys


solveSimple ::
   (Node.C node) =>
   (forall s. EquationSystem node s a v) ->
   Env.Complete node (Result a) (Result v)
solveSimple sys =
   Env.completeFMap Record.unAbsolute Record.unAbsolute $
   EqGen.solveSimple sys


fromGraph ::
  (Arith.Constant a, a ~ Arith.Scalar v, Eq v,
   Eq a, Arith.Product v,
   Arith.Product a, Arith.Integrate v, Node.C node) =>
  Bool ->
  TD.DirSequFlowGraph node -> EquationSystem node s a v
fromGraph = EqGen.fromGraph

constant :: x -> Expression node s a v x
constant = EqGen.constant

variable ::
   (Eq x, Arith.Sum x, x ~ Env.Element idx a v,
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



type SignalTerm term node = SymVar.SignalTerm Idx.Absolute term node
type ScalarTerm term node = SymVar.ScalarTerm Idx.Absolute term node
type ScalarAtom term node = SymVar.ScalarAtom Idx.Absolute term node

type VarTerm var term node = SymVar.VarTerm var Idx.Absolute term node

type
   SymbolicEquationSystem node s term =
      Utility.SymbolicEquationSystem Utility.Ignore Record.Absolute node s term

symbol ::
   (SymVar.Symbol var, Pointed term) =>
   var node -> VarTerm var term node
symbol = SymVar.symbol . Idx.absolute

givenSymbol ::
  (t ~ VarTerm var term node,
   Eq t, Arith.Sum t,
   t ~ Env.Element idx (ScalarTerm term node) (SignalTerm term node),
   Ord (idx node), FormatValue (idx node), Pointed term,
   Var.Type idx ~ var, SymVar.Symbol var, Env.AccessMap idx) =>
  idx node ->
  SymbolicEquationSystem node s term
givenSymbol idx =
   idx .= symbol (Var.index idx)


infixr 6 =<>

(=<>) ::
  (t ~ VarTerm var term node,
   Eq t, Arith.Sum t,
   t ~ Env.Element idx (ScalarTerm term node) (SignalTerm term node),
   Ord (idx node), FormatValue (idx node), Pointed term,
   Var.Type idx ~ var, SymVar.Symbol var, Env.AccessMap idx) =>
   idx node ->
   SymbolicEquationSystem node s term ->
   SymbolicEquationSystem node s term
idx =<> eqsys = givenSymbol idx <> eqsys


infix 0 .=, =%%=

(.=) ::
  (Eq x, Arith.Sum x, x ~ Env.Element idx a v,
   Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   idx node -> x ->
   EquationSystem node s a v
evar .= val  =  variable evar =.= EqGen.constant val



(=%%=) ::
  (Eq x, Arith.Sum x, x ~ Env.Element idx a v,
  Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
  idx node -> idx node -> EquationSystem node s a v
(=%%=) = (EqGen.=%%=)


envFromFlowRecord ::
   (Ord node) =>
   SD.SequData (SigRecord.DTimeFlowRecord node v a) ->
   Env.Signal node (Data (v :> Nil) a)
envFromFlowRecord =
   fold .
   SD.mapWithSection
      (\section (SigRecord.Record times signals) ->
         mempty {
            Env.dtimeMap =
               Map.singleton (XIdx.dTime section) (Signal.unpack times),
            Env.powerMap =
               Map.mapKeys
                  (\(Idx.PPos x) -> Idx.InSection section $ Idx.Power x) $
               fmap Signal.unpack signals
         })


fromEnvScalar ::
   (Eq a, Arith.Sum a, Node.C node) =>
   Env.Scalar node a ->
   EquationSystem node s a v
fromEnvScalar = EqGen.fromEnvScalar . fmap Record.Absolute

fromEnvSignal ::
   (Eq v, Arith.Sum v, Node.C node) =>
   Env.Signal node v ->
   EquationSystem node s a v
fromEnvSignal = EqGen.fromEnvSignal . fmap Record.Absolute

fromEnv ::
   (Eq a, Arith.Sum a, Eq v, Arith.Sum v, Node.C node) =>
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
