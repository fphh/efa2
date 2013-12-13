{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Sequence.Symbolic (
   module EFA.Flow.Sequence.Symbolic,
   (.=), (%=), (=%%=),
   Verify.Ignore,
   ) where

import qualified EFA.Symbolic.Variable as SymVar
import EFA.Symbolic.Variable (VarTerm, Symbol, varSymbol)
import EFA.Utility (Pointed)

import qualified EFA.Flow.Sequence.EquationSystem as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.SequenceState.Variable as Var
import qualified EFA.Flow.SequenceState.Index as Idx
import EFA.Flow.Sequence.EquationSystem ((.=), (%=), (=%%=))

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Verify as Verify

import qualified UniqueLogic.ST.TF.System as Sys

import Data.Monoid ((<>))


type
   ScalarTerm term recIdx node =
      SymVar.ScalarTerm term recIdx Idx.Section node

type
   ScalarAtom term recIdx node =
      SymVar.ScalarAtom term recIdx Idx.Section node

type
   SignalTerm term recIdx node =
      SymVar.SignalTerm term recIdx Idx.Section node

type
   EquationSystem mode rec node s term =
      EqSys.EquationSystem mode rec node s
         (ScalarTerm term (EqRecord.ToIndex rec) node)
         (SignalTerm term (EqRecord.ToIndex rec) node)


given ::
   (Sys.Value mode t, t ~ VarTerm var term recIdx node,
    t ~ SeqFlow.Element idx (ScalarTerm term recIdx node) (SignalTerm term recIdx node),
    EqSys.Record rec, recIdx ~ EqRecord.ToIndex rec, Pointed term,
    Var.Type idx ~ var, Symbol var,
    SeqFlow.Lookup idx, Node.C node) =>
   RecIdx.Record recIdx (idx node) ->
   EquationSystem mode rec node s term
given idx =
   idx .= varSymbol idx


infixr 6 =<>

(=<>) ::
   (Sys.Value mode t, t ~ VarTerm var term recIdx node,
    t ~ SeqFlow.Element idx (ScalarTerm term recIdx node) (SignalTerm term recIdx node),
    EqSys.Record rec, recIdx ~ EqRecord.ToIndex rec, Pointed term,
    Var.Type idx ~ var, Symbol var,
    SeqFlow.Lookup idx, Node.C node) =>
   RecIdx.Record recIdx (idx node) ->
   EquationSystem mode rec node s term ->
   EquationSystem mode rec node s term
idx =<> eqsys = given idx <> eqsys
