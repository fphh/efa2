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
import EFA.Flow.Sequence.EquationSystem ((.=), (%=), (=%%=))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Flow.SequenceState.Variable as Var

import qualified UniqueLogic.ST.TF.System as Sys

import Data.Monoid ((<>))


type
   ScalarTerm recIdx term node =
      SymVar.ScalarTerm recIdx term Idx.Section node

type
   ScalarAtom recIdx term node =
      SymVar.ScalarAtom recIdx term Idx.Section node

type
   SignalTerm recIdx term node =
      SymVar.SignalTerm recIdx term Idx.Section node

type
   EquationSystem mode rec node s term =
      EqSys.EquationSystem mode rec node s
         (ScalarTerm (EqRecord.ToIndex rec) term node)
         (SignalTerm (EqRecord.ToIndex rec) term node)


given ::
   (Sys.Value mode t, t ~ VarTerm var recIdx term node,
    t ~ SeqFlow.Element idx (ScalarTerm recIdx term node) (SignalTerm recIdx term node),
    EqSys.Record rec, recIdx ~ EqRecord.ToIndex rec, Pointed term,
    Var.Type idx ~ var, Symbol var,
    SeqFlow.Lookup idx, Node.C node) =>
   RecIdx.Record recIdx (idx node) ->
   EquationSystem mode rec node s term
given idx =
   idx .= varSymbol idx


infixr 6 =<>

(=<>) ::
   (Sys.Value mode t, t ~ VarTerm var recIdx term node,
    t ~ SeqFlow.Element idx (ScalarTerm recIdx term node) (SignalTerm recIdx term node),
    EqSys.Record rec, recIdx ~ EqRecord.ToIndex rec, Pointed term,
    Var.Type idx ~ var, Symbol var,
    SeqFlow.Lookup idx, Node.C node) =>
   RecIdx.Record recIdx (idx node) ->
   EquationSystem mode rec node s term ->
   EquationSystem mode rec node s term
idx =<> eqsys = given idx <> eqsys
