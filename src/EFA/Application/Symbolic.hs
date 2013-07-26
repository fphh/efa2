{-# LANGUAGE TypeFamilies #-}
module EFA.Application.Symbolic (
   module EFA.Application.Symbolic,
   (.=), (%=), (=%%=),
   Verify.Ignore,
   ) where

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.System ((.=), (%=), (=%%=))

import qualified EFA.Symbolic.Variable as SymVar
import EFA.Symbolic.Variable (VarTerm, Symbol, varSymbol)
import EFA.Utility (Pointed)

import EFA.Report.FormatValue (FormatValue)

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
      EqGen.EquationSystem mode rec node s
         (ScalarTerm (EqRecord.ToIndex rec) term node)
         (SignalTerm (EqRecord.ToIndex rec) term node)


given ::
  {-
  The Eq constraint is requested by unique-logic but not really needed.
  It is not easily possible to compare the terms for equal meaning
  and it is better not to compare them at all.
  We should remove the Eq constraint as soon as unique-logic allows it.
  -}
  (Verify.GlobalVar mode t recIdx var node,
   t ~ VarTerm var recIdx term node,
   Eq t, Arith.Sum t,
   t ~ Env.Element idx (ScalarTerm recIdx term node) (SignalTerm recIdx term node),
   EqGen.Record rec, recIdx ~ EqRecord.ToIndex rec,
   Ord (idx node), FormatValue (idx node), Pointed term,
   Var.Type idx ~ var, Symbol var, Env.AccessMap idx) =>
  Idx.Record recIdx (idx node) ->
  EquationSystem mode rec node s term
given idx =
   idx .= varSymbol idx


infixr 6 =<>

(=<>) ::
  (Verify.GlobalVar mode t recIdx var node,
   t ~ VarTerm var recIdx term node,
   Eq t, Arith.Sum t,
   t ~ Env.Element idx (ScalarTerm recIdx term node) (SignalTerm recIdx term node),
   EqGen.Record rec, recIdx ~ EqRecord.ToIndex rec,
   Ord (idx node), FormatValue (idx node), Pointed term,
   Var.Type idx ~ var, Symbol var, Env.AccessMap idx) =>
  Idx.Record recIdx (idx node) ->
  EquationSystem mode rec node s term ->
  EquationSystem mode rec node s term
idx =<> eqsys = given idx <> eqsys
