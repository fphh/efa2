{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Topology.NestedDelta (
   parameterSymbol, givenParameterSymbol, givenParameterNumber,
   (?=),
   ) where

import EFA.Application.NestedDelta
          (OuterExtrusion, runOuterExtrusion, parameterRecord)

import qualified EFA.Flow.Topology.Symbolic as SymVar
import qualified EFA.Flow.Topology.EquationSystem as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import EFA.Flow.Topology.EquationSystem ((?=))

import qualified EFA.Equation.Verify as Verify
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import EFA.Utility (Pointed)



parameterSymbol ::
   (Pointed term, Node.C node, t ~ SymVar.Term Idx.Delta term node,
    FlowTopo.Lookup idx) =>

   OuterExtrusion rec t ->
   idx node -> rec (Result t)

parameterSymbol param idx =
   runOuterExtrusion param
      (SymVar.varSymbol $ Idx.before idx)
      (SymVar.varSymbol $ Idx.delta  idx)


givenParameterSymbol ::
   (Verify.LocalVar mode v, EqSys.Record rec,
    Pointed term, Node.C node, SymVar.Term Idx.Delta term node ~ v,
    FlowTopo.Lookup idx) =>

   idx node ->
   OuterExtrusion rec v ->
   EqSys.EquationSystem mode rec node s v
givenParameterSymbol idx param =
   idx ?= parameterSymbol param idx


givenParameterNumber ::
   (Verify.LocalVar mode v,
    EqSys.Record rec, Node.C node, FlowTopo.Lookup idx) =>

   idx node -> v -> v ->
   OuterExtrusion rec v ->
   EqSys.EquationSystem mode rec node s v
givenParameterNumber idx x y param =
   idx ?= parameterRecord param x y
