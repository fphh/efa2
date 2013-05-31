{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Symbolic.Variable where

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.Variable as Var
import qualified EFA.Symbolic.Mixed as Term
import EFA.Utility (Pointed, point)



type
   SignalTerm recIdx term node =
      Term.Signal term
         (Idx.Record recIdx (Var.ForNodeScalar node))
         (Idx.Record recIdx (Var.InSectionSignal node))

type
   ScalarTerm recIdx term node =
      Term.Scalar term
         (Idx.Record recIdx (Var.ForNodeScalar node))
         (Idx.Record recIdx (Var.InSectionSignal node))

type
   ScalarAtom recIdx term node =
      Term.ScalarAtom term
         (Idx.Record recIdx (Var.ForNodeScalar node))
         (Idx.Record recIdx (Var.InSectionSignal node))


type
   VarTerm var recIdx term node =
      Term var term
         (Idx.Record recIdx (Var.ForNodeScalar node))
         (Idx.Record recIdx (Var.InSectionSignal node))

class (var ~ Variable (Term var)) => Symbol var where
   type Term var :: (* -> *) -> * -> * -> *
   type Variable term :: * -> *
   symbol ::
      Pointed term =>
      Idx.Record recIdx (var node) ->
      VarTerm var recIdx term node

instance Symbol (Idx.InSection Var.Signal) where
   type Term (Idx.InSection Var.Signal) = Term.Signal
   type Variable Term.Signal = Idx.InSection Var.Signal
   symbol = Term.Signal . point

instance Symbol (Idx.ForNode Var.Scalar) where
   type Term (Idx.ForNode Var.Scalar) = Term.Scalar
   type Variable Term.Scalar = Idx.ForNode Var.Scalar
   symbol = Term.Scalar . point . Term.ScalarVariable


varSymbol ::
   (Pointed term, Var.Index idx, Var.Type idx ~ var, Symbol var) =>
   Idx.Record recIdx (idx node) -> VarTerm var recIdx term node
varSymbol idx =
   symbol (fmap Var.index idx)
