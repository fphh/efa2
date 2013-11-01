{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Symbolic.Variable where

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Flow.SequenceState.Variable as Var
import qualified EFA.Symbolic.Mixed as Term
import EFA.Utility (Pointed, point)



type
   SignalTerm recIdx term part node =
      Term.Signal term
         (Idx.Record recIdx (Var.ForStorageScalar part node))
         (Idx.Record recIdx (Var.InPartSignal part node))

type
   ScalarTerm recIdx term part node =
      Term.Scalar term
         (Idx.Record recIdx (Var.ForStorageScalar part node))
         (Idx.Record recIdx (Var.InPartSignal part node))

type
   ScalarAtom recIdx term part node =
      Term.ScalarAtom term
         (Idx.Record recIdx (Var.ForStorageScalar part node))
         (Idx.Record recIdx (Var.InPartSignal part node))


type
   VarTerm var recIdx term node =
      Term var term
         (Idx.Record recIdx (Var.ForStorageScalar (Part var) node))
         (Idx.Record recIdx (Var.InPartSignal (Part var) node))

class (var ~ Variable (Term var) (Part var)) => Symbol var where
   type Term var :: (* -> *) -> * -> * -> *
   type Part var :: *
   type Variable term part :: * -> *
   symbol ::
      Pointed term =>
      Idx.Record recIdx (var node) ->
      VarTerm var recIdx term node

instance Symbol (Var.InPartSignal part) where
   type Term (Var.InPartSignal part) = Term.Signal
   type Part (Var.InPartSignal part) = part
   type Variable Term.Signal part = Var.InPartSignal part
   symbol = Term.Signal . point

instance Symbol (Var.ForStorageScalar part) where
   type Term (Var.ForStorageScalar part) = Term.Scalar
   type Part (Var.ForStorageScalar part) = part
   type Variable Term.Scalar part = Var.ForStorageScalar part
   symbol = Term.Scalar . point . Term.ScalarVariable


varSymbol ::
   (Pointed term, Var.Index idx, Var.Type idx ~ var, Symbol var) =>
   Idx.Record recIdx (idx node) -> VarTerm var recIdx term node
varSymbol idx =
   symbol (fmap Var.index idx)
