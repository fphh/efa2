{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Flow.SequenceState.Symbolic where

import qualified EFA.Flow.SequenceState.Variable as Var
import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Symbolic.Mixed as Term
import EFA.Utility (Pointed, point)



type
   SignalTerm term recIdx part node =
      Term.Signal term
         (RecIdx.Record recIdx (Var.ForStorageScalar part node))
         (RecIdx.Record recIdx (Var.InPartSignal part node))

type
   ScalarTerm term recIdx part node =
      Term.Scalar term
         (RecIdx.Record recIdx (Var.ForStorageScalar part node))
         (RecIdx.Record recIdx (Var.InPartSignal part node))

type
   ScalarAtom term recIdx part node =
      Term.ScalarAtom term
         (RecIdx.Record recIdx (Var.ForStorageScalar part node))
         (RecIdx.Record recIdx (Var.InPartSignal part node))


type
   VarTerm var term recIdx node =
      Term var term
         (RecIdx.Record recIdx (Var.ForStorageScalar (Part var) node))
         (RecIdx.Record recIdx (Var.InPartSignal (Part var) node))

class (var ~ Variable (Term var) (Part var)) => Symbol var where
   type Term var :: (* -> *) -> * -> * -> *
   type Part var :: *
   type Variable term part :: * -> *
   symbol ::
      Pointed term =>
      RecIdx.Record recIdx (var node) ->
      VarTerm var term recIdx node

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
   RecIdx.Record recIdx (idx node) -> VarTerm var term recIdx node
varSymbol idx =
   symbol (fmap Var.index idx)
