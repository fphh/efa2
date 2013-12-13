module EFA.Flow.State.Variable  (
   Any,
   Signal, RecordSignal, signal,
   Scalar, RecordScalar, scalar,

   Var.Index, Var.Type, Var.index,
   Var.FormatIndex,
   Var.checkedLookup,
   ) where

import qualified EFA.Flow.SequenceState.Variable as Var
import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record


type Any = Var.Any Idx.State

signal :: Signal node -> Any node
signal = Var.Signal

scalar :: Scalar node -> Any node
scalar = Var.Scalar


type Signal = Var.Signal Idx.State

type RecordSignal rec node =
        RecIdx.Record (Record.ToIndex rec) (Signal node)


type Scalar = Var.Scalar Idx.State

type RecordScalar rec node =
        RecIdx.Record (Record.ToIndex rec) (Scalar node)
