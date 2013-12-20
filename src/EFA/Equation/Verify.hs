{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Equation.Verify where

import qualified EFA.Flow.SequenceState.Symbolic as SymVar
import qualified EFA.Flow.SequenceState.Variable as Var
import qualified EFA.Flow.Cumulated.Variable as CumVar
import qualified EFA.Flow.Topology.Variable as TopoVar

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Pair as Pair

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Format (Format)
import EFA.Utility (Pointed, point)

import qualified UniqueLogic.ST.TF.System.Simple as SysSimple
import qualified UniqueLogic.ST.TF.System as Sys
import qualified UniqueLogic.ST.TF.MonadTrans as UMT

import qualified Control.Monad.Exception.Synchronous as ME
import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.Trans.Writer (writer)
import Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Control.Monad.ST (ST)
import Control.Monad (liftM, ap, when)
import Control.Applicative (Applicative, pure, (<*>))

import qualified Prelude as P
import Prelude hiding (max, log)



data Assign output = Assign output output

type Assigns output = [Assign output]


newtype
   Track output m a =
      Track {runTrack :: ME.ExceptionalT (Exception output) (MW.WriterT (Assigns output) m) a}

data
   Exception output =
      Exception (Maybe output) output output
   deriving (Show)

instance (Monad m) => Functor (Track output m) where
   fmap = liftM

instance (Monad m) => Applicative (Track output m) where
   pure = return
   (<*>) = ap

instance (Monad m) => Monad (Track output m) where
   return = Track . UMT.point
   x >>= k  =  Track $ UMT.bind (runTrack x) (runTrack . k)


instance MT.MonadTrans (Track output) where
   lift = Track . MT.lift . MT.lift

instance UMT.C (Track output) where
   point = return
   bind = (>>=)

class LabeledNumber a where
   match :: a -> a -> Bool

instance Eq b => LabeledNumber (Pair.T a b) where
   match (Pair.Cons _ x) (Pair.Cons _ y)  =  x==y

instance
   (Format output, LabeledNumber ln, FormatValue ln) =>
      Sys.Value (Track output) ln where
   data ValueConstraint (Track output) ln =
           (LabeledNumber ln, FormatValue ln) => VerifyConstraint
   valueConstraint _ _ = VerifyConstraint

instance (Format output) => Sys.C (Track output) where
   update al av act =
      case Sys.valueConstraint al av of
         VerifyConstraint ->
            Sys.updateAndCheck
               (inconsistency Nothing)
               al av act

class Sys.Value w a => LocalVar w a where
   localVariable :: ST s (Sys.Variable w s a)

class LocalVar w a => GlobalVar w a var where
   globalVariable :: var -> ST s (Sys.Variable w s a)


type Variable output s a b = Sys.Variable (Track output) s (Pair.T a b)


type Term term recIdx node = term (RecIdx.Record recIdx (TopoVar.Signal node))

type CumTerm term recIdx node = term (RecIdx.Record recIdx (CumVar.Any node))

type MixedTerm mixedTerm recIdx part node =
        mixedTerm
           (RecIdx.Record recIdx (Var.Scalar part node))
           (RecIdx.Record recIdx (Var.Signal part node))

instance
   (Format output, FormatValue a, LabeledNumber a) =>
      LocalVar (Track output) a where
   localVariable = localVariableTracked

instance
   (GlobalRecVar output a term recIdx var,
    Format output, FormatValue term, FormatValue a, Eq a) =>
      GlobalVar (Track output)
         (Pair.T term a) (RecIdx.Record recIdx var) where
   globalVariable = globalRecordVariable


class GlobalRecVar output a term recIdx var where
   globalRecordVariable ::
      RecIdx.Record recIdx var ->
      ST s (Sys.Variable (Track output) s (Pair.T term a))

instance
   (Format output, FormatValue a, Eq a,
    Format.Record recIdx, Node.C node,
    FormatValue (Term term recIdx node), Pointed term) =>
      GlobalRecVar output a
         (Term term recIdx node)
         recIdx (TopoVar.Signal node) where
   globalRecordVariable = globalVariableTracked point

instance
   (Format output, FormatValue a, Eq a,
    Format.Record recIdx, Node.C node,
    FormatValue (CumTerm term recIdx node), Pointed term) =>
      GlobalRecVar output a
         (CumTerm term recIdx node)
         recIdx (CumVar.Any node) where
   globalRecordVariable = globalVariableTracked point

instance
   (Format output, FormatValue a, Eq a,
    Format.Record recIdx, FormatValue (var node),
    FormatValue (MixedTerm (mixedTerm term) recIdx part node),
    Pointed term, SymVar.Symbol var,
    mixedTerm ~ SymVar.Term var, part ~ SymVar.Part var) =>
      GlobalRecVar output a
         (MixedTerm (mixedTerm term) recIdx part node)
         recIdx (var node) where
   globalRecordVariable = globalVariableTracked SymVar.symbol


type Ignore = IdentityT

runIgnorant :: Ignore m a -> m a
runIgnorant = runIdentityT


instance LocalVar IdentityT a where
   localVariable = SysSimple.globalVariable

instance GlobalVar IdentityT a var where
   globalVariable _ = SysSimple.globalVariable


globalVariableTracked ::
   (Format output, Format.Record recIdx, FormatValue (idx node),
    FormatValue varTerm, FormatValue a, Eq a) =>
   (RecIdx.Record recIdx (idx node) -> varTerm) ->
   RecIdx.Record recIdx (idx node) ->
   ST s (Variable output s varTerm a)
globalVariableTracked symbol idx =
   Sys.globalVariable
      (\al av ->
         Sys.updateAndCheck (inconsistency $ Just $ formatValue idx) al av .
         logUpdate symbol idx)

localVariableTracked ::
   (Format output, FormatValue a, LabeledNumber a) =>
   ST s (Sys.Variable (Track output) s a)
localVariableTracked =
   Sys.globalVariable
      (\al av ->
         Sys.updateAndCheck (inconsistency Nothing) al av .
         mapMaybeT UMT.lift)


inconsistency ::
   (LabeledNumber a, FormatValue a,
    Monad m, Format output) =>
   Maybe output ->
   a -> a ->
   UMT.Wrap (Track output) m ()
inconsistency name old new =
   when (not $ match old new) $
   UMT.wrap $ Track $ ME.throwT $
   Exception name (formatValue old) (formatValue new)

logUpdate ::
   (Format output, Format.Record recIdx, FormatValue (idx node),
    FormatValue varTerm, FormatValue a) =>
   (RecIdx.Record recIdx (idx node) -> varTerm) ->
   RecIdx.Record recIdx (idx node) ->
   MaybeT (ST s) (Pair.T varTerm a) ->
   MaybeT (UMT.Wrap (Track output) (ST s)) (Pair.T varTerm a)
logUpdate symbol idx act = do
   tn@(Pair.Cons _ x) <- mapMaybeT UMT.lift act
   MT.lift $ UMT.wrap $ Track $ MT.lift $
      writer (Pair.Cons (symbol idx) x,
              [Assign (formatValue idx) (formatValue tn)])
