{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Equation.System (
  EquationSystem, Expression, RecordExpression,

  fromGraph,
  fromMapResult,
  fromEnvResult,
  fromEnvScalarResult,
  fromEnvSignalResult,
  fromMap,
  fromEnv,
  fromEnvScalar,
  fromEnvSignal,

  solve, solveFromMeasurement,

  solveSimple,
  solveTracked, solveSimpleTracked,

  constant,
  constantRecord,
  liftF, liftF2,
  sqrt,

  Record, Wrap(Wrap, unwrap),

  (=.=), (.=),
  (=%=), (%=), (=%%=),
  (?=),
  variable,
  variableRecord,
  power,
  energy,
  maxEnergy,
  eta,
  xfactor,
  insum,
  outsum,
  storage,
  dtime,
  Result(..),
  ) where

import qualified EFA.Application.Index as XIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Pair as Pair
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr

import EFA.Report.FormatValue (FormatValue)

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, Scalar, integrate)
import EFA.Equation.Result(Result(..))

import EFA.Utility ((>>!))

import UniqueLogic.ST.TF.Expression ((=:=))
import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System as Sys

import qualified Data.Accessor.Monad.Trans.State as AccessState
import qualified Data.Accessor.Basic as Accessor

import qualified Control.Monad.Exception.Synchronous as ME
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, execStateT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

import Control.Monad.ST (ST, runST)
import Control.Monad (liftM2)

import Control.Applicative (Applicative, pure, liftA, liftA2, liftA3)
import Control.Category ((.))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List.HT as ListHT
import qualified Data.List as List

import qualified Data.NonEmpty as NonEmpty

import qualified Data.Foldable as Fold

import Data.Map (Map)
import Data.Traversable (Traversable, traverse, for, sequenceA)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mempty, mappend, mconcat)
import Data.Ord.HT (comparing)

import qualified Prelude as P
import Prelude hiding (sqrt, (.))


type
   BK mode rec node s a v =
      StateT
         (Env.Complete node
            (RecordVariable mode rec s a)
            (RecordVariable mode rec s v))
         (WriterT (System mode s) (ST s))

type RecordVariable mode rec s x = rec (Sys.Variable mode s x)

newtype System mode s = System (Sys.T mode s ())

instance Monoid (System mode s) where
   mempty = System $ return ()
   mappend (System x) (System y) = System $ x >>! y


type Expr mode = Expr.T mode

type
   Expression mode rec node s a v x =
      Bookkeeping mode rec node s a v (Expr mode s x)

type
   RecordExpression mode rec node s a v x =
      Bookkeeping mode rec node s a v (Wrap rec (Expr mode s x))

newtype Wrap rec a = Wrap {unwrap :: rec a}
   deriving (Functor, Applicative, Fold.Foldable, Traversable)


newtype
   Bookkeeping mode rec node s a v x =
      Bookkeeping (BK mode rec node s a v x)
   deriving (Functor, Applicative)

newtype
   EquationSystem mode rec node s a v =
      EquationSystem (BK mode rec node s a v ())

instance Monoid (EquationSystem mode rec node s a v) where
   mempty = EquationSystem $ return ()
   mappend (EquationSystem x) (EquationSystem y) =
      EquationSystem $ x >>! y


liftF ::
  (Sys.Value mode y, Record rec, Sum y) =>
  (x -> y) ->
  RecordExpression mode rec node s a v x ->
  RecordExpression mode rec node s a v y
liftF = liftA . liftE1 . Expr.fromRule2 . Sys.assignment2

liftF2 ::
  (Sys.Value mode z, Record rec, Sum z) =>
  (x -> y -> z) ->
  RecordExpression mode rec node s a v x ->
  RecordExpression mode rec node s a v y ->
  RecordExpression mode rec node s a v z
liftF2 = liftA2 . liftE2 . Expr.fromRule3 . Sys.assignment3


instance (Record rec, Sum a) => Sum (Wrap rec a) where
   (~+) = liftE2 (~+)
   (~-) = liftE2 (~-)
   negate = liftE1 Arith.negate

instance (Record rec, Product a) => Product (Wrap rec a) where
   (~*) = liftE2 (~*)
   (~/) = liftE2 (~/)
   recip = liftE1 Arith.recip
   constOne = liftE1 Arith.constOne

instance (Record rec, Constant a) => Constant (Wrap rec a) where
   zero = pure zero
   fromInteger  = liftE0 . Arith.fromInteger
   fromRational = liftE0 . Arith.fromRational

instance
   (Record rec, Integrate v, Sum (Scalar v)) =>
      Integrate (Wrap rec v) where
   type Scalar (Wrap rec v) = Wrap rec (Scalar v)
   integrate = liftE1 integrate

{-
-- only needed for simplified arithmetic on Absolute records
instance (Num a) => Sum (Expr mode s a) where
   (~+) = Expr.fromRule3 Rule.add
   (~-) = Expr.fromRule3 (\z x y -> Rule.add x y z)

instance (Fractional a) => Product (Expr mode s a) where
   (~*) = Expr.fromRule3 Rule.mul
   (~/) = Expr.fromRule3 (\z x y -> Rule.mul x y z)

instance (Fractional a) => Constant (Expr mode s a) where
   zero = Expr.constant 0
   fromInteger'  = Expr.constant . fromInteger
   fromRational' = Expr.constant . fromRational
-}


instance (Sum x) => Sum (Bookkeeping mode rec node s a v x) where
   (~+) = liftA2 (~+)
   (~-) = liftA2 (~-)
   negate = fmap Arith.negate

instance (Product x) => Product (Bookkeeping mode rec node s a v x) where
   (~*) = liftA2 (~*)
   (~/) = liftA2 (~/)
   recip = fmap Arith.recip
   constOne = fmap Arith.constOne

instance (Constant x) => Constant (Bookkeeping mode rec node s a v x) where
   zero = pure zero
   fromInteger  = pure . Arith.fromInteger
   fromRational = pure . Arith.fromRational

instance (Integrate x) => Integrate (Bookkeeping mode rec node s a v x) where
   type Scalar (Bookkeeping mode rec node s a v x) =
           Bookkeeping mode rec node s a v (Scalar x)
   integrate = fmap integrate



instance (Num x) => Num (Bookkeeping mode rec node s a v x) where
   fromInteger = pure . fromInteger

   (*) = liftA2 (*)
   (+) = liftA2 (+)
   (-) = liftA2 (-)

   abs = fmap abs
   signum = fmap signum


instance (Fractional x) => Fractional (Bookkeeping mode rec node s a v x) where
   fromRational = pure . fromRational
   (/) = liftA2 (/)

{-
instance (Floating x) => Floating (Bookkeeping mode rec node s a v x) where
         pi = constant pi
         exp = liftF exp
         sqrt = liftF sqrt
         log = liftF log
         (**) = liftF2 (**)
         logBase = liftF2 logBase
         sin = liftF sin
         tan = liftF tan
         cos = liftF cos
         asin = liftF asin
         atan = liftF atan
         acos = liftF acos
         sinh = liftF sinh
         tanh = liftF tanh
         cosh = liftF cosh
         asinh = liftF asinh
         atanh = liftF atanh
         acosh = liftF acosh
-}

sqrt ::
   (Sys.Value mode x, Sum x, Floating x, Record rec) =>
   RecordExpression mode rec node s a v x ->
   RecordExpression mode rec node s a v x
sqrt = liftF P.sqrt


localVariable ::
   (Record rec, Verify.LocalVar mode a, Sum a) =>
   WriterT (System mode s) (ST s) (RecordVariable mode rec s a)
localVariable = do
   vars <- lift $ sequenceA $ pure Verify.localVariable
   tell $ recordRules vars
   return vars

globalVariable ::
   (Record rec, Verify.GlobalVar mode a (Record.ToIndex rec) var node,
    Var.Index idx, Var.Type idx ~ var, FormatValue (idx node),
    Sum a) =>
   idx node ->
   WriterT (System mode s) (ST s) (RecordVariable mode rec s a)
globalVariable idx = do
   vars <-
      lift $ for Record.indices $ \recIdx ->
         Verify.globalVariable $ Idx.Record recIdx idx
   tell $ recordRules vars
   return vars


class (Traversable rec, Applicative rec, Record.IndexSet rec) => Record rec where
   recordRules ::
      (Sys.Value mode a, Sum a) =>
      RecordVariable mode rec s a -> System mode s
   equalRecord ::
      (Verify.LocalVar mode a) =>
      Wrap rec (Expr mode s a) ->
      Wrap rec (Expr mode s a) ->
      System mode s
   liftE0 :: (Sum x) => x -> Wrap rec x
   liftE1 ::
      (Sum y) =>
      (x -> y) ->
      Wrap rec x -> Wrap rec y
   liftE2 ::
      (Sum z) =>
      (x -> y -> z) ->
      Wrap rec x -> Wrap rec y -> Wrap rec z


instance Record Record.Absolute where

   recordRules _ = mempty

   equalRecord (Wrap (Record.Absolute x)) (Wrap (Record.Absolute y)) =
      System (x =:= y)

   liftE0 = Wrap . Record.Absolute

   liftE1 f (Wrap (Record.Absolute x)) = Wrap $ Record.Absolute $ f x

   liftE2 f (Wrap (Record.Absolute x)) (Wrap (Record.Absolute y)) =
      Wrap $ Record.Absolute $ f x y


instance Record Record.Delta where

   recordRules vars = System $
      Arith.ruleAdd (Record.before vars) (Record.delta vars) (Record.after vars)

   {-
   I omit equality on the delta part since it would be redundant.
   -}
   equalRecord (Wrap recX) (Wrap recY) =
      System (Record.before recX =:= Record.before recY) <>
      System (Record.after  recX =:= Record.after  recY)

   liftE0 x = Wrap $ Record.deltaCons x x

   liftE1 f (Wrap rec) =
      Wrap $
      Record.deltaCons (f $ Record.before rec) (f $ Record.after rec)

   liftE2 f (Wrap recX) (Wrap recY) =
      Wrap $
      Record.deltaCons
         (f (Record.before recX) (Record.before recY))
         (f (Record.after  recX) (Record.after  recY))


-- maybe we should move this to Record, together with the 'Wrap' type
extDeltaCons ::
   (Record f, Sum a) => Wrap f a -> Wrap f a -> Record.ExtDelta f a
extDeltaCons b a =
   Record.ExtDelta {
      Record.extBefore = unwrap b,
      Record.extAfter = unwrap a,
      Record.extDelta = unwrap (a ~- b)
   }


instance (Record rec) => Record (Record.ExtDelta rec) where

   recordRules vars =
      recordRules (Record.extBefore vars) <>
      recordRules (Record.extDelta vars) <>
      recordRules (Record.extAfter vars) <>
      (System $ Fold.sequence_ $
         liftA3 Arith.ruleAdd
            (Record.extBefore vars)
            (Record.extDelta vars)
            (Record.extAfter vars))

   {-
   I omit equality on the delta part since it would be redundant.
   -}
   equalRecord (Wrap recX) (Wrap recY) =
      equalRecord (Wrap $ Record.extBefore recX) (Wrap $ Record.extBefore recY) <>
      equalRecord (Wrap $ Record.extAfter  recX) (Wrap $ Record.extAfter  recY)

   liftE0 x = Wrap $ extDeltaCons (liftE0 x) (liftE0 x)

   liftE1 f (Wrap rec) =
      Wrap $
      extDeltaCons
         (liftE1 f $ Wrap $ Record.extBefore rec)
         (liftE1 f $ Wrap $ Record.extAfter rec)

   liftE2 f (Wrap recX) (Wrap recY) =
      Wrap $
      extDeltaCons
         (liftE2 f (Wrap $ Record.extBefore recX) (Wrap $ Record.extBefore recY))
         (liftE2 f (Wrap $ Record.extAfter  recX) (Wrap $ Record.extAfter  recY))


infix 0 =.=, =%=

(=.=) ::
  (Verify.LocalVar mode x) =>
  Expression mode rec node s a v x ->
  Expression mode rec node s a v x ->
  EquationSystem mode rec node s a v
(Bookkeeping xs) =.= (Bookkeeping ys) =
  EquationSystem $ lift . tell . System =<< liftM2 (=:=) xs ys

(=%=) ::
  (Verify.LocalVar mode x, Record rec) =>
  RecordExpression mode rec node s a v x ->
  RecordExpression mode rec node s a v x ->
  EquationSystem mode rec node s a v
(Bookkeeping xs) =%= (Bookkeeping ys) =
  EquationSystem $ lift . tell =<< liftM2 equalRecord xs ys


infix 0 =%%=, .=, %=, ?=

(=%%=) ::
   (Verify.GlobalVar mode x (Record.ToIndex rec) (Var.Type idx) node,
    Arith.Sum x, Record rec, Env.Element idx a v ~ x,
    Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   idx node -> idx node ->
   EquationSystem mode rec node s a v
x =%%= y  =  variableRecord x =%= variableRecord y

(.=) ::
   (Verify.GlobalVar mode x (Record.ToIndex rec) (Var.Type idx) node,
    Arith.Sum x, Record rec, Env.Element idx a v ~ x,
    Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   Record.Indexed rec (idx node) -> x ->
   EquationSystem mode rec node s a v
evar .= val  =  variable evar =.= constant val

(%=) ::
   (Verify.GlobalVar mode x (Record.ToIndex rec) (Var.Type idx) node,
    Arith.Sum x, Record rec, Env.Element idx a v ~ x,
    Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   idx node -> rec x ->
   EquationSystem mode rec node s a v
evar %= val  =  variableRecord evar =%= constantRecord val

(?=) ::
   (Verify.GlobalVar mode x (Record.ToIndex rec) (Var.Type idx) node,
    Arith.Sum x, Record rec, Env.Element idx a v ~ x,
    Env.AccessMap idx, Ord (idx node), FormatValue (idx node)) =>
   idx node -> rec (Result x) ->
   EquationSystem mode rec node s a v
evar ?= val  =
   join $
   fmap
      (fold .
       liftA2
          (\rx var -> foldMap (\x -> pure var =.= constant x) rx)
          (Wrap val))
      (variableRecord evar)

join ::
   Bookkeeping mode rec node s a v (EquationSystem mode rec node s a v) ->
   EquationSystem mode rec node s a v
join (Bookkeeping m) =
   EquationSystem $ m >>= \(EquationSystem sys) -> sys


constant ::
  (Sys.Value mode x) =>
  x -> Expression mode rec node s a v x
constant = pure . Expr.constant

constantRecord ::
  (Sys.Value mode x, Record rec) =>
  rec x -> RecordExpression mode rec node s a v x
constantRecord = pure . Wrap . fmap Expr.constant


withLocalVar ::
  (Verify.LocalVar mode x, Sum x, Record rec) =>
  (RecordExpression mode rec node s a v x -> EquationSystem mode rec node s a v) ->
  EquationSystem mode rec node s a v
withLocalVar f = EquationSystem $ do
   v <- lift localVariable
   case f $ pure $ Wrap $ fmap Expr.fromVariable v of
        EquationSystem act -> act


newtype
   AccessMap rec node s a v idx env =
      AccessMap {
         getAccessMap ::
            (Env.Environment idx ~ env) =>
            Accessor.T
               (Env.Complete node (RecordVariable mode rec s a) (RecordVariable mode rec s v))
               (Map (idx node) (RecordVariable mode rec s (Env.Element idx a v)))
      }

accessMap ::
   (Env.AccessMap idx) =>
   Accessor.T
      (Env.Complete node (RecordVariable mode rec s a) (RecordVariable mode rec s v))
      (Map (idx node) (RecordVariable mode rec s (Env.Element idx a v)))
accessMap =
   getAccessMap $
   Env.switchPart
      (AccessMap $ Env.accessMap)
      (AccessMap $ Env.accessMap)


variableRecord ::
   (Verify.GlobalVar mode x (Record.ToIndex rec) (Var.Type idx) node,
    Sum x, x ~ Env.Element idx a v,
    Env.AccessMap idx, Ord (idx node), FormatValue (idx node), Record rec) =>
   idx node -> RecordExpression mode rec node s a v x
variableRecord idx =
  Bookkeeping $ fmap Wrap $ do
    oldMap <- AccessState.get accessMap
    case Map.lookup idx oldMap of
      Just var -> return $ fmap Expr.fromVariable var
      Nothing -> do
        var <- lift $ globalVariable idx
        AccessState.set accessMap $ Map.insert idx var oldMap
        return (fmap Expr.fromVariable var)

variable ::
   (Verify.GlobalVar mode x (Record.ToIndex rec) (Var.Type idx) node,
    Sum x, x ~ Env.Element idx a v,
    Env.AccessMap idx, Ord (idx node), FormatValue (idx node), Record rec) =>
   Record.Indexed rec (idx node) ->
   Expression mode rec node s a v x
variable (Idx.Record recIdx idx) =
   fmap (Accessor.get (Record.access recIdx) . unwrap) $
   variableRecord idx


power ::
   (Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Sum v, Record rec, Node.C node) =>
   Idx.InSection Idx.StructureEdge node -> RecordExpression mode rec node s a v v
power = variableRecord . Idx.liftInSection Idx.Power

energy ::
   (Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Sum v, Record rec, Node.C node) =>
   Idx.InSection Idx.StructureEdge node -> RecordExpression mode rec node s a v v
energy = variableRecord . Idx.liftInSection Idx.Energy

maxEnergy ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node,
    Sum a, Record rec, Node.C node) =>
   Idx.ForNode XIdx.StorageEdge node -> RecordExpression mode rec node s a v a
maxEnergy = variableRecord . Idx.liftForNode Idx.MaxEnergy

stEnergy ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node,
    Sum a, Record rec, Node.C node) =>
   Idx.ForNode XIdx.StorageEdge node -> RecordExpression mode rec node s a v a
stEnergy = variableRecord . Idx.liftForNode Idx.StEnergy

eta ::
   (Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Sum v, Record rec, Node.C node) =>
   Idx.InSection Idx.StructureEdge node -> RecordExpression mode rec node s a v v
eta = variableRecord . Idx.liftInSection Idx.Eta

xfactor ::
   (Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Sum v, Record rec, Node.C node) =>
   Idx.InSection Idx.StructureEdge node -> RecordExpression mode rec node s a v v
xfactor = variableRecord . Idx.liftInSection Idx.X

stxfactor ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node,
    Sum a, Record rec, Node.C node) =>
   Idx.ForNode XIdx.StorageTrans node -> RecordExpression mode rec node s a v a
stxfactor = variableRecord . Idx.liftForNode Idx.StX

insum ::
   (Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Sum v, Record rec, Node.C node) =>
   Idx.SecNode node -> RecordExpression mode rec node s a v v
insum = variableRecord . Idx.inSection (Idx.Sum Idx.In)

outsum ::
   (Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Sum v, Record rec, Node.C node) =>
   Idx.SecNode node -> RecordExpression mode rec node s a v v
outsum = variableRecord . Idx.inSection (Idx.Sum Idx.Out)

stinsum ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node,
    Sum a, Record rec, Node.C node) =>
   Idx.TimeNode Idx.SectionOrExit node -> RecordExpression mode rec node s a v a
stinsum = variableRecord . Idx.forNode Idx.StInSum

stoutsum ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node,
    Sum a, Record rec, Node.C node) =>
    Idx.TimeNode Idx.InitOrSection node -> RecordExpression mode rec node s a v a
stoutsum = variableRecord . Idx.forNode Idx.StOutSum

storage ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node,
    Sum a, Record rec, Node.C node) =>
   Idx.BndNode node -> RecordExpression mode rec node s a v a
storage = variableRecord . Idx.forNode Idx.Storage

dtime ::
   (Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Sum v, Record rec, Node.C node) =>
   Idx.Section -> RecordExpression mode rec node s a v v
dtime = variableRecord . flip Idx.InPart Idx.DTime


storageTransFromEdge ::
   Idx.ForNode XIdx.StorageEdge node ->
   Idx.ForNode XIdx.StorageTrans node
storageTransFromEdge =
   Idx.liftForNode Idx.storageTransFromEdge

mwhen :: Monoid a => Bool -> a -> a
mwhen True t = t
mwhen False _ = mempty


fromMapResult ::
   (Verify.GlobalVar mode x (Record.ToIndex rec) (Var.Type idx) node,
    Sum x, x ~ Env.Element idx a v,
    Env.AccessMap idx, Ord (idx node), FormatValue (idx node), Record rec) =>
   Map (idx node) (rec (Result x)) ->
   EquationSystem mode rec node s a v
fromMapResult =
   fold . Map.mapWithKey (?=)

fromEnvScalarResult ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node,
    Sum a, Node.C node, Record rec) =>
   Env.Scalar node (rec (Result a)) ->
   EquationSystem mode rec node s a v
fromEnvScalarResult (Env.Scalar me st se sx sis sos) =
      mconcat $
         fromMapResult me :
         fromMapResult st :
         fromMapResult se :
         fromMapResult sx :
         fromMapResult sis :
         fromMapResult sos :
         []

fromEnvSignalResult ::
   (Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Sum v, Node.C node, Record rec) =>
   Env.Signal node (rec (Result v)) ->
   EquationSystem mode rec node s a v
fromEnvSignalResult (Env.Signal e p n dt x s) =
      mconcat $
         fromMapResult e :
         fromMapResult p :
         fromMapResult n :
         fromMapResult dt :
         fromMapResult x :
         fromMapResult s :
         []

fromEnvResult ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node, Sum a,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node, Sum v,
    Node.C node, Record rec) =>
   Env.Complete node (rec (Result a)) (rec (Result v)) ->
   EquationSystem mode rec node s a v
fromEnvResult (Env.Complete envScalar envSignal) =
   fromEnvScalarResult envScalar <> fromEnvSignalResult envSignal


fromMap ::
   (Verify.GlobalVar mode x (Record.ToIndex rec) (Var.Type idx) node,
    Sum x, x ~ Env.Element idx a v,
    Env.AccessMap idx, Ord (idx node), FormatValue (idx node), Record rec) =>
   Map (idx node) (rec x) ->
   EquationSystem mode rec node s a v
fromMap =
   fold . Map.mapWithKey (%=)

fromEnvScalar ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node,
    Sum a, Node.C node, Record rec) =>
   Env.Scalar node (rec a) ->
   EquationSystem mode rec node s a v
fromEnvScalar (Env.Scalar me st se sx sis sos) =
      mconcat $
         fromMap me :
         fromMap st :
         fromMap se :
         fromMap sx :
         fromMap sis :
         fromMap sos :
         []

fromEnvSignal ::
   (Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
    Sum v, Node.C node, Record rec) =>
   Env.Signal node (rec v) ->
   EquationSystem mode rec node s a v
fromEnvSignal (Env.Signal e p n dt x s) =
      mconcat $
         fromMap e :
         fromMap p :
         fromMap n :
         fromMap dt :
         fromMap x :
         fromMap s :
         []

fromEnv ::
   (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node, Sum a,
    Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node, Sum v,
    Node.C node, Record rec) =>
   Env.Complete node (rec a) (rec v) ->
   EquationSystem mode rec node s a v
fromEnv (Env.Complete envScalar envSignal) =
   fromEnvScalar envScalar <> fromEnvSignal envSignal


fromGraph ::
  (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node, Constant a, a ~ Scalar v,
   Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node, Product v, Integrate v,
   Record rec, Node.C node) =>
  Bool ->
  TD.DirSequFlowGraph node -> EquationSystem mode rec node s a v
fromGraph equalInOutSums g = mconcat $
  fromEdges (Gr.edges g) :
  fromNodes equalInOutSums g :
  fromStorageSequences g :
  []

-----------------------------------------------------------------

fromEdges ::
  (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node, Sum a,
   Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node,
   Product v, Record rec, Node.C node) =>
  [TD.FlowEdge Gr.DirEdge (Idx.AugSecNode node)] ->
  EquationSystem mode rec node s a v
fromEdges =
   foldMap $ \se ->
      case TD.edgeType se of
         TD.StructureEdge edge@(Idx.InPart s _) ->
            let equ xy = energy xy =%= dtime s ~* power xy
                e = TD.structureEdgeFromDirEdge edge
            in  equ e <> equ (Idx.flip e) <>
                (power (Idx.flip e) =%= eta e ~* power e)
         TD.StorageEdge _ -> mempty

fromNodes ::
  (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node, Constant a, a ~ Scalar v,
   Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node, Product v, Integrate v,
   Record rec, Node.C node) =>
  Bool ->
  TD.DirSequFlowGraph node -> EquationSystem mode rec node s a v
fromNodes equalInOutSums =
  fold . Map.mapWithKey f . Gr.nodeEdges
   where f an (ins, nodeType, outs) =
            let msn = Idx.secNodeFromBndNode =<< Idx.bndNodeFromAugNode an
                withSecNode = flip foldMap msn

                partition =
                   ListHT.unzipEithers .
                   map
                      (\edge ->
                         case TD.edgeType edge of
                            TD.StructureEdge e -> Left $ TD.structureEdgeFromDirEdge e
                            TD.StorageEdge e -> Right e) .
                   Set.toList

                (outsStruct, outsStore) = partition outs
                (insStruct,  insStore)  = partition ins

                splitStructEqs sec varsum edges =
                   foldMap
                      (splitFactors varsum energy (Arith.constOne (dtime sec)) xfactor)
                      (NonEmpty.fetch edges)

                splitStoreEqs varsum prex edges =
                   foldMap
                      (splitFactors varsum stEnergy Arith.one
                         (stxfactor . prex . storageTransFromEdge))
                      (NonEmpty.fetch edges)

            in  case nodeType of
                   TD.Crossing ->
                      mwhen equalInOutSums $
                      withSecNode $ \sn -> insum sn =%= outsum sn
                   TD.Storage dir ->
                      flip foldMap (TD.viewNodeDir (an,dir)) $ \view ->
                         case view of
                            TD.ViewNodeIn rn ->
                                fromInStorages rn outsStore
                                <>
                                splitStoreEqs (stoutsum rn) id outsStore
                                <>
                                (withSecNode $ \sn ->
                                    stoutsum rn =%= integrate (insum sn))
                            TD.ViewNodeOut rn ->
                                fromOutStorages insStore
                                <>
                                splitStoreEqs (stinsum rn) Idx.flip insStore
                                <>
                                (withSecNode $ \sn ->
                                   stinsum rn =%= integrate (outsum sn))
                   _ -> mempty
                <>
                (withSecNode $ \sn@(Idx.TimeNode sec _) ->
                   splitStructEqs sec (insum sn) (map Idx.flip insStruct)
                   <>
                   splitStructEqs sec (outsum sn) outsStruct)


fromStorageSequences ::
  (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node, Sum a, a ~ Scalar v,
   Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node, Product v, Integrate v,
   Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem mode rec node s a v
fromStorageSequences =
   let f xs =
          let charge old now (Idx.TimeNode aug n, dir) =
                 case (aug, dir) of
                    (Idx.Exit, Just TD.Out) ->
                       old =%= withNode stinsum Idx.Exit n
                    (Idx.NoExit Idx.Init, Just TD.In) ->
                       now =%= withNode stoutsum Idx.Init n
                    (Idx.NoExit (Idx.NoInit sec), _) ->
                       now =%=
                       case dir of
                          Nothing -> old
                          Just TD.In  -> old ~+ withNode stoutsum (Idx.NoInit sec) n
                          Just TD.Out -> old ~- withNode stinsum  (Idx.NoExit sec) n
                    _ -> error "inconsistency between section and storage direction"
              storages =
                 map
                    (maybe (error "no storage content after Exit") storage .
                     Idx.bndNodeFromAugNode . fst)
                    xs
          in  mconcat $
              zipWith3 charge
                 (error "no storage content before Init" : storages) storages xs
   in  foldMap (f . Map.toList) . getStorageSequences

withNode :: (Idx.TimeNode time node -> expr) -> time -> node -> expr
withNode f sec node = f $ Idx.TimeNode sec node


-- Storages must not have more than one in or out edge.
getStorageSequences ::
  (Node.C node) =>
  TD.DirSequFlowGraph node ->
  Map node (Map (Idx.AugSecNode node) (Maybe TD.StoreDir))
getStorageSequences =
  Map.unionsWith (Map.unionWith (error "duplicate boundary for node")) .
  map (\(bn@(Idx.TimeNode _ n), dir) -> Map.singleton n $ Map.singleton bn dir) .
  Map.toList . Map.mapMaybe TD.maybeStorage . Gr.nodeLabels


fromInStorages ::
  (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node, Sum a, a ~ Scalar v,
   Verify.GlobalVar mode v (Record.ToIndex rec) Var.InSectionSignal node, Product v, Integrate v,
   Record rec, Node.C node) =>
  Idx.TimeNode Idx.InitOrSection node -> [Idx.ForNode XIdx.StorageEdge node] ->
  EquationSystem mode rec node s a v
fromInStorages sn outs =
   let toSec (Idx.ForNode (Idx.StorageEdge _ x) _) = x
       souts = List.sortBy (comparing toSec) outs
       maxEnergies = map maxEnergy souts
       stEnergies  = map stEnergy souts
   in  mconcat $
       zipWith (=%=) maxEnergies
          (stoutsum sn : zipWith (~-) maxEnergies stEnergies)

fromOutStorages ::
  (Verify.GlobalVar mode a (Record.ToIndex rec) Var.ForNodeScalar node,
   Constant a, Record rec, Node.C node) =>
  [Idx.ForNode XIdx.StorageEdge node] ->
  EquationSystem mode rec node s a v
fromOutStorages ins =
   withLocalVar $ \s ->
      foldMap
         (splitFactors s maxEnergy Arith.one
            (stxfactor . Idx.flip . storageTransFromEdge))
         (NonEmpty.fetch ins)

splitFactors ::
   (Verify.LocalVar mode x, Product x, Record rec) =>
   RecordExpression mode rec node s a v x ->
   (secnode -> RecordExpression mode rec node s a v x) ->
   RecordExpression mode rec node s a v x ->
   (secnode -> RecordExpression mode rec node s a v x) ->
   NonEmpty.T [] secnode -> EquationSystem mode rec node s a v
splitFactors s ef one xf ns =
   (s =%= NonEmpty.foldl1 (~+) (fmap ef ns))
   <>
   (one =%= NonEmpty.foldl1 (~+) (fmap xf ns))
   <>
   (foldMap (\n -> ef n =%= s ~* xf n) ns)



-----------------------------------------------------------------



queryEnv ::
  (Traversable env, Traversable rec) =>
  env (RecordVariable mode rec s a) -> ST s (env (rec (Result a)))
queryEnv =
  traverse (traverse (fmap (maybe Undetermined Determined) . Sys.query))

solveSimple ::
  (Record rec, Node.C node) =>
  (forall s. EquationSystem Verify.Ignore rec node s a v) ->
  Env.Complete node (rec (Result a)) (rec (Result v))
solveSimple sys = runST $ do
  let EquationSystem eqsys = sys
  (Env.Complete scalmap sigmap, System eqs) <-
     runWriterT $ execStateT eqsys mempty
  Verify.runIgnorant $ Sys.solve eqs
  liftA2 Env.Complete (queryEnv scalmap) (queryEnv sigmap)

solveSimpleTracked ::
  (Record rec, Node.C node) =>
  (forall s. EquationSystem (Verify.Track output) rec node s a v) ->
  (ME.Exceptional
     (Verify.Exception output)
     (Env.Complete node (rec (Result a)) (rec (Result v))),
   Verify.Assigns output)
solveSimpleTracked sys = runST $ do
  let EquationSystem eqsys = sys
  (Env.Complete scalmap sigmap, System eqs) <-
     runWriterT $ execStateT eqsys mempty
  runWriterT $ ME.runExceptionalT $ Verify.runTrack $ do
     Sys.solveBreadthFirst eqs
     liftA2 Env.Complete
        (MT.lift $ queryEnv scalmap) (MT.lift $ queryEnv sigmap)

{- |
In the input 'EquationSystem' you can pass simple variable assignments
like

> edgeVar Idx.Eta sec0 node1 node2 .= 0.42

but you may also insert complex relations like

> variableSignal (edgeVar Idx.Power sec0 node2 node1) =.=
>    square (variableSignal (edgeVar Idx.Power sec0 node1 node2))

.
-}
solve ::
  (Constant a, a ~ Scalar v,
   Product v, Integrate v,
   Record rec, Node.C node) =>
  Flow.RangeGraph node ->
  (forall s. EquationSystem Verify.Ignore rec node s a v) ->
  Env.Complete node (rec (Result a)) (rec (Result v))
solve (_rngs, g) given =
  solveSimple (given <> fromGraph True (TD.dirFromSequFlowGraph g))

solveTracked ::
  (Verify.GlobalVar (Verify.Track output) a (Record.ToIndex rec) Var.ForNodeScalar node,
   Constant a, a ~ Scalar v, a ~ Pair.T termScalar an,
   Verify.GlobalVar (Verify.Track output) v (Record.ToIndex rec) Var.InSectionSignal node,
   Product v, Integrate v, v ~ Pair.T termSignal vn,
   Record rec, Node.C node) =>
  Flow.RangeGraph node ->
  (forall s. EquationSystem (Verify.Track output) rec node s a v) ->
  (ME.Exceptional
     (Verify.Exception output)
     (Env.Complete node (rec (Result a)) (rec (Result v))),
   Verify.Assigns output)
solveTracked (_rngs, g) given =
  solveSimpleTracked (given <> fromGraph True (TD.dirFromSequFlowGraph g))


--------------------------------------------------------------------


solveFromMeasurement ::
  (Constant a, a ~ Scalar v,
   Product v, Integrate v,
   Record rec, Node.C node) =>
  Flow.RangeGraph node ->
  (forall s. EquationSystem Verify.Ignore rec node s a v) ->
  Env.Complete node (rec (Result a)) (rec (Result v))
solveFromMeasurement (_rngs, g) given =
  solveSimple (given <> fromGraph False (TD.dirFromSequFlowGraph g))
