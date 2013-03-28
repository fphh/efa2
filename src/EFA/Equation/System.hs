{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Equation.System (
  EquationSystem, Expression, RecordExpression,
  Element, VariableRecord,
  fromGraph, fromEnvResult, fromEnv,
  solve, solveFromMeasurement, conservativelySolve,
  solveSimple,

  constant,
  constantRecord,
  liftF, liftF2,
  sqrt,

  Record, Wrap(Wrap, unwrap),

  (=.=), (.=),
  (=%=), (%=),
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

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr

import EFA.Equation.Result(Result(..))

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, Scalar, integrate)

import EFA.Utility ((>>!))

import UniqueLogic.ST.Expression ((=:=))
import qualified UniqueLogic.ST.Expression as Expr
import qualified UniqueLogic.ST.System as Sys

import qualified Data.Accessor.Monad.Trans.State as AccessState
import qualified Data.Accessor.Basic as Accessor

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, execStateT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

import Control.Monad.ST (ST, runST)
import Control.Monad (liftM2)

import Control.Applicative (Applicative, pure, liftA, liftA2, liftA3)
import Control.Category ((.))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.HT as LH

import qualified Data.NonEmpty as NonEmpty

import qualified Data.Foldable as Fold
import Data.Traversable (Traversable, traverse, sequenceA)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mempty, mappend, mconcat)

import qualified Prelude as P
import Prelude hiding (sqrt, (.))


type
   BK rec node s a v =
      StateT (Env.Complete node (rec (Sys.Variable s a)) (rec (Sys.Variable s v)))
         (WriterT (System s) (ST s))


newtype System s = System (Sys.M s ())

instance Monoid (System s) where
   mempty = System $ return ()
   mappend (System x) (System y) = System $ x >>! y


type
   Expression rec node s a v x =
      Bookkeeping rec node s a v (Expr.T s x)

type
   RecordExpression rec node s a v x =
      Bookkeeping rec node s a v (Wrap rec (Expr.T s x))

newtype Wrap rec a = Wrap {unwrap :: rec a}
   deriving (Functor, Applicative, Fold.Foldable, Traversable)


newtype
   Bookkeeping rec node s a v x =
      Bookkeeping (BK rec node s a v x)
   deriving (Functor, Applicative)

newtype
   EquationSystem rec node s a v =
      EquationSystem (BK rec node s a v ())

instance Monoid (EquationSystem rec node s a v) where
   mempty = EquationSystem $ return ()
   mappend (EquationSystem x) (EquationSystem y) =
      EquationSystem $ x >>! y


liftF ::
  (Record rec, Sum y) =>
  (x -> y) ->
  RecordExpression rec node s a v x ->
  RecordExpression rec node s a v y
liftF = liftA . liftE1 . Expr.fromRule2 . Sys.assignment2 ""

liftF2 ::
  (Record rec, Sum z) =>
  (x -> y -> z) ->
  RecordExpression rec node s a v x ->
  RecordExpression rec node s a v y ->
  RecordExpression rec node s a v z
liftF2 = liftA2 . liftE2 . Expr.fromRule3 . Sys.assignment3 ""


instance (Record rec, Sum a) => Sum (Wrap rec a) where
   (~+) = liftE2 (~+)
   (~-) = liftE2 (~-)
   negate = liftE1 Arith.negate

instance (Record rec, Product a) => Product (Wrap rec a) where
   (~*) = liftE2 (~*)
   (~/) = liftE2 (~/)
   recip = liftE1 Arith.recip

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
instance (Num a) => Sum (Expr.T s a) where
   (~+) = Expr.fromRule3 Rule.add
   (~-) = Expr.fromRule3 (\z x y -> Rule.add x y z)

instance (Fractional a) => Product (Expr.T s a) where
   (~*) = Expr.fromRule3 Rule.mul
   (~/) = Expr.fromRule3 (\z x y -> Rule.mul x y z)

instance (Fractional a) => Constant (Expr.T s a) where
   zero = Expr.constant 0
   fromInteger'  = Expr.constant . fromInteger
   fromRational' = Expr.constant . fromRational
-}


instance (Sum x) => Sum (Bookkeeping rec node s a v x) where
   (~+) = liftA2 (~+)
   (~-) = liftA2 (~-)
   negate = fmap Arith.negate

instance (Product x) => Product (Bookkeeping rec node s a v x) where
   (~*) = liftA2 (~*)
   (~/) = liftA2 (~/)
   recip = fmap Arith.recip

instance (Constant x) => Constant (Bookkeeping rec node s a v x) where
   zero = pure zero
   fromInteger  = pure . Arith.fromInteger
   fromRational = pure . Arith.fromRational

instance (Integrate x) => Integrate (Bookkeeping rec node s a v x) where
   type Scalar (Bookkeeping rec node s a v x) =
           Bookkeeping rec node s a v (Scalar x)
   integrate = fmap integrate



instance (Num x) => Num (Bookkeeping rec node s a v x) where
   fromInteger = pure . fromInteger

   (*) = liftA2 (*)
   (+) = liftA2 (+)
   (-) = liftA2 (-)

   abs = fmap abs
   signum = fmap signum


instance (Fractional x) => Fractional (Bookkeeping rec node s a v x) where
   fromRational = pure . fromRational
   (/) = liftA2 (/)

{-
instance (Floating x) => Floating (Bookkeeping rec node s a v x) where
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
   (Sum x, Floating x, Record rec) =>
   RecordExpression rec node s a v x ->
   RecordExpression rec node s a v x
sqrt = liftF P.sqrt


class (Traversable rec, Applicative rec, Record.C rec) => Record rec where
   newVariable ::
      (Eq a, Sum a) =>
      WriterT (System s) (ST s) (rec (Sys.Variable s a))
   equalRecord ::
      (Eq a) =>
      Wrap rec (Expr.T s a) ->
      Wrap rec (Expr.T s a) ->
      System s
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

   newVariable =
      lift $ fmap Record.Absolute Sys.globalVariable

   equalRecord (Wrap (Record.Absolute x)) (Wrap (Record.Absolute y)) =
      System (x =:= y)

   liftE0 = Wrap . Record.Absolute

   liftE1 f (Wrap (Record.Absolute x)) = Wrap $ Record.Absolute $ f x

   liftE2 f (Wrap (Record.Absolute x)) (Wrap (Record.Absolute y)) =
      Wrap $ Record.Absolute $ f x y


instance Record Record.Delta where

   newVariable = do
      vars <- lift $ sequenceA $ pure Sys.globalVariable
      tell $ System $
         Arith.ruleAdd (Record.before vars) (Record.delta vars) (Record.after vars)
      return vars

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

   newVariable = do
      vars <- liftA3 Record.ExtDelta newVariable newVariable newVariable
      tell $ System $ Fold.sequence_ $
         liftA3 Arith.ruleAdd
            (Record.extBefore vars)
            (Record.extDelta vars)
            (Record.extAfter vars)
      return vars

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
  (Eq x) =>
  Expression rec node s a v x -> Expression rec node s a v x ->
  EquationSystem rec node s a v
(Bookkeeping xs) =.= (Bookkeeping ys) =
  EquationSystem $ lift . tell . System =<< liftM2 (=:=) xs ys

(=%=) ::
  (Eq x, Record rec) =>
  RecordExpression rec node s a v x -> RecordExpression rec node s a v x ->
  EquationSystem rec node s a v
(Bookkeeping xs) =%= (Bookkeeping ys) =
  EquationSystem $ lift . tell =<< liftM2 equalRecord xs ys


infix 0 .=, %=, ?=

(.=) ::
   (Eq x, Arith.Sum x, Record rec,
    Element idx rec s a v ~ VariableRecord rec s x,
    Env.AccessMap idx, Ord (idx node)) =>
   Record.Indexed rec (idx node) -> x ->
   EquationSystem rec node s a v
evar .= val  =  variable evar =.= constant val

(%=) ::
   (Eq x, Arith.Sum x, Record rec,
    Element idx rec s a v ~ VariableRecord rec s x,
    Env.AccessMap idx, Ord (idx node)) =>
   idx node -> rec x ->
   EquationSystem rec node s a v
evar %= val  =  variableRecord evar =%= constantRecord val

(?=) ::
   (Eq x, Arith.Sum x, Record rec,
    Element idx rec s a v ~ VariableRecord rec s x,
    Env.AccessMap idx, Ord (idx node)) =>
   idx node -> rec (Result x) ->
   EquationSystem rec node s a v
evar ?= val  =
   join $
   fmap
      (fold .
       liftA2
          (\rx var -> foldMap (\x -> pure var =.= constant x) rx)
          (Wrap val))
      (variableRecord evar)

join ::
   Bookkeeping rec node s a v (EquationSystem rec node s a v) ->
   EquationSystem rec node s a v
join (Bookkeeping m) =
   EquationSystem $ m >>= \(EquationSystem sys) -> sys


constant :: x -> Expression rec node s a v x
constant = pure . Expr.constant

constantRecord :: (Record rec) => rec x -> RecordExpression rec node s a v x
constantRecord = pure . Wrap . fmap Expr.constant


withLocalVar ::
  (Eq x, Sum x, Record rec) =>
  (RecordExpression rec node s a v x -> EquationSystem rec node s a v) ->
  EquationSystem rec node s a v
withLocalVar f = EquationSystem $ do
   v <- lift newVariable
   case f $ pure $ Wrap $ fmap Expr.fromVariable v of
        EquationSystem act -> act


{-
With GHC-7.6 we could define a custom constraint like this one:

type
   Element idx rec s a v x =
      Env.Element idx
         (rec (Sys.Variable s a))
         (rec (Sys.Variable s v))
       ~ rec (Sys.Variable s x)
-}
{- |
I recommend to write the equality constraint like so:

> Element idx rec s a v ~ VariableRecord rec s x
-}
type
   Element idx rec s a v =
      Env.Element idx
         (rec (Sys.Variable s a))
         (rec (Sys.Variable s v))

{- |
I recommend not to expand this type synonym.
You should not rely on the fact, that we use unique-logic internally.
-}
type VariableRecord rec s x = rec (Sys.Variable s x)

variableRecord ::
   (Eq x, Sum x, Env.AccessMap idx, Ord (idx node), Record rec,
    Element idx rec s a v ~ rec (Sys.Variable s x)) =>
   idx node -> RecordExpression rec node s a v x
variableRecord idx =
  Bookkeeping $ fmap Wrap $ do
    oldMap <- AccessState.get Env.accessMap
    case M.lookup idx oldMap of
      Just var -> return $ fmap Expr.fromVariable var
      Nothing -> do
        var <- lift newVariable
        AccessState.set Env.accessMap $ M.insert idx var oldMap
        return (fmap Expr.fromVariable var)

variable ::
   (Eq x, Sum x,
    Env.AccessMap idx, Ord (idx node), Record rec,
    Element idx rec s a v ~ rec (Sys.Variable s x)) =>
   Record.Indexed rec (idx node) ->
   Expression rec node s a v x
variable (Idx.Record recIdx idx) =
   fmap (Accessor.get (Record.access recIdx) . unwrap) $
   variableRecord idx


power ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.StructureEdge node -> RecordExpression rec node s a v v
power = variableRecord . Idx.Power

energy ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.StructureEdge node -> RecordExpression rec node s a v v
energy = variableRecord . Idx.Energy

maxEnergy ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.StorageEdge node -> RecordExpression rec node s a v a
maxEnergy = variableRecord . Idx.MaxEnergy

stEnergy ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.StorageEdge node -> RecordExpression rec node s a v a
stEnergy = variableRecord . Idx.StEnergy

eta ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.StructureEdge node -> RecordExpression rec node s a v v
eta = variableRecord . Idx.Eta

xfactor ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.StructureEdge node -> RecordExpression rec node s a v v
xfactor = variableRecord . Idx.X

stxfactor ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.StorageEdge node -> RecordExpression rec node s a v a
stxfactor = variableRecord . Idx.StX

insum ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.BndNode node -> RecordExpression rec node s a v v
insum = variableRecord . Idx.Sum Idx.In

outsum ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.BndNode node -> RecordExpression rec node s a v v
outsum = variableRecord . Idx.Sum Idx.Out

stinsum ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.BndNode node -> RecordExpression rec node s a v a
stinsum = variableRecord . Idx.StSum Idx.In

stoutsum ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.BndNode node -> RecordExpression rec node s a v a
stoutsum = variableRecord . Idx.StSum Idx.Out

storage ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.BndNode node -> RecordExpression rec node s a v a
storage = variableRecord . Idx.Storage

dtime ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.Section -> RecordExpression rec node s a v v
dtime = variableRecord . Idx.DTime


mwhen :: Monoid a => Bool -> a -> a
mwhen True t = t
mwhen False _ = mempty


fromMapResult ::
   (Eq x, Sum x,
    Env.AccessMap idx, Ord (idx node), Record rec,
    Element idx rec s a v ~ rec (Sys.Variable s x)) =>
   M.Map (idx node) (rec (Result x)) ->
   EquationSystem rec node s a v
fromMapResult =
   fold . M.mapWithKey (?=)

fromEnvResult ::
   (Eq a, Sum a, Eq v, Sum v, Ord node, Record rec) =>
   Env.Complete node (rec (Result a)) (rec (Result v)) ->
   EquationSystem rec node s a v
fromEnvResult
   (Env.Complete (Env.Scalar me st se sx ss) (Env.Signal e p n dt x s)) =
      mconcat $
         fromMapResult me :
         fromMapResult st :
         fromMapResult se :
         fromMapResult sx :
         fromMapResult ss :
         fromMapResult e :
         fromMapResult p :
         fromMapResult n :
         fromMapResult dt :
         fromMapResult x :
         fromMapResult s :
         []


fromMap ::
   (Eq x, Sum x,
    Env.AccessMap idx, Ord (idx node), Record rec,
    Element idx rec s a v ~ rec (Sys.Variable s x)) =>
   M.Map (idx node) (rec x) ->
   EquationSystem rec node s a v
fromMap =
   fold . M.mapWithKey (%=)

fromEnv ::
   (Eq a, Sum a, Eq v, Sum v, Ord node, Record rec) =>
   Env.Complete node (rec a) (rec v) ->
   EquationSystem rec node s a v
fromEnv
   (Env.Complete (Env.Scalar me st se sx ss) (Env.Signal e p n dt x s)) =
      mconcat $
         fromMap me :
         fromMap st :
         fromMap se :
         fromMap sx :
         fromMap ss :
         fromMap e :
         fromMap p :
         fromMap n :
         fromMap dt :
         fromMap x :
         fromMap s :
         []


fromGraph ::
  (Eq a, Product a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  Bool ->
  TD.DirSequFlowGraph node -> EquationSystem rec node s a v
fromGraph equalInOutSums g = mconcat $
  fromEdges (M.keys $ Gr.edgeLabels g) :
  fromNodes equalInOutSums g :
  fromStorageSequences g :
  []

-----------------------------------------------------------------

fromEdges ::
  (Eq a, Sum a, Eq v, Product v, Record rec, Ord node) =>
  [Gr.Edge (Idx.BndNode node)] -> EquationSystem rec node s a v
fromEdges =
   foldMap $ \se ->
      case TD.edgeType se of
         TD.StructureEdge e@(Idx.StructureEdge s _nf _nt) ->
            let equ xy = energy xy =%= dtime s ~* power xy
            in  equ e <> equ (Idx.flip e) <>
                (power (Idx.flip e) =%= eta e ~* power e)
         TD.StorageEdge e -> stEnergy e =%= stEnergy (Idx.flip e)

fromNodes ::
  (Eq a, Product a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  Bool ->
  TD.DirSequFlowGraph node -> EquationSystem rec node s a v
fromNodes equalInOutSums =
  fold . M.mapWithKey f . Gr.nodes
   where f sn@(Idx.BndNode bnd _) (ins, nodeType, outs) =
            let -- these variables are used again in fromStorageSequences
                varinsum = insum sn
                varoutsum = outsum sn
                stvarinsum = stinsum sn
                stvaroutsum = stoutsum sn

                partition =
                   LH.unzipEithers .
                   map
                      (\node ->
                         case TD.edgeType $ Gr.Edge sn node of
                            TD.StructureEdge e -> Left e
                            TD.StorageEdge e -> Right e) .
                   S.toList
                (insStruct,  insStore)  = partition ins
                (outsStruct, outsStore) = partition outs
                splitStructEqs varsum edges =
                   foldMap
                      (splitFactors varsum energy xfactor)
                      (NonEmpty.fetch edges)
                splitStoreEqs varsum edges =
                   foldMap
                      (splitFactors varsum stEnergy stxfactor)
                      (NonEmpty.fetch edges)
            in  -- siehe bug 2013-02-12-sum-equations-storage
                case nodeType of
                   TD.Crossing ->
                      mwhen equalInOutSums (varinsum =%= varoutsum)
                   TD.Storage (Just dir) ->
                      let from (Idx.StorageEdge x _ _) = x
                          to (Idx.StorageEdge _ x _) = x
                          inout = (map from insStore, sn, map to outsStore)
                      in  case dir of
                             TD.In  ->
                                fromInStorages inout
                                <>
                                splitStoreEqs stvarinsum outsStore
                                <>
                                (stvarinsum =%= integrate varinsum)
                             TD.Out ->
                                fromOutStorages inout
                                <>
                                splitStoreEqs stvaroutsum insStore
                                <>
                                (stvaroutsum =%= integrate varoutsum)
                   _ -> mempty
                <>
                mwhen
                   (bnd /= Idx.initial)
                   (splitStructEqs varinsum insStruct
                    <>
                    splitStructEqs varoutsum outsStruct)


fromStorageSequences ::
  (Eq a, Sum a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a v
fromStorageSequences =
   foldMap (mconcat . LH.mapAdjacent f . M.toList) . getStorageSequences
  where f (before, _) (now, dir) =
           storage now
           =%=
           case dir of
              Nothing     -> storage before
              Just TD.In  -> storage before ~+ stinsum now
              Just TD.Out -> storage before ~- stoutsum now


-- Storages must not have more than one in or out edge.
getStorageSequences ::
  (Node.C node) =>
  TD.DirSequFlowGraph node ->
  M.Map node (M.Map (Idx.BndNode node) (Maybe TD.StoreDir))
getStorageSequences =
  foldl
     (M.unionWith (M.unionWith (error "duplicate boundary for node")))
     M.empty .
  map (\(bn@(Idx.BndNode _ n), dir) -> M.singleton n $ M.singleton bn dir) .
  M.toList . M.mapMaybe TD.maybeStorage . Gr.nodeLabels


_getBoundary :: Idx.BndNode a -> Idx.Boundary
_getBoundary (Idx.BndNode s _) = s

_getNode :: Idx.BndNode a -> a
_getNode (Idx.BndNode _ n) = n

fromInStorages ::
  (Eq a, Sum a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  ([Idx.Boundary], Idx.BndNode node, [Idx.Boundary]) ->
  EquationSystem rec node s a v
fromInStorages (_, sn@(Idx.BndNode sec n), outs) =
   flip foldMap
      (fmap NonEmpty.sort $ NonEmpty.fetch outs) $ \souts ->
         -- The next equation is special for the initial Section.
         (Idx.storageEdge maxEnergy sec (NonEmpty.head souts) n =%=
          if sec == Idx.initial
            then storage sn
            else stinsum sn)
         <>
         let f beforeNext next =
                Idx.storageEdge maxEnergy sec next n =%=
                   Idx.storageEdge maxEnergy sec beforeNext n
                      ~- Idx.storageEdge stEnergy beforeNext sec n
         in  mconcat $ LH.mapAdjacent f $ NonEmpty.flatten souts

fromOutStorages ::
  (Eq a, Product a, Record rec, Node.C node) =>
  ([Idx.Boundary], Idx.BndNode node, [Idx.Boundary]) ->
  EquationSystem rec node s a v
fromOutStorages (ins0, Idx.BndNode sec n, _) =
  flip foldMap (NonEmpty.fetch ins0) $ \ins ->
  (withLocalVar $ \s ->
    splitFactors s
      (\sect -> Idx.storageEdge maxEnergy sect sec n)
      (\sect -> Idx.storageEdge stxfactor sec sect n)
      ins)

splitFactors ::
   (Eq x, Product x, Record rec) =>
   RecordExpression rec node s a v x ->
   (secnode -> RecordExpression rec node s a v x) ->
   (secnode -> RecordExpression rec node s a v x) ->
   NonEmpty.T [] secnode -> EquationSystem rec node s a v
splitFactors s ef xf ns =
   (s =%= NonEmpty.foldl1 (~+) (fmap ef ns))
   <>
   foldMap (\n -> ef n =%= s ~* xf n) ns



-----------------------------------------------------------------


-- In principle, we could remove "dead nodes", but
-- then the storage equations would not work.
-- Therefore we should not remove "dead nodes"
-- iff they are storages.
-- Anyway, I don't remove dead nodes,
-- because it will make DirSequFlowGraph more complicated
-- or the generation of storage equations will be more complicated.

toDirSequFlowGraph ::
  (Ord node) =>
  TD.SequFlowGraph node -> TD.DirSequFlowGraph node
toDirSequFlowGraph =
  Gr.emap (const ()) . Gr.lefilter TD.isDirEdge

-----------------------------------------------------------------



queryEnv ::
  (Traversable env, Traversable rec) =>
  env (rec (Sys.Variable s a)) -> ST s (env (rec (Result a)))
queryEnv =
  traverse (traverse (fmap (maybe Undetermined Determined) . Sys.query))

solveSimple ::
  (Record rec, Node.C node) =>
  (forall s. EquationSystem rec node s a v) ->
  Env.Complete node (rec (Result a)) (rec (Result v))
solveSimple sys = runST $ do
  let EquationSystem eqsys = sys
  (Env.Complete scalmap sigmap, System eqs) <-
     runWriterT $ execStateT eqsys mempty
  Sys.solve eqs
  liftA2 Env.Complete (queryEnv scalmap) (queryEnv sigmap)

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
  (Eq a, Product a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  TD.SequFlowGraph node ->
  (forall s. EquationSystem rec node s a v) ->
  Env.Complete node (rec (Result a)) (rec (Result v))
solve g given =
  solveSimple (given <> fromGraph True (toDirSequFlowGraph g))


--------------------------------------------------------------------


solveFromMeasurement ::
  (Eq a, Product a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  TD.SequFlowGraph node ->
  (forall s. EquationSystem rec node s a v) ->
  Env.Complete node (rec (Result a)) (rec (Result v))
solveFromMeasurement g given =
  solveSimple (given <> fromGraph False (toDirSequFlowGraph g))



--------------------------------------------------------------------

-- Stellt die originalen Werte wieder her.
-- Die auf grund der Missachtung originaler Werte
-- falsch berechneten Werte bleiben aber erhalten.
-- Eine andere Lösung wäre, die Zeilen
--     (varinsum =%= varoutsum)
--     <>
-- (im Moment 273 und 274) auszukommentieren.

conservativelySolve ::
  (Eq a, Product a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  TD.SequFlowGraph node ->
  (forall s. EquationSystem rec node s a v) ->
  Env.Complete node (rec (Result a)) (rec (Result v))
conservativelySolve g given =
  solveSimple (given <> fromGraph True (toDirSequFlowGraph g))
  <>
  solveSimple given
