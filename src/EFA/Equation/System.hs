{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Equation.System (
  EquationSystem, Expression, RecordExpression,
  Element,
  fromGraph, fromEnvResult, fromEnv,
  solve, solveFromMeasurement, conservativelySolve,
  solveSimple,

  constant,
  constantRecord,
  liftF, liftF2,
  sqrt,

  Record, Wrap(Wrap, unwrap),

  (=.=),
  (=%=),
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

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.Variable as Var
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import qualified EFA.Report.Format as Format
import EFA.Graph (Edge(..))

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

import Control.Applicative (Applicative, pure, liftA, liftA2)
import Control.Category ((.))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.HT as LH

import qualified Data.NonEmpty as NonEmpty

import qualified Data.Foldable as Fold
import Data.Traversable (Traversable, traverse, sequenceA)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mempty, mappend, mconcat)
import Data.Ord (comparing)

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

instance (Record rec, Product a) => Product (Wrap rec a) where
   (~*) = liftE2 (~*)
   (~/) = liftE2 (~/)

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

instance (Product x) => Product (Bookkeeping rec node s a v x) where
   (~*) = liftA2 (~*)
   (~/) = liftA2 (~/)

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


class (Traversable rec, Applicative rec, Env.Record rec) => Record rec where
   newVariable ::
      (Eq a, Sum a) =>
      WriterT (System s) (ST s) (rec (Sys.Variable s a))
   equalRecord ::
      (Eq a) =>
      Wrap rec (Expr.T s a) ->
      Wrap rec (Expr.T s a) ->
      WriterT (System s) (ST s) ()
   liftE0 :: (Sum x) => x -> Wrap rec x
   liftE1 ::
      (Sum y) =>
      (x -> y) ->
      Wrap rec x -> Wrap rec y
   liftE2 ::
      (Sum z) =>
      (x -> y -> z) ->
      Wrap rec x -> Wrap rec y -> Wrap rec z


instance Record Env.Absolute where

   newVariable =
      lift $ fmap Env.Absolute Sys.globalVariable

   equalRecord (Wrap (Env.Absolute x)) (Wrap (Env.Absolute y)) =
      tell $ System (x =:= y)

   liftE0 = Wrap . Env.Absolute

   liftE1 f (Wrap (Env.Absolute x)) = Wrap $ Env.Absolute $ f x

   liftE2 f (Wrap (Env.Absolute x)) (Wrap (Env.Absolute y)) =
      Wrap $ Env.Absolute $ f x y


instance Record Env.Delta where

   newVariable = do
      vars <- lift $ sequenceA $ pure Sys.globalVariable
      tell $ System $
         Arith.ruleAdd (Env.before vars) (Env.delta vars) (Env.after vars)
      return vars

   {-
   I omit equality on the delta part since it would be redundant.
   -}
   equalRecord (Wrap recX) (Wrap recY) = do
      tell $ System (Env.before recX =:= Env.before recY)
      tell $ System (Env.after  recX =:= Env.after  recY)

   liftE0 x = Wrap $ Env.deltaCons x x

   liftE1 f (Wrap rec) =
      Wrap $
      Env.deltaCons (f $ Env.before rec) (f $ Env.after rec)

   liftE2 f (Wrap recX) (Wrap recY) =
      Wrap $
      Env.deltaCons
         (f (Env.before recX) (Env.before recY))
         (f (Env.after  recX) (Env.after  recY))


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
  EquationSystem $ do x <- xs; y <- ys; lift $ equalRecord x y


constant :: x -> Expression rec node s a v x
constant = pure . Expr.constant

constantRecord :: (Record rec) => rec x -> RecordExpression rec node s a v x
constantRecord = pure . Wrap . fmap Expr.constant


_withLocalVar ::
  (Eq x, Sum x, Record rec) =>
  (RecordExpression rec node s a v x -> EquationSystem rec node s a v) ->
  EquationSystem rec node s a v
_withLocalVar f = EquationSystem $ do
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
type
   Element idx rec s a v =
      Env.Element idx
         (rec (Sys.Variable s a))
         (rec (Sys.Variable s v))

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
   Env.RecordIndexed rec (idx node) ->
   Expression rec node s a v x
variable (Idx.Record recIdx idx) =
   fmap (Accessor.get (Env.accessRecord recIdx) . unwrap) $
   variableRecord idx


variableEdge ::
   (Eq x, Sum x,
    Env.AccessMap idx, Ord (idx node), Record rec,
    Element idx rec s a v ~ rec (Sys.Variable s x)) =>
   (Idx.SecNode node -> Idx.SecNode node -> idx node) ->
   Idx.SecNode node -> Idx.SecNode node ->
   RecordExpression rec node s a v x
variableEdge mkIdx x y = variableRecord (mkIdx x y)

power ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a v v
power = variableEdge Idx.Power

energy ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a v v
energy = variableEdge Idx.Energy

maxEnergy ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a v a
maxEnergy = variableEdge Idx.MaxEnergy

eta ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a v v
eta = variableEdge Idx.Eta

xfactor ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a v v
xfactor = variableEdge Idx.X

insum ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.SecNode node -> RecordExpression rec node s a v v
insum = variableRecord . Idx.Sum Idx.In

outsum ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.SecNode node -> RecordExpression rec node s a v v
outsum = variableRecord . Idx.Sum Idx.Out

storage ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.SecNode node -> RecordExpression rec node s a v a
storage = variableRecord . Idx.Storage

dtime ::
   (Eq v, Sum v, Record rec, Ord node) =>
   Idx.Section -> RecordExpression rec node s a v v
dtime = variableRecord . Idx.DTime


mwhen :: Monoid a => Bool -> a -> a
mwhen True t = t
mwhen False _ = mempty


join ::
   (Fold.Foldable rec) =>
   Bookkeeping rec node s a v (EquationSystem rec node s a v) ->
   EquationSystem rec node s a v
join (Bookkeeping m) =
   EquationSystem $ m >>= \(EquationSystem sys) -> sys

fromMapResult ::
   (Eq x, Sum x,
    Env.AccessMap idx, Ord (idx node), Record rec,
    Element idx rec s a v ~ rec (Sys.Variable s x)) =>
   M.Map (idx node) (rec (Result x)) ->
   EquationSystem rec node s a v
fromMapResult =
   fold .
   M.mapWithKey
      (\idx xrec ->
         join $
         fmap
            (fold .
             liftA2
                (\rx var ->
                   case rx of
                      Undetermined -> mempty
                      Determined x -> pure var =.= constant x)
                (Wrap xrec))
            (variableRecord idx))

fromEnvResult ::
   (Eq a, Sum a, Eq v, Sum v, Ord node, Record rec) =>
   Env.Complete node (rec (Result a)) (rec (Result v)) ->
   EquationSystem rec node s a v
fromEnvResult
   (Env.Complete (Env.Scalar me st) (Env.Signal e p n dt x s)) =
      mconcat $
         fromMapResult me :
         fromMapResult st :
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
   fold .
   M.mapWithKey
      (\idx xrec ->
         variableRecord idx =%= constantRecord xrec)

fromEnv ::
   (Eq a, Sum a, Eq v, Sum v, Ord node, Record rec) =>
   Env.Complete node (rec a) (rec v) ->
   EquationSystem rec node s a v
fromEnv
   (Env.Complete (Env.Scalar me st) (Env.Signal e p n dt x s)) =
      mconcat $
         fromMap me :
         fromMap st :
         fromMap e :
         fromMap p :
         fromMap n :
         fromMap dt :
         fromMap x :
         fromMap s :
         []


fromGraph ::
  (Eq a, Sum a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  Bool ->
  TD.DirSequFlowGraph node -> EquationSystem rec node s a v
fromGraph equalInOutSums g = mconcat $
  fromEdges (M.keys $ Gr.edgeLabels g) :
  fromNodes equalInOutSums g :
  fromInnerStorages (Gr.lefilter (TD.isStructureEdge . fst) g) :
  fromInterStorages g :
  []

-----------------------------------------------------------------

fromEdges ::
  (Eq v, Product v, Record rec, Ord node) =>
  [Gr.Edge (Idx.SecNode node)] -> EquationSystem rec node s a v
fromEdges =
   foldMap $ \e@(Edge f t) ->
      case TD.edgeType e of
         TD.StructureEdge (Idx.StructureEdge s _nf _nt) ->
            let equ x y = energy x y =%= dtime s ~* power x y
            in  equ f t <> equ t f <>
                (power t f =%= eta f t ~* power f t)
         TD.StorageEdge _ -> energy t f =%= energy f t

fromNodes ::
  (Eq v, Product v, Record rec, Ord node) =>
  Bool ->
  TD.DirSequFlowGraph node -> EquationSystem rec node s a v
fromNodes equalInOutSums = fold . M.mapWithKey f . Gr.nodes
   where f n (ins, label, outs) =
            let -- this variable is used again in fromInnerStorages
                varsumin = insum n
                varsumout = outsum n  -- and this one, too.
                splitEqs varsum nodes =
                   foldMap
                      (splitFactors varsum (energy n) (xfactor n))
                      (NonEmpty.fetch $ S.toList nodes)
            in  -- mwhen (label /= TD.Storage) (varsumin =%= varsumout)
                mwhen equalInOutSums (varsumin =%= varsumout) -- siehe bug 2013-02-12-sum-equations-storage
                <>
                splitEqs varsumin ins
                <>
                splitEqs varsumout outs


fromInnerStorages ::
  (Eq a, Sum a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a v
fromInnerStorages =
   mconcat . concatMap (LH.mapAdjacent f) . getInnerStorages
  where f (before, _) (now, dir) =
           storage now
           =%=
           case dir of
              NoDir  -> storage before
              InDir  -> storage before ~+ integrate (insum now)
              OutDir -> storage before ~- integrate (outsum now)


data StDir = InDir
           | OutDir
           | NoDir deriving (Eq, Ord, Show)

-- Only graphs without intersection edges are allowed.
-- Storages must not have more than one in or out edge.
{-
getInnerStorages :: TD.DirSequFlowGraph -> [[(Idx.SecNode, StDir)]]
getInnerStorages = getStorages format
  where format ([n], s, []) = if TD.isDirEdge n then (s, InDir) else (s, NoDir)
        format ([], s, [n]) = if TD.isDirEdge n then (s, OutDir) else (s, NoDir)
        format ([], s, []) = (s, NoDir)
        format n@(_, _, _) = error ("getInnerStorages: " ++ show n)
-}
getInnerStorages ::
  (Node.C node) =>
  TD.DirSequFlowGraph node -> [[(Idx.SecNode node, StDir)]]
getInnerStorages = getStorages format
  where format ([_], s, []) = (s, InDir)
        format ([], s, [_]) = (s, OutDir)
        format ([], s, [])  = (s, NoDir)
        format (_, s, _)  = errorSecNode "getInnerStorages" s

type InOutFormat node = InOut (Idx.SecNode node) ()
type InOut n el = ([Gr.LNode n el], n, [Gr.LNode n el])

getStorages ::
  (Ord node) =>
  (InOutFormat node -> b) -> TD.DirSequFlowGraph node -> [[b]]
getStorages format =
  M.elems
  . fmap M.elems
  . M.fromListWith (M.unionWith (error "duplicate node"))
  . map (\(ins, (n@(Idx.SecNode sec node),_), outs) ->
            (node, M.singleton sec (format (ins,n,outs))))
  . filter TD.isStorageNode
  . Gr.mkInOutGraphFormat    -- ersetzen durch nodes


-----------------------------------------------------------------

fromInterStorages ::
  (Eq a, Sum a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a v
fromInterStorages = foldMap f . getInterStorages
  where f (dir, x) =
          case dir of
               NoDir -> mempty
               InDir -> fromInStorages x
               OutDir -> fromOutStorages x

getSection :: Idx.SecNode a -> Idx.Section
getSection (Idx.SecNode s _) = s

_getNode :: Idx.SecNode a -> a
_getNode (Idx.SecNode _ n) = n

fromInStorages ::
  (Eq a, Sum a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  ([Idx.SecNode node], Idx.SecNode node, [Idx.SecNode node]) ->
  EquationSystem rec node s a v
fromInStorages (_, n, outs) =
   flip foldMap
      (fmap (NonEmpty.sortBy (comparing getSection)) $
       NonEmpty.fetch outs) $ \souts ->
         -- The next equation is special for the initial Section.
         (maxEnergy n (NonEmpty.head souts) =%=
          if getSection n == Idx.initSection
            then storage n
            else integrate (insum n))
         <>
         let f beforeNext next =
                maxEnergy n next =%=
                   maxEnergy n beforeNext ~- integrate (energy beforeNext n)
         in  mconcat $ LH.mapAdjacent f $ NonEmpty.flatten souts

fromOutStorages ::
  (Eq v, Product v, Record rec, Node.C node) =>
  ([Idx.SecNode node], Idx.SecNode node, [Idx.SecNode node]) ->
  EquationSystem rec node s a v
fromOutStorages (ins0, n, _) =
  flip foldMap (NonEmpty.fetch ins0) $ \ins ->
{-
  withLocalVar $ \s ->
    splitFactors s (flip maxEnergy n) (xfactor n) ins
    <>
-}
    splitFactors (outsum n) (energy n) (xfactor n) ins

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


getInterStorages ::
  (Node.C node) =>
  TD.DirSequFlowGraph node
  -> [(StDir, ([Idx.SecNode node], Idx.SecNode node, [Idx.SecNode node]))]
getInterStorages = concat . getStorages (format . toSecNode)
  where toSecNode (ins, n, outs) = (map fst ins, n, map fst outs)
        format x@(ins, sn@(Idx.SecNode sec _), outs) =
          case (filter h ins, filter h outs) of
               ([], [])  ->  -- We treat initial storages as in-storages
                 if sec == Idx.initSection then (InDir, x) else (NoDir, x)
               ([_], []) -> (InDir, x)
               ([], [_]) -> (OutDir, x)
               _ -> errorSecNode "getInterStorages" sn
          where h s = getSection s == sec

errorSecNode :: Node.C node => String -> Idx.SecNode node -> a
errorSecNode name node =
   error (name ++ ": " ++ Format.unUnicode (Var.formatSectionNode node))


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
  (Eq a, Sum a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  TD.SequFlowGraph node ->
  (forall s. EquationSystem rec node s a v) ->
  Env.Complete node (rec (Result a)) (rec (Result v))
solve g given =
  solveSimple (given <> fromGraph True (toDirSequFlowGraph g))


--------------------------------------------------------------------


solveFromMeasurement ::
  (Eq a, Sum a, a ~ Scalar v,
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
--     (varsumin =%= varsumout)
--     <>
-- (im Moment 273 und 274) auszukommentieren.

conservativelySolve ::
  (Eq a, Sum a, a ~ Scalar v,
   Eq v, Product v, Integrate v,
   Record rec, Node.C node) =>
  TD.SequFlowGraph node ->
  (forall s. EquationSystem rec node s a v) ->
  Env.Complete node (rec (Result a)) (rec (Result v))
conservativelySolve g given =
  solveSimple (given <> fromGraph True (toDirSequFlowGraph g))
  <>
  solveSimple given
