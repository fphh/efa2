{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Equation.System (
  EquationSystem, Expression, RecordExpression,
  fromTopology, solve, solveFromMeasurement, conservativelySolve,
  solveSimple,

  constant,
  constantRecord,
  liftF, liftF2,
  sqrt,

  Record, Wrap(Wrap, unwrap),

  (=.=),
  (=%=),
  getRecordVar,
  getVar,
  getEdgeVar,
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
          (Sum, zero, (~+), (~-),
           Product, (~*), (~/),
           Constant)

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
import Prelude hiding (sqrt)


type
   BK rec node s a =
      StateT (Env.Env node (rec (Sys.Variable s a)))
         (WriterT (System s) (ST s))


newtype System s = System (Sys.M s ())

instance Monoid (System s) where
   mempty = System $ return ()
   mappend (System x) (System y) = System $ x >>! y


type
   Expression rec node s a x =
      Bookkeeping rec node s a (Expr.T s x)

type
   RecordExpression rec node s a x =
      Bookkeeping rec node s a (Wrap rec (Expr.T s x))

newtype Wrap rec a = Wrap {unwrap :: rec a}
   deriving (Functor, Applicative, Fold.Foldable, Traversable)


newtype
   Bookkeeping rec node s a x =
      Bookkeeping (BK rec node s a x)
   deriving (Functor, Applicative)

newtype
   EquationSystem rec node s a =
      EquationSystem (BK rec node s a ())

instance Monoid (EquationSystem rec node s a) where
   mempty = EquationSystem $ return ()
   mappend (EquationSystem x) (EquationSystem y) =
      EquationSystem $ x >>! y


liftF ::
  (Record rec, Sum y) =>
  (x -> y) ->
  RecordExpression rec node s a x ->
  RecordExpression rec node s a y
liftF = liftA . liftE1 . Expr.fromRule2 . Sys.assignment2 ""

liftF2 ::
  (Record rec, Sum z) =>
  (x -> y -> z) ->
  RecordExpression rec node s a x ->
  RecordExpression rec node s a y ->
  RecordExpression rec node s a z
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


instance (Sum x) => Sum (Bookkeeping rec node s a x) where
   (~+) = liftA2 (~+)
   (~-) = liftA2 (~-)

instance (Product x) => Product (Bookkeeping rec node s a x) where
   (~*) = liftA2 (~*)
   (~/) = liftA2 (~/)

instance (Constant x) => Constant (Bookkeeping rec node s a x) where
   zero = pure zero
   fromInteger  = pure . Arith.fromInteger
   fromRational = pure . Arith.fromRational



instance (Num x) => Num (Bookkeeping rec node s a x) where
   fromInteger = pure . fromInteger

   (*) = liftA2 (*)
   (+) = liftA2 (+)
   (-) = liftA2 (-)

   abs = fmap abs
   signum = fmap signum


instance (Fractional x) => Fractional (Bookkeeping rec node s a x) where
   fromRational = pure . fromRational
   (/) = liftA2 (/)

{-
instance (Floating x) => Floating (Bookkeeping rec node s a x) where
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
   RecordExpression rec node s a x ->
   RecordExpression rec node s a x
sqrt = liftF P.sqrt


class (Traversable rec, Applicative rec) => Record rec where
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
  Expression rec node s a x -> Expression rec node s a x ->
  EquationSystem rec node s a
(Bookkeeping xs) =.= (Bookkeeping ys) =
  EquationSystem $ lift . tell . System =<< liftM2 (=:=) xs ys

(=%=) ::
  (Eq x, Record rec) =>
  RecordExpression rec node s a x -> RecordExpression rec node s a x ->
  EquationSystem rec node s a
(Bookkeeping xs) =%= (Bookkeeping ys) =
  EquationSystem $ do x <- xs; y <- ys; lift $ equalRecord x y


constant :: x -> Expression rec node s a x
constant = pure . Expr.constant

constantRecord :: (Record rec) => rec x -> RecordExpression rec node s a x
constantRecord = pure . Wrap . fmap Expr.constant


withLocalVar ::
  (Eq x, Sum x, Record rec) =>
  (RecordExpression rec node s a x -> EquationSystem rec node s a) ->
  EquationSystem rec node s a
withLocalVar f = EquationSystem $ do
   v <- lift newVariable
   case f $ pure $ Wrap $ fmap Expr.fromVariable v of
        EquationSystem act -> act


getRecordVar ::
   (Env.AccessMap idx, Ord (idx node), Record rec,
    Eq a, Sum a) =>
   idx node -> RecordExpression rec node s a a
getRecordVar idx =
  Bookkeeping $ fmap Wrap $ do
    oldMap <- AccessState.get Env.accessMap
    case M.lookup idx oldMap of
      Just var -> return $ fmap Expr.fromVariable var
      Nothing -> do
        var <- lift newVariable
        AccessState.set Env.accessMap $ M.insert idx var oldMap
        return (fmap Expr.fromVariable var)

getVar ::
   (Env.AccessMap idx, Ord (idx node), Record rec, Env.Record recIdx rec,
    Eq a, Sum a) =>
   Idx.Record recIdx (idx node) ->
   Expression rec node s a a
getVar (Idx.Record recIdx idx) =
   fmap (Accessor.get (Env.accessRecord recIdx) . unwrap) $ getRecordVar idx


getEdgeVar ::
   (Env.AccessMap idx, Ord (idx node), Eq a, Sum a, Record rec) =>
   (Idx.SecNode node -> Idx.SecNode node -> idx node) ->
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a a
getEdgeVar mkIdx x y = getRecordVar (mkIdx x y)


power ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a a
power = getEdgeVar Idx.Power

energy ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a a
energy = getEdgeVar Idx.Energy

maxEnergy ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a a
maxEnergy = getEdgeVar Idx.MaxEnergy

eta ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a a
eta = getEdgeVar Idx.Eta

xfactor ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.SecNode node -> Idx.SecNode node -> RecordExpression rec node s a a
xfactor = getEdgeVar Idx.X

insum ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.SecNode node -> RecordExpression rec node s a a
insum = getRecordVar . Idx.Sum Idx.In

outsum ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.SecNode node -> RecordExpression rec node s a a
outsum = getRecordVar . Idx.Sum Idx.Out

storage ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.SecNode node -> RecordExpression rec node s a a
storage = getRecordVar . Idx.Storage

dtime ::
   (Eq a, Sum a, Record rec, Ord node) =>
   Idx.Section -> RecordExpression rec node s a a
dtime = getRecordVar . Idx.DTime


_mwhen :: Monoid a => Bool -> a -> a
_mwhen True t = t
_mwhen False _ = mempty


fromTopology ::
  (Eq a, Product a, Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
fromTopology g = mconcat $
  makeInnerSectionEquations g :
  makeInterSectionEquations g :
  []

-----------------------------------------------------------------

makeInnerSectionEquations ::
  (Eq a, Product a, Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
makeInnerSectionEquations g = mconcat $
  makeEdgeEquations (M.keys $ Gr.edgeLabels g) :
  makeNodeEquations g :
  makeStorageEquations (Gr.lefilter (TD.isStructureEdge . fst) g) :
  []


makeEdgeEquations ::
  (Eq a, Product a, Record rec, Ord node) =>
  [Gr.Edge (Idx.SecNode node)] -> EquationSystem rec node s a
makeEdgeEquations =
   foldMap $ \e@(Edge f t) ->
      case TD.edgeType e of
         TD.StructureEdge (Idx.StructureEdge s _nf _nt) ->
            let equ x y = energy x y =%= dtime s ~* power x y
            in  equ f t <> equ t f <>
                (power t f =%= eta f t ~* power f t)
         TD.StorageEdge _ -> energy t f =%= energy f t

makeNodeEquations ::
  (Eq a, Product a, Record rec, Ord node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
makeNodeEquations = fold . M.mapWithKey f . Gr.nodes
   where f n (ins, label, outs) =
            let -- this variable is used again in makeStorageEquations
                varsumin = insum n
                varsumout = outsum n  -- and this one, too.
                splitEqs varsum nodes =
                   foldMap
                      (mkSplitFactorEquations varsum (energy n) (xfactor n))
                      (NonEmpty.fetch $ S.toList nodes)
            in  -- mwhen (label /= TD.Storage) (varsumin =%= varsumout)
                (varsumin =%= varsumout) -- siehe bug 2013-02-12-sum-equations-storage
                <>
                splitEqs varsumin ins
                <>
                splitEqs varsumout outs


makeStorageEquations ::
  (Eq a, Product a, Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
makeStorageEquations =
   mconcat . concatMap (LH.mapAdjacent f) . getInnersectionStorages
  where f (before, _) (now, dir) =
           storage now
           =%=
           case dir of
              NoDir  -> storage before
              InDir  -> storage before ~+ insum now
              OutDir -> storage before ~- outsum now


data StDir = InDir
           | OutDir
           | NoDir deriving (Eq, Ord, Show)

-- Only graphs without intersection edges are allowed.
-- Storages must not have more than one in or out edge.
{-
getInnersectionStorages :: TD.DirSequFlowGraph -> [[(Idx.SecNode, StDir)]]
getInnersectionStorages = getStorages format
  where format ([n], s, []) = if TD.isDirEdge n then (s, InDir) else (s, NoDir)
        format ([], s, [n]) = if TD.isDirEdge n then (s, OutDir) else (s, NoDir)
        format ([], s, []) = (s, NoDir)
        format n@(_, _, _) = error ("getInnersectionStorages: " ++ show n)
-}
getInnersectionStorages ::
  (Node.C node) =>
  TD.DirSequFlowGraph node -> [[(Idx.SecNode node, StDir)]]
getInnersectionStorages = getStorages format
  where format ([_], s, []) = (s, InDir)
        format ([], s, [_]) = (s, OutDir)
        format ([], s, [])  = (s, NoDir)
        format (_, s, _)  = errorSecNode "getInnersectionStorages" s

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

makeInterSectionEquations ::
  (Eq a, Product a, Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
makeInterSectionEquations = foldMap f . getIntersectionStorages
  where f (dir, x) =
          case dir of
               NoDir -> mempty
               InDir -> mkInStorageEquations x
               OutDir -> mkOutStorageEquations x

getSection :: Idx.SecNode a -> Idx.Section
getSection (Idx.SecNode s _) = s

_getNode :: Idx.SecNode a -> a
_getNode (Idx.SecNode _ n) = n

mkInStorageEquations ::
  (Eq a, Product a, Record rec, Node.C node) =>
  ([Idx.SecNode node], Idx.SecNode node, [Idx.SecNode node]) ->
  EquationSystem rec node s a
mkInStorageEquations (_, n, outs) =
   flip foldMap
      (fmap (NonEmpty.sortBy (comparing getSection)) $
       NonEmpty.fetch outs) $ \souts ->
         -- The next equation is special for the initial Section.
         (maxEnergy n (NonEmpty.head souts) =%=
          if getSection n == Idx.initSection
            then storage n
            else insum n)
         <>
         let f beforeNext next =
                maxEnergy n next =%=
                   maxEnergy n beforeNext ~- energy beforeNext n
         in  mconcat $ LH.mapAdjacent f $ NonEmpty.flatten souts

mkOutStorageEquations ::
  (Eq a, Product a, Record rec, Node.C node) =>
  ([Idx.SecNode node], Idx.SecNode node, [Idx.SecNode node]) ->
  EquationSystem rec node s a
mkOutStorageEquations (ins0, n, _) =
  flip foldMap (NonEmpty.fetch ins0) $ \ins ->
  withLocalVar $ \s ->
    mkSplitFactorEquations s (flip maxEnergy n) (xfactor n) ins
    <>
    mkSplitFactorEquations (outsum n) (energy n) (xfactor n) ins

mkSplitFactorEquations ::
   (Eq x, Product x, Record rec) =>
   RecordExpression rec node s a x ->
   (secnode -> RecordExpression rec node s a x) ->
   (secnode -> RecordExpression rec node s a x) ->
   NonEmpty.T [] secnode -> EquationSystem rec node s a
mkSplitFactorEquations s ef xf ns =
   (s =%= NonEmpty.foldl1 (~+) (fmap ef ns))
   <>
   foldMap (\n -> ef n =%= s ~* xf n) ns


getIntersectionStorages ::
  (Node.C node) =>
  TD.DirSequFlowGraph node
  -> [(StDir, ([Idx.SecNode node], Idx.SecNode node, [Idx.SecNode node]))]
getIntersectionStorages = concat . getStorages (format . toSecNode)
  where toSecNode (ins, n, outs) = (map fst ins, n, map fst outs)
        format x@(ins, sn@(Idx.SecNode sec _), outs) =
          case (filter h ins, filter h outs) of
               ([], [])  ->  -- We treat initial storages as in-storages
                 if sec == Idx.initSection then (InDir, x) else (NoDir, x)
               ([_], []) -> (InDir, x)
               ([], [_]) -> (OutDir, x)
               _ -> errorSecNode "getIntersectionStorages" sn
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


{- |
In the input 'EquationSystem' you can pass simple variable assignments
like

> edgeVar Idx.Eta sec0 node1 node2 =.= 0.42

but you may also insert complex relations like

> edgeVar Idx.Power sec0 node2 node1 =.= square (edgeVar Idx.Power sec0 node1 node2)

.
-}


solveSimple ::
  (Eq a, Product a, Record rec, Node.C node) =>
  (forall s. EquationSystem rec node s a) ->
  Env.Env node (rec (Result a))
solveSimple sys = runST $ do
  let EquationSystem eqsys = sys
  (varmap, System eqs) <- runWriterT $ execStateT eqsys mempty
  Sys.solve eqs
  traverse (traverse (fmap (maybe Undetermined Determined) . Sys.query)) varmap

solve ::
  (Eq a, Product a, Record rec, Node.C node) =>
  (forall s. EquationSystem rec node s a) ->
  TD.SequFlowGraph node -> Env.Env node (rec (Result a))
solve given g =
  solveSimple (given <> fromTopology (toDirSequFlowGraph g))


--------------------------------------------------------------------


fromTopology' ::
  (Eq a, Product a, Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
fromTopology' g = mconcat $
  makeInnerSectionEquations' g :
  makeInterSectionEquations g :
  []

makeInnerSectionEquations' ::
  (Eq a, Product a, Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
makeInnerSectionEquations' g = mconcat $
  makeEdgeEquations (M.keys $ Gr.edgeLabels g) :
  makeNodeEquations' g :
  makeStorageEquations (Gr.lefilter (TD.isStructureEdge . fst) g) :
  []


makeNodeEquations' ::
  (Eq a, Product a, Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
makeNodeEquations' = fold . M.mapWithKey f . Gr.nodes
   where f n (ins, _, outs) =
            let -- this variable is used again in makeStorageEquations
                varsumin = insum n
                varsumout = outsum n  -- and this one, too.
                splitEqs varsum nodes =
                   foldMap
                      (mkSplitFactorEquations varsum (energy n) (xfactor n))
                      (NonEmpty.fetch $ S.toList nodes)
            in  -- (varsumin =%= varsumout) -- EINZIGER UNTERSCHIED!!!
                -- <>
                splitEqs varsumin ins
                <>
                splitEqs varsumout outs



solveFromMeasurement ::
  (Eq a, Product a, Record rec, Node.C node) =>
  (forall s. EquationSystem rec node s a) ->
  TD.SequFlowGraph node -> Env.Env node (rec (Result a))
solveFromMeasurement given g =
  solveSimple (given <> fromTopology' (toDirSequFlowGraph g))



--------------------------------------------------------------------

-- Stellt die originalen Werte wieder her.
-- Die auf grund der Missachtung originaler Werte
-- falsch berechneten Werte bleiben aber erhalten.
-- Eine andere Lösung wäre, die Zeilen
--     (varsumin =%= varsumout)
--     <>
-- (im Moment 273 und 274) auszukommentieren.

conservativelySolve ::
  (Eq a, Product a, Record rec, Node.C node) =>
  (forall s. EquationSystem rec node s a) ->
  TD.SequFlowGraph node -> Env.Env node (rec (Result a))
conservativelySolve given g =
  solveSimple (given <> fromTopology (toDirSequFlowGraph g))
  <>
  solveSimple given
