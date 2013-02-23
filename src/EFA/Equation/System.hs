{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Equation.System (
  EquationSystem, ExprWithVars,
  fromTopology, solve, solveFromMeasurement, conservativelySolve,

  constant,
  liftF, liftF2,
  sqrt,

  (=.=),
  getVar,
  getEdgeVar,
  power,
  energy,
  maxenergy,
  eta,
  xfactor,
  yfactor,
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

import EFA.Utility ((>>!))

import UniqueLogic.ST.Expression ((=:=))
import qualified UniqueLogic.ST.Expression as Expr
import qualified UniqueLogic.ST.System as Sys

import qualified Data.Accessor.Monad.Trans.State as AccessState

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT)

import Control.Monad.ST (ST, runST)
import Control.Monad (liftM2)

import Control.Applicative (Applicative, pure, liftA, liftA2)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.HT as LH

import qualified Data.NonEmpty as NonEmpty

import Data.Traversable (traverse)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mempty, mappend, mconcat)

import Data.Ord (comparing)

import qualified Prelude as P
import Prelude hiding (sqrt)


type
   BK rec node s a =
      StateT (Env.Env rec node (Sys.Variable s a)) (ST s)

type ExprWithVars rec node s a x = Bookkeeping rec node s a (Expr.T s x)

newtype
   Bookkeeping rec node s a x =
      Bookkeeping (BK rec node s a x)
   deriving (Functor, Applicative)

newtype
   EquationSystem rec node s a =
      EquationSystem (BK rec node s a (Sys.M s ()))

instance Monoid (EquationSystem rec node s a) where
         mempty = EquationSystem $ return (return ())
         mappend (EquationSystem x) (EquationSystem y) =
           EquationSystem $ liftM2 (>>!) x y


liftF ::
  (x -> y) ->
  ExprWithVars rec node s a x ->
  ExprWithVars rec node s a y
liftF = liftA . Expr.fromRule2 . Sys.assignment2 ""

liftF2 ::
  (x -> y -> z) ->
  ExprWithVars rec node s a x ->
  ExprWithVars rec node s a y ->
  ExprWithVars rec node s a z
liftF2 = liftA2 . Expr.fromRule3 . Sys.assignment3 ""

instance (Fractional x) => Num (Bookkeeping rec node s a x) where
         fromInteger = pure . fromInteger

         (*) = liftA2 (*)
         (+) = liftA2 (+)
         (-) = liftA2 (-)

         abs = liftA abs
         signum = liftA signum


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
   (Floating x) =>
   ExprWithVars rec node s a x ->
   ExprWithVars rec node s a x
sqrt = liftF P.sqrt


infix 0 =.=
(=.=) ::
  (Eq x) =>
  ExprWithVars rec node s a x -> ExprWithVars rec node s a x ->
  EquationSystem rec node s a
(Bookkeeping xs) =.= (Bookkeeping ys) =
  EquationSystem $ liftM2 (=:=) xs ys


constant :: x -> ExprWithVars rec node s a x
constant = pure . Expr.constant

withLocalVar ::
  (ExprWithVars rec node s a x -> EquationSystem rec node s a) ->
  EquationSystem rec node s a
withLocalVar f = EquationSystem $ do
   v <- lift Sys.globalVariable
   case f $ pure $ Expr.fromVariable v of
        EquationSystem act -> act


getVar ::
   (Env.AccessMap idx, Ord (idx rec node)) =>
   idx rec node -> ExprWithVars rec node s a a
getVar idx =
  Bookkeeping $ fmap Expr.fromVariable $ do
    oldMap <- AccessState.get Env.accessMap
    case M.lookup idx oldMap of
      Just var -> return var
      Nothing -> do
        var <- lift Sys.globalVariable
        AccessState.set Env.accessMap $ M.insert idx var oldMap
        return var

getEdgeVar ::
   (Idx.Record rec, Env.AccessMap idx, Ord (idx rec node)) =>
   (rec -> Idx.SecNode node -> Idx.SecNode node -> idx rec node) ->
   Idx.SecNode node -> Idx.SecNode node -> ExprWithVars rec node s a a
getEdgeVar mkIdx x y = getVar (mkIdx Idx.recDeflt x y)

power :: (Idx.Record rec, Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars rec node s a a
power = getEdgeVar Idx.Power

energy :: (Idx.Record rec, Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars rec node s a a
energy = getEdgeVar Idx.Energy

maxenergy :: (Idx.Record rec, Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars rec node s a a
maxenergy = getEdgeVar Idx.MaxEnergy

eta :: (Idx.Record rec, Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars rec node s a a
eta = getEdgeVar Idx.Eta

xfactor :: (Idx.Record rec, Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars rec node s a a
xfactor = getEdgeVar Idx.X

yfactor :: (Idx.Record rec, Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars rec node s a a
yfactor = getEdgeVar Idx.Y

insum :: (Idx.Record rec, Ord node) => Idx.SecNode node -> ExprWithVars rec node s a a
insum = getVar . Idx.Sum Idx.recDeflt Idx.In

outsum :: (Idx.Record rec, Ord node) => Idx.SecNode node -> ExprWithVars rec node s a a
outsum = getVar . Idx.Sum Idx.recDeflt Idx.Out

storage :: (Idx.Record rec, Ord node) => Idx.SecNode node -> ExprWithVars rec node s a a
storage = getVar . Idx.Storage Idx.recDeflt

dtime :: (Idx.Record rec) => Idx.Section -> ExprWithVars rec node s a a
dtime = getVar . Idx.DTime Idx.recDeflt


mwhen :: Monoid a => Bool -> a -> a
mwhen True t = t
mwhen False _ = mempty


fromTopology ::
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
fromTopology g = mconcat $
  makeInnerSectionEquations g :
  makeInterSectionEquations g :
  []

-----------------------------------------------------------------

makeInnerSectionEquations ::
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
makeInnerSectionEquations g = mconcat $
  makeEnergyEquations (map fst es) :
  makeEdgeEquations es :
  makeNodeEquations g :
  makeStorageEquations g' :
  []
  where g' = Gr.lefilter (TD.isOriginalEdge . fst) g
        es = Gr.labEdges g


makeEdgeEquations ::
  (Eq a, Fractional a, Idx.Record rec, Ord node) =>
  [TD.LDirEdge node] -> EquationSystem rec node s a
makeEdgeEquations = foldMap mkEq
  where mkEq e@(Edge f t, ()) =
          case TD.getEdgeType e of
               TD.OriginalEdge -> power t f =.= eta f t * power f t
               TD.IntersectionEdge ->
                 (energy t f =.= eta f t * energy f t)
                 <> (eta f t =.= 1)


makeEnergyEquations ::
  (Eq a, Fractional a, Idx.Record rec, Ord node) =>
  [Gr.Edge (Idx.SecNode node)] -> EquationSystem rec node s a
makeEnergyEquations = foldMap mkEq
  where mkEq (Edge f@(Idx.SecNode sf _) t@(Idx.SecNode st _)) =
          mwhen (sf == st) (equ f t <> equ t f)
             where equ x y = energy x y =.= dtime sf * power x y

makeNodeEquations ::
  (Eq a, Fractional a, Idx.Record rec, Ord node) =>
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
            in  -- mwhen (label /= TD.Storage) (varsumin =.= varsumout)
                (varsumin =.= varsumout) -- siehe bug 2013-02-12-sum-equations-storage
                <>
                splitEqs varsumin ins
                <>
                splitEqs varsumout outs


makeStorageEquations ::
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
makeStorageEquations =
   mconcat . concatMap (LH.mapAdjacent f) . getInnersectionStorages
  where f (before, _) (now, dir) =
           storage now
           =.=
           storage before
           +
           case dir of
                NoDir  -> 0
                InDir  -> insum now
                OutDir -> - outsum now


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
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
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
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
  ([Idx.SecNode node], Idx.SecNode node, [Idx.SecNode node]) ->
  EquationSystem rec node s a
mkInStorageEquations (_, n, outs) =
   flip foldMap
      (fmap (NonEmpty.sortBy (comparing getSection)) $
       NonEmpty.fetch outs) $ \souts ->
      withLocalVar $ \s ->
         -- The next equation is special for the initial Section.
         (maxenergy n (NonEmpty.head souts) =.=
          if getSection n == Idx.initSection
            then storage n
            else insum n)
         <>
         mkSplitFactorEquations s (maxenergy n) (yfactor n) souts
         <>
         let f beforeNext next =
                maxenergy n next =.=
                   maxenergy n beforeNext - energy beforeNext n
         in  mconcat $ LH.mapAdjacent f $ NonEmpty.flatten souts

mkOutStorageEquations ::
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
  ([Idx.SecNode node], Idx.SecNode node, [Idx.SecNode node]) ->
  EquationSystem rec node s a
mkOutStorageEquations (ins0, n, _) =
  flip foldMap (NonEmpty.fetch ins0) $ \ins ->
  withLocalVar $ \s ->
    mkSplitFactorEquations s (flip maxenergy n) (xfactor n) ins
    <>
    mkSplitFactorEquations (outsum n) (energy n) (xfactor n) ins

mkSplitFactorEquations ::
   (Eq x, Fractional x) =>
   ExprWithVars rec node s a x ->
   (secnode -> ExprWithVars rec node s a x) ->
   (secnode -> ExprWithVars rec node s a x) ->
   NonEmpty.T [] secnode -> EquationSystem rec node s a
mkSplitFactorEquations s ef xf ns =
   (s =.= NonEmpty.sum (fmap ef ns))
   <>
   foldMap (\n -> ef n =.= s * xf n) ns


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


solve ::
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
  (forall s. EquationSystem rec node s a) ->
  TD.SequFlowGraph node -> Env.Env rec node (Result a)
solve given g = runST $ do
  let dirG = toDirSequFlowGraph g
      EquationSystem eqsys = given <> fromTopology dirG
  (eqs, varmap) <- runStateT eqsys mempty
  Sys.solve eqs
  traverse (fmap (maybe Undetermined Determined) . Sys.query) varmap


--------------------------------------------------------------------


fromTopology' ::
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
fromTopology' g = mconcat $
  makeInnerSectionEquations' g :
  makeInterSectionEquations g :
  []

makeInnerSectionEquations' ::
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem rec node s a
makeInnerSectionEquations' g = mconcat $
  makeEnergyEquations (map fst es) :
  makeEdgeEquations es :
  makeNodeEquations' g :
  makeStorageEquations g' :
  []
  where g' = Gr.lefilter (TD.isOriginalEdge . fst) g
        es = Gr.labEdges g


makeNodeEquations' ::
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
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
            in  -- (varsumin =.= varsumout) -- EINZIGER UNTERSCHIED!!!
                -- <>
                splitEqs varsumin ins
                <>
                splitEqs varsumout outs



solveFromMeasurement ::
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
  (forall s. EquationSystem rec node s a) ->
  TD.SequFlowGraph node -> Env.Env rec node (Result a)
solveFromMeasurement given g = runST $ do
  let dirG = toDirSequFlowGraph g
      EquationSystem eqsys = given <> fromTopology' dirG
  (eqs, varmap) <- runStateT eqsys mempty
  Sys.solve eqs
  traverse (fmap (maybe Undetermined Determined) . Sys.query) varmap



--------------------------------------------------------------------

-- Stellt die originalen Werte wieder her.
-- Die auf grund der Missachtung originaler Werte
-- falsch berechneten Werte bleiben aber erhalten.
-- Eine andere Lösung wäre, die Zeilen
--     (varsumin =.= varsumout)
--     <>
-- (im Moment 273 und 274) auszukommentieren.

conservativelySolve ::
  (Eq a, Fractional a, Idx.Record rec, Node.C node) =>
  (forall s. EquationSystem rec node s a) ->
  TD.SequFlowGraph node -> Env.Env rec node (Result a)
conservativelySolve given g = runST $ do
  let dirG = toDirSequFlowGraph g
      EquationSystem eqsys = given <> fromTopology dirG
      EquationSystem givenSys = given

  (eqs, varmap) <- runStateT eqsys mempty
  Sys.solve eqs

  (givenEqs, givenVarmap) <- runStateT givenSys mempty
  Sys.solve givenEqs

  traverse (fmap (maybe Undetermined Determined) . Sys.query) $
     givenVarmap <> varmap
