{-# LANGUAGE Rank2Types #-}
module EFA.Equation.System (
  EquationSystem, ExprWithVars,
  fromTopology, solve, solveFromMeasurement, conservativelySolve,

  recAbs, constToExprSys,
  liftV, liftV2, liftF, liftF2,

  (=.=),
  getVar,
  getEdgeVar,
  power,
  energy,
  maxenergy,
  eta,
  xfactor,
  yfactor,
  insumvar,
  outsumvar,
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
import Control.Monad (liftM, liftM2)


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.HT as LH

import qualified Data.NonEmpty as NonEmpty

import Data.Traversable (traverse)
import Data.Foldable (foldMap, fold)
import Data.Monoid (Monoid, (<>), mempty, mappend, mconcat)
import Data.Tuple.HT (snd3)

import Data.Ord (comparing)


import Debug.Trace

type EqSysEnv node s a = Env.Env node Env.SingleRecord (Sys.Variable s a)

newtype
   ExprWithVars node s a =
      ExprWithVars (StateT (EqSysEnv node s a) (ST s) (Expr.T s a))

newtype
   EquationSystem node s a =
      EquationSystem (StateT (EqSysEnv node s a) (ST s) (Sys.M s ()))

instance Monoid (EquationSystem node s a) where
         mempty = EquationSystem $ return (return ())
         mappend (EquationSystem x) (EquationSystem y) =
           EquationSystem $ liftM2 (>>!) x y


liftV ::
  (Expr.T s a -> Expr.T s a) ->
  ExprWithVars node s a -> ExprWithVars node s a
liftV f (ExprWithVars xs) = ExprWithVars $ liftM f xs

liftF :: (a -> a) -> ExprWithVars node s a -> ExprWithVars node s a
liftF = liftV . Expr.fromRule2 . Sys.assignment2 ""


liftV2 ::
  (Expr.T s a -> Expr.T s a -> Expr.T s a) ->
  ExprWithVars node s a ->
  ExprWithVars node s a ->
  ExprWithVars node s a
liftV2 f (ExprWithVars xs) (ExprWithVars ys) =
  ExprWithVars $ liftM2 f xs ys

liftF2
  :: (a -> a -> a)
     -> ExprWithVars node s a -> ExprWithVars node s a
     -> ExprWithVars node s a
liftF2 = liftV2 . Expr.fromRule3 . Sys.assignment3 ""

instance (Fractional a) => Num (ExprWithVars node s a) where
         fromInteger = ExprWithVars . return . fromInteger

         (*) = liftV2 (*)
         (+) = liftV2 (+)
         (-) = liftV2 (-)

         abs = liftV abs
         signum = liftV signum


instance (Fractional a) => Fractional (ExprWithVars node s a) where
         fromRational = ExprWithVars . return . fromRational
         (/) = liftV2 (/)

instance (Floating a) => Floating (ExprWithVars node s a) where
         pi = constToExprSys pi
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



infix 0 =.=
(=.=) ::
  (Eq a) =>
  ExprWithVars node s a -> ExprWithVars node s a
  -> EquationSystem node s a
(ExprWithVars xs) =.= (ExprWithVars ys) =
  EquationSystem $ liftM2 (=:=) xs ys


constToExprSys :: a -> ExprWithVars node s a
constToExprSys = ExprWithVars . return . Expr.constant

withLocalVar ::
  (ExprWithVars node s a -> EquationSystem node s b)
  -> EquationSystem node s b
withLocalVar f = EquationSystem $ do
   v <- lift Sys.globalVariable
   case f $ ExprWithVars $ return $ Expr.fromVariable v of
        EquationSystem act -> act


recAbs :: Idx.Record
recAbs = Idx.Record Idx.Absolute

getVar ::
   (Env.AccessMap idx, Ord (idx node)) =>
   idx node -> ExprWithVars node s a
getVar idx =
  ExprWithVars $ fmap Expr.fromVariable $ do
    oldMap <- AccessState.get Env.accessMap
    case M.lookup idx oldMap of
      Just var -> return var
      Nothing -> do
        var <- lift Sys.globalVariable
        AccessState.set Env.accessMap $ M.insert idx var oldMap
        return var

getEdgeVar ::
   (Env.AccessMap idx, Ord (idx node)) =>
   (Idx.Record -> Idx.SecNode node -> Idx.SecNode node -> idx node) ->
   Idx.SecNode node -> Idx.SecNode node -> ExprWithVars node s a
getEdgeVar mkIdx x y = getVar (mkIdx recAbs x y)

power :: (Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars node s a
power = getEdgeVar Idx.Power

energy :: (Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars node s a
energy = getEdgeVar Idx.Energy

maxenergy :: (Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars node s a
maxenergy = getEdgeVar Idx.MaxEnergy

eta :: (Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars node s a
eta = getEdgeVar Idx.Eta

xfactor :: (Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars node s a
xfactor = getEdgeVar Idx.X

yfactor :: (Ord node) => Idx.SecNode node -> Idx.SecNode node -> ExprWithVars node s a
yfactor = getEdgeVar Idx.Y

insumvar :: (Ord node) => Idx.SecNode node -> ExprWithVars node s a
insumvar = getVar . Idx.Var recAbs Idx.InSum

outsumvar :: (Ord node) => Idx.SecNode node -> ExprWithVars node s a
outsumvar = getVar . Idx.Var recAbs Idx.OutSum

storage :: (Ord node) => Idx.SecNode node -> ExprWithVars node s a
storage = getVar . Idx.Storage recAbs

dtime :: Idx.Section -> ExprWithVars node s a
dtime = getVar . Idx.DTime recAbs


mwhen :: Monoid a => Bool -> a -> a
mwhen True t = t
mwhen False _ = mempty


fromTopology ::
  (Eq a, Fractional a, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem node s a
fromTopology g = mconcat $
  makeInnerSectionEquations g :
  makeInterSectionEquations g :
  []

-----------------------------------------------------------------

makeInnerSectionEquations ::
  (Eq a, Fractional a, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem node s a
makeInnerSectionEquations g = mconcat $
  makeEnergyEquations (map fst es) :
  makeEdgeEquations es :
  makeNodeEquations g :
  makeStorageEquations g' :
  []
  where g' = Gr.lefilter (TD.isOriginalEdge . fst) g
        es = Gr.labEdges g


makeEdgeEquations ::
  (Eq a, Fractional a, Ord node) =>
  [TD.LDirEdge node] -> EquationSystem node s a
makeEdgeEquations = foldMap mkEq
  where mkEq e@(Edge f t, ()) =
          case TD.getEdgeType e of
               TD.OriginalEdge -> power t f =.= eta f t * power f t
               TD.IntersectionEdge ->
                 (energy t f =.= eta f t * energy f t)
                 <> (eta f t =.= 1)


makeEnergyEquations ::
  (Eq a, Fractional a, Ord node) =>
  [Gr.Edge (Idx.SecNode node)] -> EquationSystem node s a
makeEnergyEquations = foldMap mkEq
  where mkEq (Edge f@(Idx.SecNode sf _) t@(Idx.SecNode st _)) =
          mwhen (sf == st) (equ f t <> equ t f)
             where equ x y = energy x y =.= dtime sf * power x y

makeNodeEquations ::
  (Eq a, Fractional a, Ord node) =>
  TD.DirSequFlowGraph node -> EquationSystem node s a
makeNodeEquations = fold . M.mapWithKey f . Gr.nodes
   where f n (ins, label, outs) = 
            let -- this variable is used again in makeStorageEquations
                varsumin = insumvar n
                varsumout = outsumvar n  -- and this, too.
                splitEqs varsum nodes =
                   foldMap
                      (mkSplitFactorEquations varsum (energy n) (xfactor n))
                      (NonEmpty.fetch $ S.toList nodes)
            in  mwhen (label /= TD.Storage) (varsumin =.= varsumout)
                -- (varsumin =.= varsumout) -- siehe bug 2013-02-12-sum-equations-storage
                <>
                splitEqs varsumin ins
                <>
                splitEqs varsumout outs


makeStorageEquations ::
  (Eq a, Fractional a, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem node s a
makeStorageEquations =
   mconcat . concatMap (LH.mapAdjacent f) . getInnersectionStorages
  where f (before, _) (now, dir) =
           storage now
           =.=
           storage before
           +
           case dir of
                NoDir  -> 0
                InDir  -> insumvar now
                OutDir -> - outsumvar now


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
  (Eq a, Fractional a, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem node s a
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
  (Eq a, Fractional a, Node.C node) =>
  ([Idx.SecNode node], Idx.SecNode node, [Idx.SecNode node]) -> EquationSystem node s a
mkInStorageEquations (_, n, outs) =
   flip foldMap
      (fmap (NonEmpty.sortBy (comparing getSection)) $
       NonEmpty.fetch outs) $ \souts ->
      withLocalVar $ \s ->
         -- The next equation is special for the initial Section.
         (maxenergy n (NonEmpty.head souts) =.=
          if getSection n == Idx.initSection
            then storage n
            else insumvar n)
         <>
         mkSplitFactorEquations s (maxenergy n) (yfactor n) souts
         <>
         let f beforeNext next =
                maxenergy n next =.=
                   maxenergy n beforeNext - energy beforeNext n
         in  mconcat $ LH.mapAdjacent f $ NonEmpty.flatten souts

mkOutStorageEquations ::
  (Eq a, Fractional a, Node.C node) =>
  ([Idx.SecNode node], Idx.SecNode node, [Idx.SecNode node]) -> EquationSystem node s a
mkOutStorageEquations (ins0, n, _) =
  flip foldMap (NonEmpty.fetch ins0) $ \ins ->
  withLocalVar $ \s ->
    mkSplitFactorEquations s (flip maxenergy n) (xfactor n) ins
    <>
    mkSplitFactorEquations (outsumvar n) (energy n) (xfactor n) ins

mkSplitFactorEquations ::
   (Eq a, Fractional a) =>
   ExprWithVars node s a ->
   (secnode -> ExprWithVars node s a) ->
   (secnode -> ExprWithVars node s a) ->
   NonEmpty.T [] secnode -> EquationSystem node s a
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
toDirSequFlowGraph g = Gr.mkGraph ns es
  where es = map (fmap (const ())) $
               filter TD.isDirEdge (M.toList $ Gr.edgeLabels g)
        ns = map f (M.toList $ Gr.nodes g)
        f (n, x) = (n, snd3 x)

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
  (Eq a, Fractional a, Node.C node) =>
  (forall s. EquationSystem node s a) ->
  TD.SequFlowGraph node -> Env.Env node Env.SingleRecord (Result a)
solve given g = runST $ do
  let dirG = toDirSequFlowGraph g
      EquationSystem eqsys = given <> fromTopology dirG
  (eqs, varmap) <-
    runStateT eqsys $ Env.empty $ Env.SingleRecord recAbs
  Sys.solve eqs
  traverse (fmap (maybe Undetermined Determined) . Sys.query) varmap


--------------------------------------------------------------------


fromTopology' ::
  (Eq a, Fractional a, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem node s a
fromTopology' g = mconcat $
  makeInnerSectionEquations' g :
  makeInterSectionEquations g :
  []

makeInnerSectionEquations' ::
  (Eq a, Fractional a, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem node s a
makeInnerSectionEquations' g = mconcat $
  makeEnergyEquations (map fst es) :
  makeEdgeEquations es :
  makeNodeEquations' g :
  makeStorageEquations g' :
  []
  where g' = Gr.lefilter (TD.isOriginalEdge . fst) g
        es = Gr.labEdges g


makeNodeEquations' ::
  (Eq a, Fractional a, Node.C node) =>
  TD.DirSequFlowGraph node -> EquationSystem node s a
makeNodeEquations' = fold . M.mapWithKey f . Gr.nodes
   where f n (ins, _, outs) =
            let -- this variable is used again in makeStorageEquations
                varsumin = insumvar n
                varsumout = outsumvar n  -- and this, too.
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
  (Eq a, Fractional a, Node.C node) =>
  (forall s. EquationSystem node s a) ->
  TD.SequFlowGraph node -> Env.Env node Env.SingleRecord (Result a)
solveFromMeasurement given g = runST $ do
  let dirG = toDirSequFlowGraph g
      EquationSystem eqsys = given <> fromTopology' dirG
  (eqs, varmap) <-
    runStateT eqsys $ Env.empty $ Env.SingleRecord recAbs
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
  (Eq a, Fractional a, Node.C node) =>
  (forall s. EquationSystem node s a) ->
  TD.SequFlowGraph node -> Env.Env node Env.SingleRecord (Result a)
conservativelySolve given g = runST $ do
  let dirG = toDirSequFlowGraph g
      EquationSystem eqsys = given <> fromTopology dirG
      EquationSystem givenSys = given

  (eqs, varmap) <-
    runStateT eqsys $ Env.empty $ Env.SingleRecord recAbs
  Sys.solve eqs

  (givenEqs, givenVarmap) <-
    runStateT givenSys $ Env.empty $ Env.SingleRecord recAbs
  Sys.solve givenEqs

  let uenv = Env.union givenVarmap varmap
  traverse (fmap (maybe Undetermined Determined) . Sys.query) uenv
