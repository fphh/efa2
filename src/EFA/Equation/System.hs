{-# LANGUAGE Rank2Types #-}
module EFA.Equation.System where

import qualified EFA.Equation.Env as Env
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import EFA.Graph (Edge(..))

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

import Data.Maybe (maybeToList)
import Data.Ord (comparing)


-- import Debug.Trace

type EqSysEnv s a = Env.Env Env.SingleRecord (Sys.Variable s a)

newtype
   ExprWithVars s a =
      ExprWithVars (StateT (EqSysEnv s a) (ST s) (Expr.T s a))

newtype
   EquationSystem s a =
      EquationSystem (StateT (EqSysEnv s a) (ST s) (Sys.M s ()))

instance Monoid (EquationSystem s a) where
         mempty = EquationSystem $ return (return ())
         mappend (EquationSystem x) (EquationSystem y) =
           EquationSystem $ liftM2 (>>!) x y


liftV ::
  (Expr.T s a -> Expr.T s a) ->
  ExprWithVars s a -> ExprWithVars s a
liftV f (ExprWithVars xs) = ExprWithVars $ liftM f xs

liftV2 ::
  (Expr.T s a -> Expr.T s a -> Expr.T s a) ->
  ExprWithVars s a -> ExprWithVars s a -> ExprWithVars s a
liftV2 f (ExprWithVars xs) (ExprWithVars ys) = ExprWithVars $ liftM2 f xs ys


instance (Fractional a) => Num (ExprWithVars s a) where
         fromInteger = ExprWithVars . return . fromInteger

         (*) = liftV2 (*)
         (+) = liftV2 (+)
         (-) = liftV2 (-)

         abs = liftV abs
         signum = liftV signum


instance (Fractional a) => Fractional (ExprWithVars s a) where
         fromRational = ExprWithVars . return . fromRational
         (/) = liftV2 (/)

infix 0 =.=
(=.=) :: (Eq a) => ExprWithVars s a -> ExprWithVars s a -> EquationSystem s a
(ExprWithVars xs) =.= (ExprWithVars ys) = EquationSystem $ liftM2 (=:=) xs ys


constToExprSys :: a -> ExprWithVars s a
constToExprSys = ExprWithVars . return . Expr.constant

withLocalVar :: (ExprWithVars s a -> EquationSystem s b) -> EquationSystem s b
withLocalVar f = EquationSystem $ do
   v <- lift Sys.globalVariable
   case f $ ExprWithVars $ return $ Expr.fromVariable v of
        EquationSystem act -> act


recAbs :: Idx.Record
recAbs = Idx.Record Idx.Absolute

getVar ::
   (Env.AccessMap idx) =>
   idx -> ExprWithVars s a
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
   (Env.AccessMap idx) =>
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.SecNode -> Idx.SecNode -> ExprWithVars s a
getEdgeVar mkIdx x y = getVar (mkIdx recAbs x y)

power :: Idx.SecNode -> Idx.SecNode -> ExprWithVars s a
power = getEdgeVar Idx.Power

energy :: Idx.SecNode -> Idx.SecNode -> ExprWithVars s a
energy = getEdgeVar Idx.Energy

maxenergy :: Idx.SecNode -> Idx.SecNode -> ExprWithVars s a
maxenergy = getEdgeVar Idx.MaxEnergy

eta :: Idx.SecNode -> Idx.SecNode -> ExprWithVars s a
eta = getEdgeVar Idx.Eta

xfactor :: Idx.SecNode -> Idx.SecNode -> ExprWithVars s a
xfactor = getEdgeVar Idx.X

yfactor :: Idx.SecNode -> Idx.SecNode -> ExprWithVars s a
yfactor = getEdgeVar Idx.Y

insumvar :: Idx.SecNode -> ExprWithVars s a
insumvar = getVar . Idx.Var recAbs Idx.InSum

outsumvar :: Idx.SecNode -> ExprWithVars s a
outsumvar = getVar . Idx.Var recAbs Idx.OutSum

storage :: Idx.SecNode -> ExprWithVars s a
storage = getVar . Idx.Storage recAbs

dtime :: Idx.Section -> ExprWithVars s a
dtime = getVar . Idx.DTime recAbs


mwhen :: Monoid a => Bool -> a -> a
mwhen True t = t
mwhen False _ = mempty

edges :: Gr.Graph node nodeLabel edgeLabel -> [Gr.Edge node]
edges = M.keys . Gr.edgeLabels

fromTopology ::
  (Eq a, Fractional a) =>
  TD.SequFlowGraph -> EquationSystem s a
fromTopology g = mconcat $
  makeInnerSectionEquations g :
  makeInterSectionEquations g :
  []

-----------------------------------------------------------------

makeInnerSectionEquations ::
  (Eq a, Fractional a) =>
  TD.SequFlowGraph -> EquationSystem s a
makeInnerSectionEquations g = mconcat $
  makeEnergyEquations (map fst es) :
  makeEdgeEquations es :
  makeNodeEquations g :
  makeStorageEquations g' :
  []
  where g' = Gr.elfilter TD.isOriginalEdge g
        es = Gr.labEdges g


makeEdgeEquations ::
  (Eq a, Fractional a) =>
  [Gr.LEdge Idx.SecNode TD.ELabel] -> EquationSystem s a
makeEdgeEquations = foldMap mkEq
  where mkEq (Edge f t, lab) =
          case TD.edgeType lab of
               TD.OriginalEdge -> power t f =.= eta f t * power f t
               TD.IntersectionEdge ->
                 (energy t f =.= eta f t * energy f t)
                 <> (eta f t =.= 1)


makeEnergyEquations ::
  (Eq a, Fractional a) =>
  [Gr.Edge Idx.SecNode] -> EquationSystem s a
makeEnergyEquations = foldMap mkEq
  where mkEq (Edge f@(Idx.SecNode sf _) t@(Idx.SecNode st _)) =
          mwhen (sf == st) (equ f t <> equ t f)
             where equ x y = energy x y =.= dtime sf * power x y

makeNodeEquations ::
  (Eq a, Fractional a) =>
  TD.SequFlowGraph -> EquationSystem s a
makeNodeEquations = fold . M.mapWithKey f . Gr.nodes
   where f n (ins, _, outs) =
            let -- this variable is used again in makeStorageEquations
                varsumin = insumvar n
                varsumout = outsumvar n  -- and this, too.
                splitEqs varsum nodes =
                   foldMap
                      (mkSplitFactorEquations varsum (energy n) (xfactor n))
                      (NonEmpty.fetch $ S.toList nodes)
            in  (varsumin =.= varsumout)
                <>
                splitEqs varsumin ins
                <>
                splitEqs varsumout outs


makeStorageEquations ::
  (Eq a, Fractional a) =>
  TD.SequFlowGraph -> EquationSystem s a
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
getInnersectionStorages :: TD.SequFlowGraph -> [[(Idx.SecNode, StDir)]]
getInnersectionStorages = getStorages format
  where format ([n], s, []) = if TD.isDirEdge n then (s, InDir) else (s, NoDir)
        format ([], s, [n]) = if TD.isDirEdge n then (s, OutDir) else (s, NoDir)
        format ([], s, []) = (s, NoDir)
        format n@(_, _, _) = error ("getInnersectionStorages: " ++ show n)

type InOutFormat = InOut Idx.SecNode TD.ELabel
type InOut n el = ([Gr.LNode n el], n, [Gr.LNode n el])

getStorages ::
   (InOutFormat -> b) -> TD.SequFlowGraph -> [[b]]
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
  (Eq a, Fractional a) =>
  TD.SequFlowGraph -> EquationSystem s a
makeInterSectionEquations = foldMap f . getIntersectionStorages
  where f (dir, x) =
          case dir of
               NoDir -> mempty
               InDir -> mkInStorageEquations x
               OutDir -> mkOutStorageEquations x

getSection :: Idx.SecNode -> Idx.Section
getSection (Idx.SecNode s _) = s

getNode :: Idx.SecNode -> Idx.Node
getNode (Idx.SecNode _ n) = n

mkInStorageEquations ::
  (Eq a, Fractional a) =>
  ([Idx.SecNode], Idx.SecNode, [Idx.SecNode]) -> EquationSystem s a
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
  (Eq a, Fractional a) =>
  ([Idx.SecNode], Idx.SecNode, [Idx.SecNode]) -> EquationSystem s a
mkOutStorageEquations (ins0, n, _) =
  flip foldMap (NonEmpty.fetch ins0) $ \ins ->
  withLocalVar $ \s ->
    mkSplitFactorEquations s (flip maxenergy n) (xfactor n) ins
    <>
    mkSplitFactorEquations (outsumvar n) (energy n) (xfactor n) ins

mkSplitFactorEquations ::
   (Eq a, Fractional a) =>
   ExprWithVars s a ->
   (node -> ExprWithVars s a) ->
   (node -> ExprWithVars s a) ->
   NonEmpty.T [] node -> EquationSystem s a
mkSplitFactorEquations s ef xf ns =
   (s =.= NonEmpty.sum (fmap ef ns))
   <>
   foldMap (\n -> ef n =.= s * xf n) ns


getIntersectionStorages ::
  TD.SequFlowGraph -> [(StDir, ([Idx.SecNode], Idx.SecNode, [Idx.SecNode]))]
getIntersectionStorages = concat . getStorages (format . toSecNode)
  where toSecNode (ins, n, outs) = (map fst ins, n, map fst outs)
        format x@(ins, Idx.SecNode sec _, outs) =
          case (filter h ins, filter h outs) of
               ([], [])  ->  -- We treat initial storages as in-storages
                 if sec == Idx.initSection then (InDir, x) else (NoDir, x)
               ([_], []) -> (InDir, x)
               ([], [_]) -> (OutDir, x)
               _ -> error ("getIntersectionStorages: " ++ show x)
          where h s = getSection s == sec


-----------------------------------------------------------------


{- |
In the input 'EquationSystem' you can pass simple variable assignments
like

> edgeVar Idx.Eta sec0 node1 node2 =.= 0.42

but you may also insert complex relations like

> edgeVar Idx.Power sec0 node2 node1 =.= square (edgeVar Idx.Power sec0 node1 node2)

.
-}

-- -> solve
solveSystemDoIt ::
  (Eq a, Fractional a) =>
  (forall s. EquationSystem s a) ->
  TD.SequFlowGraph -> Env.Env Env.SingleRecord (Maybe a)
solveSystemDoIt given g = runST $ do
  let EquationSystem eqsys = given <> fromTopology g
  (eqs, varmap) <-
     runStateT eqsys $ Env.empty $ Env.SingleRecord $ Idx.Record Idx.Absolute
  Sys.solve eqs
  traverse Sys.query varmap

-- weg:
solveSystem ::
  (Eq a, Fractional a) =>
  (forall s. EquationSystem s a) ->
  TD.SequFlowGraph -> Env.Env Env.SingleRecord [a]
solveSystem given = fmap maybeToList . solveSystemDoIt given
