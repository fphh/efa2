{-# LANGUAGE Rank2Types #-}
module EFA2.Topology.EquationGenerator where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.HT as LH
import qualified Data.NonEmpty as NonEmpty

import EFA2.Signal.Index (SecNode(..), Section(..))
import qualified EFA2.Signal.Index as Idx

import EFA2.Topology.EfaGraph (Edge(..))
import qualified EFA2.Topology.EfaGraph as Gr

import qualified EFA2.Topology.TopologyData as TD
import EFA2.Utils.Utils ((>>!))

import UniqueLogic.ST.Expression ((=:=))
import qualified UniqueLogic.ST.Expression as Expr
import qualified UniqueLogic.ST.System as Sys

import Control.Monad.ST (ST, runST)
import Control.Monad (liftM, liftM2)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, gets, modify)


import qualified Data.Accessor.Basic as Accessor

import Data.Monoid (Monoid, (<>), mempty, mappend, mconcat)

import Data.Maybe (maybeToList)
import Data.Ord (comparing)

import Data.Traversable (traverse)
import Data.Foldable (foldMap, fold)

import qualified EFA2.Interpreter.Env as Env
import Data.Tuple.HT (snd3)

import Debug.Trace


type ProvEnv s a = Env.Envs Env.SingleRecord (Sys.Variable s a)

newtype
   ExprWithVars s a =
      ExprWithVars (StateT (ProvEnv s a) (ST s) (Expr.T s a))

newtype
   EquationSystem s a =
      EquationSystem (StateT (ProvEnv s a) (ST s) (Sys.M s ()))

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
  let newVar =
         lift Sys.globalVariable
          >>= \var -> modify (Accessor.modify Env.accessMap $ M.insert idx var)
          >>! return var
  in ExprWithVars $ fmap Expr.fromVariable $
        maybe newVar return =<< gets (M.lookup idx . Accessor.get Env.accessMap)

getEdgeVar ::
   (Env.AccessMap idx) =>
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.SecNode -> Idx.SecNode -> ExprWithVars s a
getEdgeVar mkIdx x y = getVar (mkIdx recAbs x y)

power :: SecNode -> SecNode -> ExprWithVars s a
power = getEdgeVar Idx.Power

energy :: SecNode -> SecNode -> ExprWithVars s a
energy = getEdgeVar Idx.Energy

maxenergy :: SecNode -> SecNode -> ExprWithVars s a
maxenergy = getEdgeVar Idx.MaxEnergy

eta :: SecNode -> SecNode -> ExprWithVars s a
eta = getEdgeVar Idx.FEta

xfactor :: SecNode -> SecNode -> ExprWithVars s a
xfactor = getEdgeVar Idx.X

yfactor :: SecNode -> SecNode -> ExprWithVars s a
yfactor = getEdgeVar Idx.Y

insumvar :: SecNode -> ExprWithVars s a
insumvar = getVar . Idx.Var recAbs Idx.InSum

outsumvar :: SecNode -> ExprWithVars s a
outsumvar = getVar . Idx.Var recAbs Idx.OutSum

storage :: SecNode -> ExprWithVars s a
storage = getVar . Idx.Storage recAbs

dtime :: Section -> ExprWithVars s a
dtime = getVar . Idx.DTime recAbs


mwhen :: Monoid a => Bool -> a -> a
mwhen True t = t
mwhen False _ = mempty 

edges :: Gr.EfaGraph node nodeLabel edgeLabel -> [Gr.Edge node]
edges = M.keys . Gr.edgeLabels

makeAllEquations ::
  (Eq a, Fractional a) =>
  TD.SequFlowGraph -> EquationSystem s a
makeAllEquations g = mconcat $
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
  [Gr.LEdge SecNode TD.ELabel] -> EquationSystem s a
makeEdgeEquations es = foldMap mkEq es
  where mkEq (Edge f t, lab) =
          case TD.edgeType lab of
               TD.OriginalEdge -> power t f =.= eta f t * power f t
               TD.IntersectionEdge ->
                 (energy t f =.= eta f t * energy f t)
                 <> (eta f t =.= 1)


makeEnergyEquations ::
  (Eq a, Fractional a) =>
  [Gr.Edge SecNode] -> EquationSystem s a
makeEnergyEquations = foldMap mkEq
  where mkEq (Edge f@(SecNode sf _) t@(SecNode st _)) =
          mwhen (sf == st) (equ f t <> equ t f)
             where equ x y = energy x y =.= dtime sf * power x y

makeNodeEquations ::
  (Eq a, Fractional a) =>
  TD.SequFlowGraph -> EquationSystem s a
makeNodeEquations = fold . M.mapWithKey ((f .) . g) . Gr.nodes
  where  g n (ins, _, outs) = (S.toList ins, n, S.toList outs)
         f (ins, n, outs) =
           --(1 =.= sum xin)
           -- <> (1 =.= sum xout)
           (varsumin =.= sum ein)
           <> (varsumout =.= sum eout)
           <> mwhen (not (null ins) && not (null outs)) (varsumin =.= varsumout)
           <> (mconcat $ zipWith (h varsumin) ein xin)
           <> (mconcat $ zipWith (h varsumout) eout xout)
          where xin = map (xfactor n) ins
                xout = map (xfactor n) outs
                ein = map (energy n) ins
                eout = map (energy n) outs
                varsumin = insumvar n       -- this variable is used again in makeStorageEquations
                varsumout = outsumvar n     -- and this, too.
                h s en x = en =.= x * s


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
getInnersectionStorages :: TD.SequFlowGraph -> [[(SecNode, StDir)]] -- Map SecNode StDir
getInnersectionStorages = getStorages format
  where format ([n], (s, _), []) = if TD.isDirEdge n then (s, InDir) else (s, NoDir)
        format ([], (s, _), [n]) = if TD.isDirEdge n then (s, OutDir) else (s, NoDir)
        format ([], (s, _), []) = (s, NoDir)
        format n@(_, _, _) = error ("getInnersectionStorages: " ++ show n)

type InOutFormat = Gr.InOut SecNode TD.NodeType TD.ELabel

getStorages ::
   (InOutFormat -> b) -> TD.SequFlowGraph -> [[b]]
getStorages format =
  M.elems
  . fmap M.elems
  . M.fromListWith (M.unionWith (error "duplicate node"))
  . map (\io -> case fst $ snd3 io of
                   Idx.Section sec node -> (node, M.singleton sec (format io)))
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
  ([SecNode], SecNode, [SecNode]) -> EquationSystem s a
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
  ([SecNode], SecNode, [SecNode]) -> EquationSystem s a
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
  TD.SequFlowGraph -> [(StDir, ([SecNode], SecNode, [SecNode]))]
getIntersectionStorages = concat . getStorages (format . toSecNode)
  where toSecNode (ins, n, outs) = (map fst ins, fst n, map fst outs)
        format x@(ins, SecNode sec _, outs) =
          case (filter h ins, filter h outs) of
               ([], [])  ->  -- We treat initial storages as in-storages
                 if sec == Idx.initSection then (InDir, x) else (NoDir, x)
               ([_], []) -> (InDir, x)
               ([], [_]) -> (OutDir, x)
               _ -> error ("getIntersectionStorages: " ++ show x)
          where h s = getSection s == sec


{-

makeInterSectionEquations ::
  (Eq a, Fractional a) =>
  TD.SequFlowGraph -> EquationSystem s a
makeInterSectionEquations g = mconcat $
  makeInterNodeEquations g :
  []

makeInterNodeEquations ::
  (Eq a, Fractional a) =>
  TD.SequFlowGraph -> EquationSystem s a
makeInterNodeEquations topo = foldMap f st
  where st = getIntersectionStorages topo
        f (dir, x) =
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
  ([SecNode], SecNode, [SecNode]) -> EquationSystem s a
mkInStorageEquations (_, _, []) = mempty
mkInStorageEquations (_, n, outs) =
  withLocalVar $ \s ->
    -- The next equation is special for the initial Section.
    (energy n so =.= if initialSec n then initStorage else varsumin)
    <> (s =.= sum es)
    <> (mconcat $ zipWith (\x e -> e =.= x * s) xs es)
    <> (mconcat $ zipWith f sos souts)
  where souts@(so:sos) = L.sortBy (comparing getSection) outs
        initStorage = storage n
        varsumin = insumvar n
        initialSec s = getSection s == Idx.initSection
        xs = map (xfactor n) souts
        es = map (energy n) souts
        f next beforeNext = energy n next =.= energy n beforeNext - energy beforeNext n

mkOutStorageEquations ::
  (Eq a, Fractional a) =>
  ([SecNode], SecNode, [SecNode]) -> EquationSystem s a
mkOutStorageEquations ([], _, _) = mempty
mkOutStorageEquations (ins, n, _) =
  withLocalVar $ \s ->
    (s =.= sum esOpposite)
    <> (varsumout =.= sum esHere)
    <> (mconcat $ zipWith (\e x -> e =.= x * s) esOpposite xsHere)
    <> (mconcat $ zipWith (\e x -> e =.= x * varsumout) esHere xsHere)
  where sins = L.sortBy (comparing getSection) ins
        esOpposite = map (flip energy n) sins
        esHere = map (energy n) sins
        xsHere = map (xfactor n) sins
        varsumout = outsumvar n


getIntersectionStorages ::
  TD.SequFlowGraph -> [(StDir, ([SecNode], SecNode, [SecNode]))]
getIntersectionStorages = concat . getStorages (format . toSecNode)
  where toSecNode (ins, n, outs) = (map fst ins, fst n, map fst outs)
        format x@(ins, SecNode sec _, outs) =
          case (filter h ins, filter h outs) of
               ([], [])  ->  -- We treat initial storages as in-storages
                 if sec == Idx.initSection then (InDir, x) else (NoDir, x)
               ([_], []) -> (InDir, x)
               ([], [_]) -> (OutDir, x)
               _ -> error ("getIntersectionStorages: " ++ show x)
          where h s = getSection s == sec

-}

-----------------------------------------------------------------


{- |
In the input 'EquationSystem' you can pass simple variable assignments
like

> edgeVar Idx.Eta sec0 node1 node2 =.= 0.42

but you may also insert complex relations like

> edgeVar Idx.Power sec0 node2 node1 =.= square (edgeVar Idx.Power sec0 node1 node2)

.
-}
solveSystemDoIt ::
  (Eq a, Fractional a) =>
  (forall s. EquationSystem s a) ->
  TD.SequFlowGraph -> Env.Envs Env.SingleRecord (Maybe a)
solveSystemDoIt given g = runST $ do
  let EquationSystem eqsys = given <> makeAllEquations g
  (eqs, varmap) <-
     runStateT eqsys $ Env.empty $ Env.SingleRecord $ Idx.Record Idx.Absolute
  Sys.solve eqs
  traverse Sys.query varmap

solveSystem ::
  (Eq a, Fractional a) =>
  (forall s. EquationSystem s a) ->
  TD.SequFlowGraph -> Env.Envs Env.SingleRecord [a]
solveSystem given = fmap maybeToList . solveSystemDoIt given
