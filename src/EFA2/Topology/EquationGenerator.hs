-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RankNTypes #-}

module EFA2.Topology.EquationGenerator where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import EFA2.Signal.Index as Idx
import EFA2.Topology.EfaGraph as Gr

import EFA2.Topology.TopologyData
       (SequFlowGraph, isOriginalEdge, getActiveStores, isStorage,
                       getFlowDirection, FlowDirection(..),
                       isDirEdge, isStorageNode)
import EFA2.Solver.Equation
import UniqueLogic.ST.Expression as Expr
import UniqueLogic.ST.System as Sys
--import UniqueLogic.ST.Rule as R

import Control.Monad.ST
import Control.Monad

-- Unterschied strict vs lazy?
--import Control.Monad.Writer

-- import Control.Monad.State

import Control.Monad.Trans.Class
import Control.Monad.Trans.State

--import Control.Applicative

import Data.Monoid
import Data.Maybe
import Data.Maybe.HT (toMaybe)

--import Data.Ord (comparing)
import Data.Traversable (traverse)
import Data.Foldable (foldMap, fold)
import Data.Tuple.HT (snd3)

import EFA2.Interpreter.Env as Env

import Debug.Trace



type ProvEnv s a = M.Map Env.Index (Variable s a)

newtype ExprWithVars s a = ExprWithVars (StateT (ProvEnv s a) (ST s) (T s a))
type SysWithVars s a = StateT (ProvEnv s a) (ST s) (M s ())


-- Gleichungen mitloggen
newtype EquationSystem s a = EquationSystem (SysWithVars s a)

instance Monoid (EquationSystem s a) where
         mempty = EquationSystem $ return (return ())
         mappend (EquationSystem x) (EquationSystem y) =
           EquationSystem $ liftM2 (>>) x y

liftV2 :: 
  (T s a -> T s a -> T s a) -> 
  ExprWithVars s a -> ExprWithVars s a -> ExprWithVars s a
liftV2 f (ExprWithVars xs) (ExprWithVars ys) = ExprWithVars $ liftM2 f xs ys


instance (Num a, Fractional a) => Num (ExprWithVars s a) where
         (*) = liftV2 (*)
         (+) = liftV2 (+)
         (-) = liftV2 (-)

         fromInteger = ExprWithVars . return . fromInteger
         abs (ExprWithVars xs) = ExprWithVars $ liftM abs xs
         signum (ExprWithVars xs) = ExprWithVars $ liftM signum xs

infix 0 .=
(.=) :: (Eq a) => ExprWithVars s a -> ExprWithVars s a -> EquationSystem s a
(ExprWithVars xs) .= (ExprWithVars ys) = EquationSystem $ liftM2 (=:=) xs ys


constToExprSys :: a -> ExprWithVars s a
constToExprSys = ExprWithVars . return . Expr.constant

varToExprSys :: Variable s a -> ExprWithVars s a
varToExprSys = ExprWithVars . return . Expr.fromVariable


withLocalVar :: (ExprWithVars s a -> t) -> t
withLocalVar f = f $ ExprWithVars $
  liftM fromVariable (lift globalVariable)

recAbs :: Record
recAbs = Record Absolute

makeVar ::
  (MkIdxC a) =>
  (Record -> Idx.SecNode -> Idx.SecNode -> a) ->
  Idx.SecNode -> Idx.SecNode -> Env.Index
makeVar idxf nid nid' =
  mkVar $ idxf recAbs nid nid'

getVar :: Env.Index -> ExprWithVars s a
getVar idx =
  let oldVar = return . fromVariable
      newVar = 
        lift globalVariable
          >>= \var -> modify (M.insert idx var)
          >> return (fromVariable var)
  in ExprWithVars $ gets (M.lookup idx) >>= maybe newVar oldVar




power :: SecNode -> SecNode -> ExprWithVars s a
power = (getVar .) . makeVar Idx.Power

energy :: SecNode -> SecNode -> ExprWithVars s a
energy = (getVar .) . makeVar Idx.Energy

eta :: SecNode -> SecNode -> ExprWithVars s a
eta = (getVar .) . makeVar Idx.FEta

xfactor :: SecNode -> SecNode -> ExprWithVars s a
xfactor = (getVar .) . makeVar Idx.X

--vvar :: Use -> SecNode -> ExprWithVars s a
--vvar use = getVar . mkVar . Idx.Var recAbs use

insumvar :: SecNode -> ExprWithVars s a
insumvar = getVar . mkVar . Idx.InSumVar recAbs

outsumvar :: SecNode -> ExprWithVars s a
outsumvar = getVar . mkVar . Idx.OutSumVar recAbs

storage :: SecNode -> ExprWithVars s a
storage = getVar . mkVar . Idx.Storage recAbs


dtime :: Section -> ExprWithVars s a
dtime = getVar . mkVar . Idx.DTime recAbs

mwhen :: Monoid a => Bool -> a -> a
mwhen True t = t
mwhen False _ = mempty 

edges :: Gr.EfaGraph node nodeLabel edgeLabel -> [Gr.Edge node]
edges g = M.keys el
  where el = Gr.edgeLabels g

makeAllEquations ::
  (Eq a, Fractional a) =>
  SequFlowGraph -> EquationSystem s a
makeAllEquations g = mconcat $
  makeInnerSectionEquations g :
  []

makeInnerSectionEquations ::
  (Eq a, Fractional a) =>
  SequFlowGraph -> EquationSystem s a
makeInnerSectionEquations g = mconcat $
  makeEnergyEquations es :
  makeEdgeEquations es :
  makeNodeEquations g' :
  makeStorageEquations g' :
  []
  where g' = elfilter isOriginalEdge g
        es = edges g'

makeEdgeEquations ::
  (Eq a, Fractional a) =>
  [Edge SecNode] -> EquationSystem s a
makeEdgeEquations es = foldMap mkEq es
  where mkEq (Edge f t) = power t f .= eta f t * power f t


makeEnergyEquations ::
  (Eq a, Fractional a) =>
  [Edge SecNode] -> EquationSystem s a
makeEnergyEquations es = foldMap mkEq es
  where mkEq (Edge f@(SecNode sf _) t@(SecNode st _)) =
          mwhen (sf == st)
            (energy f t .= dt * power f t) <> (energy t f .= dt * power t f)
          where dt = dtime sf


{-
Die annonyme Variable ist unguenstig, weil man sie spaeter wieder braucht.
Siehe eins weiter unten.

makeNodeEquations ::
  (Eq a, Fractional a) =>
  SequFlowGraph -> EquationSystem s a
makeNodeEquations = fold . M.mapWithKey ((f .) . g) . Gr.nodes
  where g n (ins, _, outs) = (S.toList ins, n, S.toList outs)
        f a@(ins, n, outs) = withLocalVar $ \s ->
             (1 .= sum xin)
          <> (1 .= sum xout)

          -- <> (s .= sum ein)
          <> mwhen (not (null outs)) (s .= sum ein)

          -- <> (s .= sum eout)
          <> mwhen (not (null ins)) (s .= sum eout)

          <> (mconcat $ zipWith (\en x -> en .= x * s) ein xin)
          <> (mconcat $ zipWith (\en x -> en .= x * s) eout xout)
          where xin = map (xfactor n) ins
                xout = map (xfactor n) outs
                ein = map (energy n) ins
                eout = map (energy n) outs
-}

makeNodeEquations ::
  (Eq a, Fractional a) =>
  SequFlowGraph -> EquationSystem s a
makeNodeEquations = mconcat . mapGraph (f . g)
  where g (ins, n, outs) = (map fst ins, fst n, map fst outs)
        f a@(ins, n, outs) =
          (1 .= sum xin)
          <> (1 .= sum xout)
          <> (varsumin .= sum ein)
          <> (varsumout .= sum eout)
          <> mwhen (not (null ins) && not (null outs)) (varsumin .= varsumout)
          <> (mconcat $ zipWith (h varsumin) ein xin)
          <> (mconcat $ zipWith (h varsumout) eout xout)
          where xin = map (xfactor n) ins
                xout = map (xfactor n) outs
                ein = map (energy n) ins
                eout = map (energy n) outs
                varsumin = insumvar n       -- this variable is used again in makeStorageEquations
                varsumout = outsumvar n     -- and this, too.
                h s en x = en .= x * s




makeStorageEquations ::
  (Eq a, Fractional a) =>
  SequFlowGraph -> EquationSystem s a
makeStorageEquations topo = mconcat $ foldMap g st
  where st = getStorages topo
        g lst = zipWith f lst (tail lst)
        f (before, _) (now, dir) =
          case dir of
               NoDir  -> stNow .= stBefore
               InDir  -> stNow .= stBefore + varsumin
               OutDir -> stNow .= stBefore - varsumout
          where stBefore = storage before
                stNow = storage now
                varsumin = insumvar now
                varsumout = outsumvar now


data StDir = InDir
           | OutDir
           | NoDir deriving (Eq, Ord, Show)

getStorages :: SequFlowGraph -> [[(SecNode, StDir)]]
getStorages topo = inoutst
  where inoutst = L.groupBy nodeId $ map f $ filter isStorageNode $ mkInOutGraphFormat topo
        f ([n], (s, _), []) = if isDirEdge n then (s, InDir) else (s, NoDir)
        f ([], (s, _), [n]) = if isDirEdge n then (s, OutDir) else (s, NoDir)
        f ([], (s, _), []) = (s, NoDir)
        f n@(_, _, _) = error (show n ++ ": getStorages")
        nodeId (SecNode _ s, _) (SecNode _ t, _) = s == t


mapToEnvs :: (a -> b) -> M.Map Env.Index a -> Envs SingleRecord b
mapToEnvs func = M.foldWithKey f envs
  where envs = emptyEnv { recordNumber = SingleRecord (Record Absolute) }
        f (Env.Energy idx) v e =
          e { energyMap = M.insert idx (func v) (energyMap e) }
        f (Env.Power idx) v e =
          e { powerMap = M.insert idx (func v) (powerMap e) }
        f (Env.FEta idx) v e =
          e { fetaMap = M.insert idx (const $ func v) (fetaMap e) }
        f (Env.X idx) v e =
          e { xMap = M.insert idx (func v) (xMap e) }
        f (Env.Store idx) v e =
          e { storageMap = M.insert idx (func v) (storageMap e) }
        f (Env.DTime idx) v e =
          e { dtimeMap = M.insert idx (func v) (dtimeMap e) }
        f _ _ e = e

solveSystemDoIt ::
  (Eq a, Fractional a) =>
  [(Env.Index, a)] -> SequFlowGraph -> M.Map Env.Index (Maybe a)
solveSystemDoIt given g = runST $ do
  let f (var, val) = getVar var .= constToExprSys val
      -- Reihenfolge spielt hier eine Rolle,
      -- auch wenn given neue Variablen enthält. Warum? Ist jetzt klar!
      EquationSystem eqsys = foldMap f given <> makeAllEquations g
      --EquationSystem eqsys = makeAllEquations g <> foldMap f given
  (eqs, varmap) <- runStateT eqsys M.empty
  solve eqs
  traverse query varmap

solveSystem ::
  (Eq a, Fractional a) =>
  [(Env.Index, a)] -> SequFlowGraph -> Envs SingleRecord [a]
solveSystem given = mapToEnvs maybeToList . solveSystemDoIt given


-- Equation ueberfluessigne Typparameter entfernen


-----------------------------------------------------------------------

{-
example1 :: ST s ((Variable s Double, Variable s Double, Variable s Double), M s ())
example1 = do
  vars@(xv, yv, zv) <- liftM3 (,,) globalVariable globalVariable globalVariable
  let given = do
        let z = fromVariable zv
        z =:= 2
      eqs = do
        let x = fromVariable xv
            y = fromVariable yv
            z = fromVariable zv
        x*3 =:= y/2
        5 =:= 2+x+z
  return $ (vars, do { given; eqs })


solveItExample1 :: (Maybe Double, Maybe Double, Maybe Double)
solveItExample1 = runST $ do
  ((xv, yv, zv), eqs) <- example1
  solve eqs
  liftM3 (,,) (query xv) (query yv) (query zv)


example2 :: ST s ((Variable s Double, Variable s Double, Variable s Double), M s ())
example2 = do
  vars@(xv, yv, zv) <- liftM3 (,,) globalVariable globalVariable globalVariable
  let eqs = do
        let x = fromVariable xv
            y = fromVariable yv
            z = fromVariable zv
        x*3 =:= y/2
        5 =:= 2+x+z
  return $ (vars, eqs)

(.==) :: (Eq a) => Variable s a -> a -> M s ()
(.==) zv d = fromVariable zv =:= Expr.constant d


solveIt :: [M s a] -> M s b -> ST s c -> ST s c
solveIt given eqs getVal = solve (sequence given >> eqs) >> getVal


solveItExample2 :: Double -> (Maybe Double, Maybe Double, Maybe Double)
solveItExample2 d = runST $ do
  ((xv, yv, zv), eqs) <- example2
  let getValues = liftM3 (,,) (query xv) (query yv) (query zv)
  solveIt [zv .== d] eqs getValues


solveIt2 :: GivenEquations s a -> EquationSystem s b -> ST s c -> ST s c
solveIt2 (GivenEquations given) (EquationSystem sys) getVal = do
  g <- given
  (_, s) <- sys
  solve (g >> s) >> getVal
-}