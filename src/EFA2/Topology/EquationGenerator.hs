-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RankNTypes #-}

module EFA2.Topology.EquationGenerator where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

import EFA2.Signal.Index as Idx
import EFA2.Topology.EfaGraph as Gr

import EFA2.Topology.TopologyData (SequFlowGraph, isOriginalEdge)
import EFA2.Solver.Equation
import UniqueLogic.ST.Expression as Expr
import UniqueLogic.ST.System as Sys
import UniqueLogic.ST.Rule as R

import Control.Monad.ST
import Control.Monad

-- Unterschied strict vs lazy?
--import Control.Monad.Writer
import Control.Monad.State

import Control.Applicative

import Data.Monoid
import Data.Maybe
import Data.Ord (comparing)
import Data.Traversable (traverse)
import Data.Foldable (foldMap)

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

vvar use = getVar . mkVar . Idx.Var recAbs use

insum = getVar . mkVar . Idx.InSumVar recAbs
outsum = getVar . mkVar . Idx.OutSumVar recAbs


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
  makeOriginalEdgeEquations g :
  []

makeOriginalEdgeEquations ::
  (Eq a, Fractional a) =>
  SequFlowGraph -> EquationSystem s a
makeOriginalEdgeEquations g = mconcat $
  makeEnergyEquations es :
  makeEdgeEquations es :
  makeNodeEquations g' :
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
  where mkEq (Edge f@(SecNode sf nf) t@(SecNode st nt)) =
          mwhen (sf == st)
            (energy f t .= dt * power f t) <> (energy t f .= dt * power t f)
          where dt = dtime sf


makeNodeEquations ::
  (Eq a, Fractional a) =>
  SequFlowGraph -> EquationSystem s a
makeNodeEquations = mconcat . mapGraph (f . g)
  where g (ins, n, outs) = (map fst ins, fst n, map fst outs)
        f (ins, n, outs) =
          (1 .= sum xin)
          <> (1 .= sum xout)
          <> (sumin .= sumout)
          <> (mconcat $ zipWith (f sumin) ein xin)
          <> (mconcat $ zipWith (f sumout) eout xout)
          where xin = map (xfactor n) ins
                xout = map (xfactor n) outs
                ein = map (energy n) ins
                eout = map (energy n) outs
                sumin = sum ein
                sumout = sum eout
                f s en x = en .= x * s

mapToEnvs :: M.Map Env.Index (Maybe a) -> Envs SingleRecord (Maybe a)
mapToEnvs = M.foldWithKey f envs
  where envs = emptyEnv { recordNumber = SingleRecord (Record Absolute) }
        f (Env.Energy idx) v e =
          e { energyMap = M.insert idx v (energyMap e) }
        f (Env.Power idx) v e =
          e { powerMap = M.insert idx v (powerMap e) }
        f (Env.FEta idx) v e =
          e { fetaMap = M.insert idx (const v) (fetaMap e) }
        f (Env.X idx) v e =
          e { xMap = M.insert idx v (xMap e) }
        f (Env.DTime idx) v e =
          e { dtimeMap = M.insert idx v (dtimeMap e) }
        f _ _ e = e

solveSystemDoIt ::
  (Eq a, Fractional a) =>
  [(Env.Index, a)] -> SequFlowGraph -> M.Map Env.Index (Maybe a)
solveSystemDoIt given g = runST $ do
  let f (var, val) = getVar var .= constToExprSys val
      -- Reihenfolge spielt hier eine Rolle,
      -- auch wenn given neue Variablen enthält. Warum?
      EquationSystem eqsys = foldMap f given <> makeAllEquations g
  (eqs, varmap) <- runStateT eqsys M.empty
  solve eqs
  traverse query varmap

solveSystem ::
  (Eq a, Fractional a) =>
  [(Env.Index, a)] -> SequFlowGraph -> Envs SingleRecord (Maybe a)
solveSystem given = mapToEnvs . solveSystemDoIt given

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