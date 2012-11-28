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

import EFA2.Interpreter.Env as Env

import Debug.Trace



-- type ProvEnv s a = [(Env.Index, Variable s a)]
type ProvEnv s a = M.Map Env.Index (Variable s a)

-- map bevorzugen

-- module umbenennen in gleichungsgenerator

-- zu Newtype machen und Num instanz und provenv als writer ++ symbolisch darstellung von gleichungen
--newtype ExprWithVars s a = ExprWithVars (ST s (ProvEnv s a, T s a))

newtype ExprWithVars s a = ExprWithVars (StateT (ProvEnv s a) (ST s) (T s a))
type SysWithVars s a = StateT (ProvEnv s a) (ST s) (M s ())


-- Gleichungen mitloggen
newtype EquationSystem s a = EquationSystem (SysWithVars s a)

newtype GivenEquations s a = GivenEquations (ST s (M s ()))

instance Monoid (EquationSystem s a) where
         mempty = EquationSystem $ return (return ())
         mappend (EquationSystem x) (EquationSystem y) =
           EquationSystem $ liftM2 (>>) x y


liftV2 :: 
  (T s a -> T s a -> T s a) -> 
  ExprWithVars s a -> ExprWithVars s a -> ExprWithVars s a
liftV2 f (ExprWithVars xs) (ExprWithVars ys) = ExprWithVars $ liftM2 f xs ys

-- Kann man hier mit Applicative und Functor noch was schoenen?
-- Scheitert wohl daran, dass beim fmap das f :: a -> T s a sein muesste?
instance (Num a, Fractional a) => Num (ExprWithVars s a) where
         --(ExprWithVars xs) * (ExprWithVars ys) = ExprWithVars $ liftM2 (*) xs ys
         --(ExprWithVars xs) * (ExprWithVars ys) = ExprWithVars $ liftM2 (fromRule3 R.mul) xs ys
         (*) = liftV2 (*)
         (+) = liftV2 (+)
         (-) = liftV2 (-)

         --(ExprWithVars xs) + (ExprWithVars ys) = ExprWithVars $ liftM2 (+) xs ys
         --(ExprWithVars xs) + (ExprWithVars ys) = ExprWithVars $ liftM2 (fromRule3 R.add) xs ys

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
makeEdgeEquations es = mconcat $ map mkEq es
  where mkEq (Edge f t) = power t f .= eta f t * power f t


makeEnergyEquations ::
  (Eq a, Fractional a) =>
  [Edge SecNode] -> EquationSystem s a
makeEnergyEquations es = mconcat $ map mkEq es
  where mkEq (Edge f@(SecNode sf nf) t@(SecNode st nt)) =
          mwhen (sf == st)
            (energy f t .= dt * power f t) <> (energy t f .= dt * power t f)
          where dt = dtime sf

makeNodeEquations ::
  (Eq a, Fractional a) =>
  SequFlowGraph -> EquationSystem s a
makeNodeEquations g = mconcat $
  map mkInEqs ins
  ++ map mkOutEqs outs
  ++ map mkSumEqs (ins ++ outs)
  where ins = M.toList $ fmap (S.toList) $ inEdges g
        outs = M.toList $ fmap (S.toList) $ outEdges g
        mkInEqs (n, ns) = (1 .= sum xs) <> (mconcat $ zipWith f energies xs)
          where xs = map (xfactor n) ns
                energies = map (energy n) ns
                f en x = en .= x * insum n 
        mkOutEqs (n, ns) = (1 .= sum xs) <> (mconcat $ zipWith f energies xs)
          where xs = map (xfactor n) ns
                energies = map (energy n) ns
                f en x = en .= x * outsum n
        mkSumEqs (n, _) = insum n .= outsum n



listToEnvs :: [(Env.Index, a)] -> Envs SingleRecord a
listToEnvs lst = L.foldl' f envs lst 
  where envs = emptyEnv { recordNumber = SingleRecord (Record Absolute) }
        f e (Env.Energy idx, v) =
          e { energyMap = M.insert idx v (energyMap e) }
        f e (Env.Power idx, v) =
          e { powerMap = M.insert idx v (powerMap e) }
        f e (Env.FEta idx, v) =
          e { fetaMap = M.insert idx (const v) (fetaMap e) }
        f e (Env.X idx, v) =
          e { xMap = M.insert idx v (xMap e) }
        f e (Env.DTime idx, v) =
          e { dtimeMap = M.insert idx v (dtimeMap e) }
        f e _ = e


solveSystem ::
  (Eq a, Fractional a) =>
  [(Env.Index, a)] -> SequFlowGraph -> Envs SingleRecord [a]
solveSystem given g = runST $ do
  let EquationSystem sys = makeAllEquations g

  (eqs, varmap) <- runStateT sys M.empty
  let f (var, val) =
        case (M.lookup var varmap) of
             Just v -> varToExprSys v .= constToExprSys val
             Nothing -> getVar var .= constToExprSys val
      EquationSystem gssys = mconcat $ map f given

  (gs, gsvars) <- runStateT gssys M.empty
  let allVars = M.toList $ M.union varmap gsvars

  solve (gs >> eqs)

  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  -- Map each element of a structure to an action,
  -- evaluate these actions from left to right, and collect the results. 

  res <- traverse (fmap maybeToList . snd . fmap query) allVars


  return $ listToEnvs (zip (map fst allVars) res)

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