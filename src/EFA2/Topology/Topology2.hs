-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE RankNTypes #-}

module EFA2.Topology.Topology2 where

import qualified Data.Map as M
import qualified Data.List as L

import EFA2.Signal.Index as Idx
import EFA2.Topology.EfaGraph as Gr

import EFA2.Topology.TopologyData (SequFlowGraph)
import EFA2.Solver.Equation
import UniqueLogic.ST.Expression as Exp
import UniqueLogic.ST.System as Sys

import Control.Monad.ST
import Control.Monad

import Data.Monoid
import Data.Maybe

import EFA2.Interpreter.Env as Env

-- import Debug.Trace

-----------------------------------------------------------------------------------
-- Topology Graph
-- | This is the main topology graph representation.

{-
makeNodes :: [(Int, nt)] -> [Gr.LNode Idx.Node nt]
makeNodes ns = map f ns
  where f (n, ty) = (Idx.Node n, ty)

makeEdges :: [(Int, Int, el)] -> [Gr.LEdge Idx.Node el]
makeEdges es = map f es
  where f (a, b, l) = (Gr.Edge (Idx.Node a) (Idx.Node b), l)

{-
makeWithDirEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node FlowDirection]
makeWithDirEdges es = map f es
  where f (a, b) = (Gr.Edge (Idx.Node a) (Idx.Node b), WithDir)
-}

makeSimpleEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node ()]
makeSimpleEdges es = map f es
  where f (a, b) = (Gr.Edge (Idx.Node a) (Idx.Node b), ())
-}

-----------------------------------------------------------------------------------

type ProvEnv s a = [(Env.Index, Variable s a)]
-- type ProvEnv s a = M.Map Env.Index (Variable s a)

type ExpWithVars s a = ST s (ProvEnv s a, T s a)

type SysWithVars s a = ST s (ProvEnv s a, M s ())

newtype EquationSystem s a = EquationSystem (SysWithVars s a)

newtype GivenEquations s a = GivenEquations (ST s (M s ()))

instance Monoid (EquationSystem s a) where
         mempty = EquationSystem $ return (mempty, return ())
         mappend (EquationSystem x) (EquationSystem y) = EquationSystem $ do
           (xs, m) <- x
           (ys, n) <- y
           return (mappend xs ys, do { m; n })



getVar :: Env.Index -> ExpWithVars s a
getVar idx = do
  var <- globalVariable
  let -- env = M.singleton idx var
      env = [(idx, var)]
  return (env, fromVariable var)


(.=) :: (Eq a) => ExpWithVars s a -> ExpWithVars s a -> EquationSystem s a
xs .= ys = EquationSystem $ 
  xs >>= \(vs, x) -> ys >>= \(us, y) -> return (mappend vs us, x =:= y)

(.*) :: (Fractional a) => ExpWithVars s a -> ExpWithVars s a -> ExpWithVars s a
xs .* ys = xs >>= \(vs, x) -> ys >>= \(us, y) -> return (mappend vs us, x * y)

infix 0 .=
infix 5 .*

constToExpSys :: a -> ExpWithVars s a
constToExpSys c = return (mempty, Exp.constant c)

varToExpSys :: Variable s a -> ExpWithVars s a
varToExpSys v = return (mempty, fromVariable v)

makeVar ::
  (MkIdxC a) =>
  (Record -> Idx.SecNode -> Idx.SecNode -> a) ->
  Idx.SecNode -> Idx.SecNode -> Env.Index
makeVar idxf nid nid' =
  mkVar $ idxf (Record Absolute) nid nid'

power :: SecNode -> SecNode -> ExpWithVars s a
power = (getVar .) . makeVar Idx.Power

eta :: SecNode -> SecNode -> ExpWithVars s a
eta = (getVar .) . makeVar Idx.FEta

edges :: Gr.EfaGraph node nodeLabel edgeLabel -> [Gr.Edge node]
edges g = M.keys el
  where el = Gr.edgeLabels g

makeAllEquations ::
  (Eq a, Fractional a) =>
  SequFlowGraph -> EquationSystem s a
makeAllEquations g = makeEtaEquations (edges g)

makeEtaEquations ::
  (Eq a, Fractional a) =>
  [Edge SecNode] -> EquationSystem s a
makeEtaEquations es = mconcat $ map mkEq es
  where mkEq (Edge f t) = power t f .= eta f t .* power f t


listToEnvs :: [(Env.Index, a)] -> Envs SingleRecord a
listToEnvs lst = L.foldl' f envs lst 
  where envs = emptyEnv { recordNumber = SingleRecord (Record Absolute) }
        f e (Env.Power idx, v) =
          e { powerMap = M.insert idx v (powerMap e) }
        f e (Env.FEta idx, v) =
          e { fetaMap = M.insert idx (\_ -> v) (fetaMap e) }
        f e (Env.DTime idx, v) =
          e { dtimeMap = M.insert idx v (dtimeMap e) }


solveSystem ::
  (Eq a, Fractional a) =>
  [(Env.Index, a)] -> SequFlowGraph -> Envs SingleRecord [a]
solveSystem given g = runST $ do
  let EquationSystem sys = makeAllEquations g

  (vars, eqs) <- sys
  let varmap = M.fromList vars
      f (var, val) =
        case (M.lookup var varmap) of
             Just v -> varToExpSys v .= constToExpSys val
             Nothing -> getVar var .= constToExpSys val
      EquationSystem gssys = mconcat $ map f given

  (gsvars, gs) <- gssys
  let allVars = vars ++ gsvars

  solve (gs >> eqs)

  res <- mapM (query . snd) allVars
  return $ listToEnvs (zip (map fst allVars) (map maybeToList res))

-----------------------------------------------------------------------


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
(.==) zv d = fromVariable zv =:= Exp.constant d


solveIt :: [M s a] -> M s b -> ST s c -> ST s c
solveIt given eqs getVal = solve (sequence given >> eqs) >> getVal
{-
allPositions :: SequFlowGraph -> Envs NoRecord ()
allPositions g = L.foldl' f emptyEnv es
  where es = edges g
        f env (Edge f t) = 
          env { powerMap = M.insert (Idx.Power (Idx.Record Absolute) f t) () (powerMap env),
                xMap = M.insert (Idx.X (Idx.Record Absolute) f t) () (xMap env),
                fetaMap = M.insert (Idx.FEta (Idx.Record Absolute) f t) (\_ -> ()) (fetaMap env)}

solveSystem ::
  (Eq a, Fractional a) =>
  [(Env.Index, a)] -> SequFlowGraph -> Envs SingleRecord [a]
solveSystem given g = runST $ do
  let EquationSystem sys = makeAllEquations g
  (vars, eqs) <- sys
  let varmap = M.fromList vars
      f (var, val) = trace (show var) $ case (M.lookup var varmap) of
                          Nothing -> do xvar <- globalVariable
                                        fromVariable xvar .== val
                          Just v -> v .== val
      gs = map f given
      -- ws = map (forceMaybe . flip M.lookup varmap) wanted
      posEnvs = allPositions g
      func idx _ = [1.0] -- query (forceMaybe (M.lookup idx varmap))
      --valEnvs = emptyEnv { powerMap = M.mapWithKey func (powerMap posEnvs) }
  solve (sequence gs >> eqs)
  -- solveIt gs eqs (query (ws !! 0))
  --return valEnvs
  return $ emptyEnv { recordNumber = SingleRecord (Record Absolute),
                      powerMap = M.mapWithKey func (powerMap posEnvs) }
-}


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
