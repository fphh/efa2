{-# LANGUAGE FlexibleInstances #-}

module EFA2.Topology.Topology2 where

import qualified Data.Map as M
 
import EFA2.Signal.Index as Idx
import EFA2.Topology.EfaGraph as Gr

import EFA2.Topology.TopologyData (SequFlowGraph)
import EFA2.Solver.Equation
import UniqueLogic.ST.Expression as Exp
import UniqueLogic.ST.System as Sys

import Control.Monad.ST
import Control.Monad

import Data.Monoid

import EFA2.Interpreter.Env as Env

import Debug.Trace

-----------------------------------------------------------------------------------
-- Topology Graph
-- | This is the main topology graph representation.

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


-----------------------------------------------------------------------------------

-- type für Gleichungssystem.
{-
  data Equations =
     Equations
        (ST s (Envs MixedRecord (System.Variable s a)))
        (System.M s ()) 

-}

newtype Equations s a = Equations (ST s (MWithVars s a))

edges :: Gr.EfaGraph node nodeLabel edgeLabel -> [Gr.Edge node]
edges g = M.keys el
  where el = Gr.edgeLabels g


makeVar ::
  (MkIdxC a) =>
  (Record -> Idx.SecNode -> Idx.SecNode -> a) ->
  Idx.SecNode -> Idx.SecNode -> Env.Index
makeVar idxf nid nid' =
  mkVar $ idxf (Record Absolute) nid nid'


makeAllEquations ::
  (Eq a, Fractional a) =>
  SequFlowGraph -> ST s (MWithVars s a)
makeAllEquations g = makeEtaEquations (edges g)

{-
instance (Monad m0, Monad m1, Monoid l) =>
  Monoid (Equations (m0 (l, m1 ())))  where
    mempty = return $ Equations (mempty, return ())
    -- mappend x y = x >>= \(xs, m) -> y >>= \(ys, n) -> return (mappend xs ys, m >> n)

    mappend x y = do
      (xs, m) <- x
      (ys, n) <- y
      return (mappend xs ys, do { m; n })
-}

instance (Monad m0, Monad m1) =>
  Monoid (m0 ([x], m1 ()))  where
    mempty = return ([], return ())
    -- mappend x y = x >>= \(xs, m) -> y >>= \(ys, n) -> return (xs ++ ys, m >> n)
    mappend x y = do
      (xs, m) <- x
      (ys, n) <- y
      return (xs ++ ys, do { m; n })


getVar :: Env.Index -> ST s (TWithVars s a)
getVar idx = do
  var <- globalVariable
  return ([(idx, var)], fromVariable var)

{-
type VarEnvs s a = Envs MixedRecord (Variable s a)

class EnvsPut a where
      putIntoEnv :: a -> b -> Envs Absolute b -> Envs Absolute b

instance EnvsPut Idx.Power where
         putIntoEnv k v envs = envs { powerMap = M.insert k v (powerMap envs) }

instance EnvsPut Idx.FEta where
         putIntoEnv k v envs = envs { fetaMap = M.insert k v (fetaMap envs) }
-}


-- T = Typ => TWithVars = ExpressionWithVars
type TWithVars s a = ([(Env.Index, Variable s a)], T s a)

-- M = Monad => SystemWithVars
type MWithVars s a = ([(Env.Index, Variable s a)], M s ())

-- .= .*: auf ST s (MWithVars s a) Ebene definieren

(.=) :: (Eq a) => TWithVars s a -> TWithVars s a -> MWithVars s a
(vs, x) .= (us, y) = (vs ++ us, x =:= y)

(.*) :: (Fractional a) => TWithVars s a -> TWithVars s a -> TWithVars s a
(vs, x) .* (us, y) = (vs ++ us, x * y)


infix 0 .=
infix 5 .*


makeEtaEquations ::
  (Eq a, Fractional a) =>
  [Edge SecNode] -> ST s (MWithVars s a)
makeEtaEquations es = mconcat $ map mkEq es
  where mkEq :: (Eq a0, Fractional a0) => Edge SecNode -> ST s0 (MWithVars s0 a0)
        mkEq (Edge from to) = liftM3 eq (getVar piname) (getVar poname) (getVar etaname)
          where eq pin pout eta = pout .= pin .* eta
                piname = makeVar Idx.Power from to
                poname = makeVar Idx.Power to from
                etaname = makeVar Idx.FEta from to


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

(.==) :: Variable s Double -> Double -> M s ()
(.==) zv d = fromVariable zv =:= Exp.constant d

solveIt :: M s a -> M s b -> ST s c -> ST s c
solveIt given sys getVal = solve (given >> sys) >> getVal


solveItExample2 :: Double -> (Maybe Double, Maybe Double, Maybe Double)
solveItExample2 d = runST $ do
  ((xv, yv, zv), eqs) <- example2
  let getValues = liftM3 (,,) (query xv) (query yv) (query zv)
  solveIt (zv .== d) eqs getValues
