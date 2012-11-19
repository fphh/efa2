{-# LANGUAGE FlexibleContexts #-}

module EFA2.Topology.Topology2 where

import qualified Data.Map as M
 
import EFA2.Signal.Index as Idx
import EFA2.Topology.EfaGraph as Gr

import EFA2.Interpreter.Env (Envs, SingleRecord, MixedRecord)
import EFA2.Topology.TopologyData (FlowDirection(..), SequFlowGraph)
import EFA2.Solver.Equation -- (Equation(..))
import UniqueLogic.ST.Expression
import UniqueLogic.ST.Rule
import UniqueLogic.ST.System

import Control.Monad.ST
import Control.Monad

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

makeWithDirEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node FlowDirection]
makeWithDirEdges es = map f es
  where f (a, b) = (Gr.Edge (Idx.Node a) (Idx.Node b), WithDir)

makeSimpleEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node ()]
makeSimpleEdges es = map f es
  where f (a, b) = (Gr.Edge (Idx.Node a) (Idx.Node b), ())


-----------------------------------------------------------------------------------

edges :: Gr.EfaGraph node nodeLabel edgeLabel -> [Gr.Edge node]
edges g = M.keys el
  where el = Gr.edgeLabels g


makeVar ::
  (MkIdxC a) =>
  (Record -> Idx.SecNode -> Idx.SecNode -> a) ->
  Idx.SecNode -> Idx.SecNode -> Env.Index
makeVar mkIdx nid nid' =
  mkVar $ mkIdx (Record Absolute) nid nid'

type VarName = String


makeAllEquations :: 
  (Eq a, Fractional a, MonadPlus (M s)) =>
  SequFlowGraph -> ST s ([(Index, Variable s a)], M s ())
makeAllEquations g = makeEtaEquations (edges g)


fmapPair :: (a -> b, c -> d) -> (a, c) -> (b, d)
fmapPair (f, g) (x, y) = (f x, g y)

rearrange :: 
  (Monad (m0 s), MonadPlus (m1 s)) => 
  m0 s [([x], m1 s t)] -> m0 s ([x], m1 s t)
rearrange = liftM (fmapPair (join, msum) . unzip)

getVars :: [Index] -> ST s ([(Index, Variable s a)], [T s a])
getVars idxs = do
  vars <- sequence (replicate (length idxs) globalVariable)
  let vars' = map fromVariable vars
  return (zip idxs vars, vars')

makeEtaEquations ::
  (Eq a, Fractional a, MonadPlus (M s)) =>
  [Edge SecNode] -> ST s ([(Index, Variable s a)], M s ())
makeEtaEquations es = rearrange $ mapM mkEq es
  where mkEq (Edge from to) = liftM (fmap eq) $ getVars names
          where eq [pi, po, eta] = po =:= pi * eta
                names = [piname, poname, etaname]
                piname = makeVar Idx.Power from to
                poname = makeVar Idx.Power to from
                etaname = makeVar Idx.FEta from to
                


{-
makeEtaEquations ::
  (Eq a, Fractional a, MonadPlus (M s)) =>
  [Edge t] -> ST s ([Variable s a], M s ())
makeEtaEquations es = rearrange $ mapM mkEq es
  where mkEq (Edge from to) = do
          vars <- sequence (replicate 3 globalVariable)
          let [pin', pout', eta'] = map fromVariable vars
          return (vars, pout' =:= pin' * eta')
-}
   


{-
makeEtaEquations ::
  (Eq a, Fractional a, MonadPlus (M s)) =>
  [Edge t] -> ST s ([Variable s a], M s ())
makeEtaEquations es = rearrange $ mapM mkEq es
  where mkEq (Edge from to) = do
          (vars, [pin', pout', eta']) <- getVars 3
          return (vars, pout' =:= pin' * eta')
-}


{-
mkEdgeEq :: Idx.Record -> SequFlowGraph -> [Equation]
mkEdgeEq recordNum =
   map f . M.keys .
   M.filter (not . isIntersectionEdge) . edgeLabels
  where f (Edge x y) =
           EqEdge
              (mkVar $ Idx.Power recordNum x y)
              (mkVar $ Idx.FEta recordNum x y)
              (mkVar $ Idx.Power recordNum y x)
-}

-----------------------------------------------------------------------

example :: ST s ([Variable s Double], M s ())
example = do
  vars@[xv, yv, zv] <- sequence (replicate 3 globalVariable)

  let given = do
        let z = fromVariable zv
        z =:= 2

  let eqs = do
        let x = fromVariable xv
            y = fromVariable yv
            z = fromVariable zv
        x*3 =:= y/2
        5 =:= 2+x+z

  return $ (vars, do { given; eqs })


solveItExample :: [Maybe Double]
solveItExample = runST $ do
  (vars, eqs) <- example
  solve eqs
  mapM query vars

{-
-- solveIt :: ST s ([Variable s a], M s b) -> [Maybe a]
solveIt sys = runST $ do
  (vars, eqs) <- sys
  solve eqs
  mapM query vars
-}