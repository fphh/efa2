{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module EFA2.Topology.Topology where

import Data.Maybe
import Data.Either
import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.List.HT as HTL

import Control.Monad.Error

import Debug.Trace

import EFA2.Solver.Equation
import EFA2.Interpreter.Env
import EFA2.Topology.TopologyData
import EFA2.Utils.Utils

-----------------------------------------------------------------------------------
-- Topology Graph
-- | This is the main topology graph representation.

makeNodes :: [(Int, NodeType)] -> [LNode NLabel]
makeNodes ns = map f ns
  where f (n, ty) = (n, NLabel 0 0 n ty)

makeEdges :: [(Int, Int, ELabel)] -> [LEdge ELabel]
makeEdges es = map f es
  where f (a, b, l) = (a, b, l)

makeWithDirEdges :: [(Int, Int)] -> [LEdge ELabel]
makeWithDirEdges es = map f es
  where f (a, b) = (a, b, defaultELabel)



------------------------------------------------------------------------
-- Making equations:

makeAllEquations :: Topology -> [EqTerm]
makeAllEquations topo = mkEdgeEq dirTopo ++ mkNodeEq dirTopo
  where dirTopo = makeDirTopology topo

-- | Takes section, record, and a graph.
mkEdgeEq :: Topology -> [EqTerm]
mkEdgeEq (Topology g) = map f ns
  where ns = edges g
        f (x, y) = mkVar (PowerIdx ys yr yn xn) := (mkVar (PowerIdx xs xr xn yn)) :* (mkVar (EtaIdx xs xr xn yn))
          where (NLabel xs xr xn _) = fromJust $ lab g x
                (NLabel ys yr yn _) = fromJust $ lab g y

mkNodeEq :: Topology -> [EqTerm]
mkNodeEq topo = concat $ mapGraph mkEq topo

-- | ATTENTION: We must only produce equations, where every variable occurs only once.
-- This has to do with 'transformEq', which can only factor out variables that occure only once.
mkEq :: ([NLabel], NLabel, [NLabel]) -> [EqTerm]
mkEq (ins, n@(NLabel rec sec from to), outs)
  | length ins == 0 && length outs == 0 = []
  | length ins == 0 && length outs > 0 = xoeqs ++ oeqs' ++ vosumeq
  | length ins > 0 && length outs == 0 = xieqs ++ ieqs' ++ visumeq
  | otherwise = vosumeq ++ oeqs ++ visumeq ++ ieqs ++ xoeqs ++ xieqs ++ oeqs' ++ ieqs'
  where 

        ins' = zip (repeat n) ins
        outs' = zip (repeat n) outs

        xis = map (makeVar XIdx) ins'
        xos = map (makeVar XIdx) outs'
        eis = map (makeVar PowerIdx) ins'
        eos = map (makeVar PowerIdx) outs'
        -- For section and record, we focus on the current node n.
        makeVar mkIdx ((NLabel s r n _), (NLabel _ _ n' _)) = mkVar $ mkIdx s r n n'

        visum = mkVar (VarIdx sec rec from 0)
        visumeq = [visum := add eis]

        vosum = mkVar (VarIdx sec rec from 1)
        vosumeq = [vosum := add eos]

        --isum = add eis
        --osum = add eos
        ieqs = zipWith3 f eis xis (repeat vosum)
        oeqs = zipWith3 f eos xos (repeat visum)
        f x y z = x := y :* z

        xieqs | length xis > 0 = [Const 1.0 := add xis]
              | otherwise = []
        xoeqs | length xos > 0 = [Const 1.0 := add xos]
              | otherwise = []

        ieqs' | length eis > 1 = zipWith (g visum) xis eis
              | otherwise = []
        oeqs' | length eos > 1 = zipWith (g vosum) xos eos
              | otherwise = []
        g v x e = x := e :* Recip v

{-
        oeqs' | length eos > 1 = concatMap g $ pairs $ zipWith (,) xos eos
              | otherwise = []
-}

{-
        ieqs' | length eis > 1 = concatMap g $ pairs $ zipWith (,) xis eis
              | otherwise = []
        oeqs' | length eos > 1 = concatMap g $ pairs $ zipWith (,) xos eos
              | otherwise = []
        g ((x1@(X (XIdx s r x y)), e1), (x2, e2)) = [x1 :* e2 := x2 :* e1]
-} 

-- | We sort in and out going edges according to 'FlowDirection'.
-- Undirected edges are filtered away.
-- This is important for creating correct equations.
makeDirTopology :: Topology -> Topology
makeDirTopology topo = mkGraph ns es
  where es = map flipAgainst $ filter onlyDirected $ labEdges topo
        onlyDirected (_, _, elabel) = flowDirection elabel /= UnDir
        flipAgainst e@(x, y, elabel)
          | AgainstDir <- flowDirection elabel = (y, x, elabel { flowDirection = WithDir })
          | otherwise = e
        ns = unique (concatMap (\(x, y, _) -> [(x,  fromJust (lab topo x)), (y, fromJust (lab topo y))]) es)
