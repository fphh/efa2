{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module EFA2.Topology.Topology where

import Data.Maybe
import Data.Either
--import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.List.HT as HTL

import Control.Monad.Error

import Debug.Trace

import EFA2.Solver.Equation
import EFA2.Interpreter.Env
import EFA2.Topology.TopologyData
import EFA2.Topology.EfaGraph
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

makeAllEquations :: (Show a) => Topology -> [Envs a] -> ([Envs a], [EqTerm])
makeAllEquations topo envs = (envs', ts)
  where ts = mkEdgeEq dirTopo ++ mkNodeEq dirTopo ++ enveqs
        dirTopo = makeDirTopology topo

        m = M.fromList $ map g (labNodes topo)
        g (nid, NLabel s r oldNid _) = ((s, r, oldNid), nid)
        envs' = map (shiftIndices m) envs

        enveqs = concatMap f envs'
        f (Envs p dp e de x v) = envToEqTerms p
                                 ++ envToEqTerms dp
                                 ++ envToEqTerms e
                                 ++ envToEqTerms de
                                 ++ envToEqTerms x
                                 ++ envToEqTerms v


shiftIndices :: (Show a) => M.Map (Int, Int, Int) Node -> Envs a -> Envs a
shiftIndices m (Envs p dp e de x v) = Envs p' dp' e' de' x' v'
  where p' = M.mapKeys pf p
        pf (PowerIdx s r f t) = PowerIdx s r (m M.! (s, r, f)) (m M.! (s, r, t))

        dp' = M.mapKeys dpf dp
        dpf (DPowerIdx s r f t) = DPowerIdx s r (m M.! (s, r, f)) (m M.! (s, r, t))

        e' = M.mapKeys ef e
        ef (EtaIdx s r f t) = EtaIdx s r (m M.! (s, r, f)) (m M.! (s, r, t))

        de' = M.mapKeys def de
        def (DEtaIdx s r f t) = DEtaIdx s r (m M.! (s, r, f)) (m M.! (s, r, t))

        x' = M.mapKeys xf x
        xf (XIdx s r f t) = XIdx s r (m M.! (s, r, f)) (m M.! (s, r, t))

        v' = M.mapKeys vf v
        vf (VarIdx s r f t) = VarIdx s r (m M.! (s, r, f)) (m M.! (s, r, t))


envToEqTerms :: (MkVarC k) => M.Map k v -> [EqTerm]
envToEqTerms m = map (give . fst) (M.toList m)


-- | Takes section, record, and a graph.
mkEdgeEq :: Topology -> [EqTerm]
mkEdgeEq (Topology g) = map f ns
  where ns = edges g
        f (x, y) = mkVar (PowerIdx ys yr y x) := (mkVar (PowerIdx xs xr x y)) :* (mkVar (EtaIdx xs xr x y))
          where NLabel xs xr _ _ = fromJust $ lab g x
                NLabel ys yr _ _ = fromJust $ lab g y

mkNodeEq :: Topology -> [EqTerm]
mkNodeEq topo = concat $ mapGraph mkEq topo

mkEq :: ([LNode NLabel], LNode NLabel, [LNode NLabel]) -> [EqTerm]
mkEq (ins, n@(nid, NLabel sec rec _ _), outs)
  | length ins == 0 && length outs == 0 = []
  | length ins == 0 && length outs > 0 = xoeqs ++ oeqs' ++ vosumeq
  | length ins > 0 && length outs == 0 = xieqs ++ ieqs' ++ visumeq
  | otherwise = vosumeq ++ oeqs ++ visumeq ++ ieqs ++ xoeqs ++ xieqs ++ oeqs' ++ ieqs'
  where xis = map (makeVar XIdx) ins
        xos = map (makeVar XIdx) outs
        eis = map (makeVar PowerIdx) ins
        eos = map (makeVar PowerIdx) outs
        -- For section and record, we focus on the current node n.
        makeVar mkIdx (nid', _) = mkVar $ mkIdx sec rec nid nid'

        visum = mkVar (VarIdx sec rec nid 0)
        visumeq = [visum := add eis]

        vosum = mkVar (VarIdx sec rec nid 1)
        vosumeq = [vosum := add eos]

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



-- | We sort in and out going edges according to 'FlowDirection'.
-- Undirected edges are filtered away.
-- This is important for creating correct equations.
makeDirTopology :: Topology -> Topology
makeDirTopology topo@(Topology _) = mkGraph ns es
  where es = map flipAgainst $ filter onlyDirected $ labEdges topo
        onlyDirected a@(_, _, elabel) = flowDirection elabel /= UnDir
        flipAgainst e@(x, y, elabel)
          | AgainstDir <- flowDirection elabel = (y, x, elabel { flowDirection = WithDir })
          | otherwise = e
        ns = unique (concatMap (\(x, y, _) -> [(x,  fromJust (lab topo x)), (y, fromJust (lab topo y))]) es)
