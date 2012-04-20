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

import EFA2.Utils.Utils
import EFA2.Solver.Equation
import EFA2.Interpreter.InTerm
import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env

-----------------------------------------------------------------------------------
-- Topology Graph
-- | This is the main topology graph representation.

data NodeType = Storage | Sink | Source | Crossing deriving (Show, Ord, Eq)

data NLabel = NLabel { sectionNLabel :: Int,
                       recordNLabel :: Int,
                       nodeNLabel :: Int,
                       nodetypeNLabel :: NodeType } deriving (Show, Eq, Ord)

--data ELabel = ELabel deriving (Show, Eq, Ord)


--mkLEdge :: Int -> Int -> LEdge ELabel
--mkLEdge x y = (x, y, ELabel x y)

--flipLEdge :: LEdge ELabel -> LEdge ELabel
--flipLEdge (x, y, ELabel u v) = (y, x, ELabel v u)

--mkLNode :: Int -> LNode NLabel
--mkLNode n = map (NLabel 0 0) ns

makeNodes :: [(Int, NodeType)] -> [LNode NLabel]
makeNodes ns = map f ns
  where f (n, ty) = (n, NLabel 0 0 n ty)

makeEdges :: [(Int, Int)] -> [LEdge ()]
makeEdges es = map f es
  where f (a, b) = (a, b, ())

------------------------------------------------------------------------
-- Making equations:

-- | Takes section, record, and a graph.
mkEdgeEq :: Gr NLabel () -> [EqTerm]
mkEdgeEq g = map f ns
  where ns = edges g
        f (x, y) = mkVar (PowerIdx ys yr yn xn) := (mkVar (PowerIdx xs xr xn yn)) :* (mkVar (EtaIdx xs xr xn yn))
          where (NLabel xs xr xn _) = fromJust $ lab g x
                (NLabel ys yr yn _) = fromJust $ lab g y

mkNodeEq :: Gr NLabel () -> [EqTerm]
mkNodeEq g = concat $ mapGraph mkEq g

-- | ATTENTION: We must only produce equations, where every variable occurs only once.
-- This has to do with 'transformEq', which can only factor out variables that occure only once.
mkEq :: ([NLabel], NLabel, [NLabel]) -> [EqTerm]
mkEq (ins, n, outs)
  | length ins == 0 && length outs == 0 = []
  | length ins == 0 && length outs > 0 = xoeqs ++ oeqs'
  | length ins > 0 && length outs == 0 = xieqs ++ ieqs'
  | otherwise = oeqs ++ ieqs ++ xoeqs ++ xieqs ++ oeqs' ++ ieqs'
  where ins' = zip (repeat n) ins
        outs' = zip (repeat n) outs

        xis = map (makeVar XIdx) ins'
        xos = map (makeVar XIdx) outs'
        eis = map (makeVar PowerIdx) ins'
        eos = map (makeVar PowerIdx) outs'
        -- For section and record, we focus on the current node n.
        makeVar mkIdx ((NLabel s r n _), (NLabel _ _ n' _)) = mkVar $ mkIdx s r n n'

        isum = add eis
        osum = add eos
        ieqs = zipWith3 f eis xis (repeat osum)
        oeqs = zipWith3 f eos xos (repeat isum)
        f x y z = x := y :* z

        xieqs | length xis > 0 = [Const 1.0 := add xis]
              | otherwise = []
        xoeqs | length xos > 0 = [Const 1.0 := add xos]
              | otherwise = []

        ieqs' | length eis > 1 = map g $ pairs $ zipWith (,) xis eis
              | otherwise = []
        oeqs' | length eos > 1 = map g $ pairs $ zipWith (,) xos eos
              | otherwise = []
        g ((x1, e1), (x2, e2)) = x1 :* e2 := x2 :* e1

         


----------------------------------------------------------------------------------
-- Classes to allow indexing of power positions, etas and nodes


{-
instance (Arith a) => EnvClass [a] where
         mkPowerEnv = mkPowerValEnv
         mkEtaEnv = mkEtaValEnv
         mkXEnv = mkXValEnv
-}
{-
mkPowerValEnv :: (M.Map PowerIdx [a]) -> LRPowerEnv [a]
mkPowerValEnv m x = checkIdx PowerIdxError x (M.map Right m)

mkEtaValEnv :: (Arith c) => Gr a b -> LRPowerEnv [c] -> LREtaEnv [c]
mkEtaValEnv g penv x = checkIdx EtaIdxError x etas
  where es = edges g
        etas = M.fromList $ map h (zip es (map f es))
        f (x, y) = do
          p <- penv (PowerIdx x y)
          q <- penv (PowerIdx y x)
          return (zipWith (./) q p)
        h ((x, y), v) = (EtaIdx x y, v)

mkXValEnv :: (Arith c) => Gr a b -> LRPowerEnv [c] -> LRXEnv [c]
mkXValEnv g penv x = checkIdx XIdxError x xs
  where xs = M.fromList $ foldGraph f [] g
        f acc (ins, x, outs) = h ins x ++ h outs x ++ acc
        h ns x = zip xidx xs
          where es = zip (repeat x) ns
                xidx = map (uncurry XIdx) es
                pidx = map (uncurry PowerIdx) es
                ps = map penv pidx
                psums = L.foldl' add (Right (repeat zero)) pidx
                xs = map (flip div psums) ps
        add (Right acc) idx = do
          p <- penv idx
          return (zipWith (.+) p acc)
        add err@(Left _) _ = err
        div (Right as) (Right bs) = Right (zipWith (./) as bs)
        div err@(Left _) _ = err
        div _ err@(Left _) = err
-}

----------------------------------------------------------------------------------

