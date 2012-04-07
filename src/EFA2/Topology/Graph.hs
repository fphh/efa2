{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module EFA2.Graph.Graph where

import Data.Maybe
import Data.Either
import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.List.HT as HTL

import Control.Monad.Error

import Debug.Trace

import EFA2.Utils.Utils
import EFA2.Graph.GraphData
import EFA2.Signal.Arith
import EFA2.Term.TermData
import EFA2.Term.Env
import EFA2.Term.Equation


-----------------------------------------------------------------------------------
-- Topology Graph
-- | This is the main topology graph representation.

--data TopoGraph = Graph NodeTyp deriving (Show, Eq, Ord)
--data NodeTyp = Storage | Sink | Source | Crossing deriving (Show, Ord, Eq)


mkLEdge :: Int -> Int -> LEdge ELabel
mkLEdge x y = (x, y, ELabel x y)

flipLEdge :: LEdge ELabel -> LEdge ELabel
flipLEdge (x, y, ELabel u v) = (y, x, ELabel v u)

mkLNode :: Int -> LNode NLabel
mkLNode x = (x, NLabel x)


makeEdges :: [Int] -> [LEdge ELabel]
makeEdges no = map (uncurry mkLEdge) (pairs no)

makeNodes :: [Int] -> [LNode NLabel]
makeNodes no = map mkLNode no


------------------------------------------------------------------------
-- Making equations:

mkEdgeEq :: Gr a b -> [EqTerm]
mkEdgeEq g = map f ns
  where ns = edges g
        f (x, y) = mkVar (PowerIdx y x) := FAbs (mkVar (PowerIdx x y)) (mkVar (EtaIdx x y))

{-
mkNodeEq :: Gr a b -> [EqTerm]
mkNodeEq g = concat $ mapGraph mkEq g

{- TODO: Rethink equations ineqs' and oeqs'. Currently they are invalid. What is wrong? Is is necessary? -}
{- ATTENTION: We must only produce equations, where every variable occurs only once.
This has to do with transformEq, which can only factor out variables that occure only once. -}
mkEq :: ([Node], Node, [Node]) -> [EqTerm]
mkEq ([], _, _) = []
mkEq (_, _, []) = []
--mkEq (ins, n, outs) = ieqs' ++ oeqs' ++ ieqs ++ oeqs
mkEq (ins, n, outs) = {- ieqs' ++ oeqs' ++ -} ieqs ++ oeqs ++ xieqs ++ xoeqs ++ ieqs'' ++ oeqs''
  where ins' = zip (repeat n) ins
        outs' = zip (repeat n) outs
        xis = map (uncurry mkX) ins'
        xos = map (uncurry mkX) outs'
        eis = map (uncurry mkEnergy) ins'
        eos = map (uncurry mkEnergy) outs'
        isum = add eis
        osum = add eos
        ieqs = zipWith3 f eis xis (repeat osum)
        oeqs = zipWith3 f eos xos (repeat isum)
        f x y z = x := Mult y z

        ieqs' | length eis > 1 = [g (head xis) (head eis) (tail eis)]
              | otherwise = []
        oeqs' | length eos > 1 = [g (head xos) (head eos) (tail eos)]
              | otherwise = []
        g x e es = e := (add $ map (Mult (Mult x (Recip (Add (Const 1.0) (Minus x))))) es)

        xieqs | length xis > 1 = [Const 1.0 := add xis]
              | otherwise = []
        xoeqs | length xos > 1 = [Const 1.0 := add xos]
              | otherwise = []

        ieqs'' | length eis > 1 = map h (zip xis (HTL.removeEach eis))
               | otherwise = []
        oeqs'' | length eos > 1 = map h (zip xos (HTL.removeEach eos))
               | otherwise = []
        h (x, (y, z)) = add z := Mult y (Add (Recip x) (Minus (Const 1.0)))

         
-}

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

