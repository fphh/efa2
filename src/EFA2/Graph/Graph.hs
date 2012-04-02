{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module EFA2.Graph.Graph where

import Data.Maybe
import Data.Either
import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Map as M

import Control.Monad.Error

import Debug.Trace

import EFA2.Utils.Utils
import EFA2.Graph.GraphData
import EFA2.Signal.Arith
import EFA2.Term.TermData


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


----------------------------------------------------------------------------------
-- Classes to allow indexing of power positions, etas and nodes



composeLREnv :: (a -> IdxErrorMonad b) -> (a -> IdxErrorMonad b) -> (a -> IdxErrorMonad b)
composeLREnv env1 env2 x
  | y@(Right _) <- env1 x = y
  | otherwise = env2 x

mkEnv :: (a -> IdxErrorMonad b) -> (a -> b)
mkEnv env x
  | Left err <- res = error (show err)
  | Right y <- res = y
  where res = env x

checkIdx :: (Ord a) => (a -> IdxError) -> a -> M.Map a (IdxErrorMonad b) -> IdxErrorMonad b
checkIdx err x vs | Just y <- M.lookup x vs = y
                  | otherwise = throwError (err x)

instance (Arith a) => EnvClass [a] where
         mkPowerEnv = mkPowerValEnv
         mkEtaEnv = mkEtaValEnv
         mkXEnv = mkXValEnv

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

mkSymPowerEnv :: Int -> PowerIdx -> M.Map PowerIdx [InTerm]
mkSymPowerEnv n idx = M.singleton idx (replicate n (PIdx idx))

mkSymEtaEnv :: LREtaEnv [InTerm]
mkSymEtaEnv idx = Right (repeat (EIdx idx))

mkSymDPowerEnv :: Int -> DPowerIdx -> M.Map DPowerIdx [InTerm]
mkSymDPowerEnv n idx = M.singleton idx (replicate n (DPIdx idx))

mkSymDEtaEnv :: LRDEtaEnv [InTerm]
mkSymDEtaEnv idx = Right (repeat (DEIdx idx))


mkSymXEnv :: LRXEnv [InTerm]
mkSymXEnv idx = Right (repeat (ScaleIdx idx))


solveInTerms :: (Interpreter a, EnvClass a) => M.Map PowerIdx a -> LREtaEnv a -> LRXEnv a -> [InTerm] -> M.Map PowerIdx a
solveInTerms penv eenv xenv ts = M.fromList $ snd $ L.foldl' f (penv', []) ts
  where eenv' = mkEnv eenv
        xenv' = mkEnv xenv
        penv' = mkPowerEnv penv
        f (pacc, sol) (InEqual (PIdx idx) t) = (newPEnv `composeLREnv` pacc, (idx, val):sol)
          where val = interpret (mkEnv pacc) eenv' xenv' t
                newPEnv x | idx == x = return val
                newPEnv idx = throwError (PowerIdxError idx)

{-
solveDInTerms :: (Interpreter a, EnvClass a) => M.Map PowerIdx a -> LREtaEnv a -> LRXEnv a -> [InTerm] -> M.Map PowerIdx a
solveDInTerms penv eenv xenv ts = M.fromList $ snd $ L.foldl' f (penv', []) ts
  where eenv' = mkEnv eenv
        xenv' = mkEnv xenv
        penv' = mkPowerEnv penv
        f (pacc, sol) (InEqual (PIdx idx) t) = (newPEnv `composeLREnv` pacc, (idx, val):sol)
          where val = interpretD (mkEnv pacc) eenv' xenv' t
                newPEnv x | idx == x = return val
                newPEnv idx = throwError (PowerIdxError idx)
-}

{-
instance EnvClass [InTerm a] where
         mkPowerEnv = undefined
         mkEtaEnv = undefined
         mkXEnv = undefined
-}

----------------------------------------------------------------------------------

