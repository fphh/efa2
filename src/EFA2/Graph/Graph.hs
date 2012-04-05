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
import EFA2.Term.Env


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

