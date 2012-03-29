{-# LANGUAGE FlexibleInstances #-}

module EFA2.Graph.Graph where

import Data.Maybe
import Data.Either
import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Map as M

import Control.Monad.Error

import Debug.Trace

import EFA2.Graph.GraphData
import EFA2.Utils.Utils

import EFA2.Signal.TH
import EFA2.Term.TermData


-----------------------------------------------------------------------------------
-- Topology Graph
-- | This is the main topology graph representation.

--data TopoGraph = Graph NodeTyp deriving (Show, Eq, Ord)
--data NodeTyp = Storage | Sink | Source | Crossing deriving (Show, Ord, Eq)

data NLabel = NLabel Int deriving (Show)
data ELabel = ELabel Int Int deriving (Show)


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


data IdxError = PowerIdxError PowerIdx
              | EtaIdxError EtaIdx
              | XIdxError XIdx
              | OtherError String deriving (Eq, Show)

instance Error IdxError where
         noMsg = OtherError "Unknown index error!" 
         strMsg str = OtherError str

type IdxErrorMonad = Either IdxError

type LRNodeEnv a = NodeIdx -> IdxErrorMonad a
type LREtaEnv a = EtaIdx -> IdxErrorMonad a
type LRPowerEnv a = PowerIdx -> IdxErrorMonad a
type LRXEnv a = XIdx -> IdxErrorMonad a

type NodeEnv a = NodeIdx -> a
type EtaEnv a = EtaIdx -> a
type PowerEnv a = PowerIdx -> a
type XEnv a = XIdx -> a


composeLREnv :: (a -> IdxErrorMonad b) -> (a -> IdxErrorMonad b) -> (a -> IdxErrorMonad b)
composeLREnv env1 env2 x
  | y@(Right _) <- env1 x = y
  | otherwise = env2 x

mkEnv :: (a -> IdxErrorMonad b) -> (a -> b)
mkEnv env x
  | Left err <- res = error (show err)
  | Right y <- res = y
  where res = env x

checkIdx :: (Ord a, Show b) => (a -> IdxError) -> a -> M.Map a (IdxErrorMonad b) -> IdxErrorMonad b
checkIdx err x vs | Just y <- M.lookup x vs = y
                  | otherwise = throwError (err x)

class EnvClass a where
      mkPowerEnv :: (M.Map PowerIdx a) -> LRPowerEnv a
      mkEtaEnv :: Gr b c -> LRPowerEnv a -> LREtaEnv a
      mkXEnv :: Gr b c -> LRPowerEnv a -> LRXEnv a

instance EnvClass [Val] where
         mkPowerEnv = mkPowerValEnv
         mkEtaEnv = mkEtaValEnv
         mkXEnv = mkXValEnv

instance EnvClass (InTerm Abs) where
         mkPowerEnv = undefined
         mkEtaEnv = undefined
         mkXEnv = undefined

mkPowerValEnv :: (M.Map PowerIdx [Val]) -> LRPowerEnv [Val]
mkPowerValEnv m x = checkIdx PowerIdxError x (M.map Right m)

mkEtaValEnv :: Gr a b -> LRPowerEnv [Val] -> LREtaEnv [Val]
mkEtaValEnv g penv x = checkIdx EtaIdxError x etas
  where es = edges g
        etas = M.fromList $ map h (zip es (map f es))
        f (x, y) = do
          p <- penv (PowerIdx x y)
          q <- penv (PowerIdx y x)
          return (zipWith (/) q p)
        h ((x, y), v) = (EtaIdx x y, v)

mkXValEnv :: Gr a b -> LRPowerEnv [Val] -> LRXEnv [Val]
mkXValEnv g penv x = checkIdx XIdxError x xs
  where xs = M.fromList $ foldGraph f [] g
        f acc (ins, x, outs) = zip ixidx ixs ++ zip oxidx oxs ++ acc
          where 
                oes = zip (repeat x) outs
                oxidx = map (uncurry XIdx) oes
                opidx = map (uncurry PowerIdx) oes
                ops = map penv opidx
                opsums = L.foldl' add (Right (repeat 0)) opidx
                oxs = map (flip div opsums) ops

                ies = zip (repeat x) ins
                ixidx = map (uncurry XIdx) ies
                ipidx = map (uncurry PowerIdx) ies
                ips = map penv ipidx
                ipsums = L.foldl' add (Right (repeat 0)) ipidx
                ixs = map (flip div ipsums) ips
{-
                g ns x = zip ixidx ixs                
                  where ies = zip (repeat x) ins
                        ixidx = map (uncurry mkXIdx) ies
                        ipidx = map (uncurry mkPowerIdx) ies
                        ips = map penv ipidx
                        ipsums = L.foldl' add (Right (repeat 0)) ipidx
                        ixs = map (flip div ipsums) ips
-}

        add (Right acc) idx = do
          p <- penv idx
          return (zipWith (+) p acc)
        add err@(Left _) _ = err
        div (Right as) (Right bs) = Right (zipWith (/) as bs)
        div err@(Left _) _ = err
        div _ err@(Left _) = err


----------------------------------------------------------------------------------

