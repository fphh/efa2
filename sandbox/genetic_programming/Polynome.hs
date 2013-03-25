{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Polynome where

import qualified Data.List as L
import qualified Data.Map as M
import Genetic

-- import Debug.Trace


data Term' var = X var
               | Const Double
               | Minus (Term' var)
               | (Term' var) :+ (Term' var)
               | (Term' var) :* (Term' var) deriving (Eq, Ord, Show)


type Term = Term' Int


data Dir = L | R | S deriving (Show)
type Path = [Dir]

type VarMap = M.Map Int Double



paths :: Term -> [Path]
paths (X _) = [[]]
paths (Const _) = [[]]
paths (Minus t) = [[]] ++ map (S:) (paths t)
paths (s :+ t) = [[]] ++ map (L:) (paths s) ++ map (R:) (paths t)
paths (s :* t) = [[]] ++ map (L:) (paths s) ++ map (R:) (paths t)

subterm :: Term -> Path -> Term
subterm (Minus t) (S:ps) = subterm t ps
subterm (s :+ _) (L:ps) = subterm s ps
subterm (_ :+ t) (R:ps) = subterm t ps
subterm (s :* _) (L:ps) = subterm s ps
subterm (_ :* t) (R:ps) = subterm t ps
subterm t _ = t


replaceSubterm :: Term -> Path -> Term -> Term
replaceSubterm (Minus t) (S:ps) q = Minus (replaceSubterm t ps q)
replaceSubterm (s :+ t) (L:ps) q = (replaceSubterm s ps q) :+ t
replaceSubterm (s :+ t) (R:ps) q = s :+ (replaceSubterm t ps q)
replaceSubterm (s :* t) (L:ps) q = (replaceSubterm s ps q) :* t
replaceSubterm (s :* t) (R:ps) q = s :* (replaceSubterm t ps q)
replaceSubterm _ _ q = q


interpret :: Term -> VarMap -> Double
interpret t m = interpret' t
  where interpret' (X v) = m M.! v
        interpret' (Const c) = c
        interpret' (Minus q) = - (interpret' q)
        interpret' (s :+ q) = interpret' s + interpret' q
        interpret' (s :* q) = interpret' s * interpret' q

var :: Int
var = 0

mkLinearTerm :: (Double, Double) -> Term
mkLinearTerm (a, m) = (a' :* x') :+ m'
  where a' = if a < 0 then Minus (Const (-a)) else Const a
        m' = if m < 0 then Minus (Const (-m)) else Const m
        x' = X var

mkQuadraticTerm :: (Double, Double, Double) -> Term
mkQuadraticTerm (a, b, m) = (a' :* x' :* x') :+ (b' :* x') :+ m'
  where a' = if a < 0 then Minus (Const (-a)) else Const a
        b' = if b < 0 then Minus (Const (-b)) else Const b
        m' = if m < 0 then Minus (Const (-m)) else Const m
        x' = X var

mkPairs :: [Double] -> [(Double, Double)]
mkPairs [] = []
mkPairs [_] = []
mkPairs (x:y:xs) = (x, y):(mkPairs xs)

mkTripplets :: [Double] -> [(Double, Double, Double)]
mkTripplets (x:y:z:xs) = (x, y, z):(mkTripplets xs)
mkTripplets [_, _] = []
mkTripplets [_] = []
mkTripplets [] = []

mkInitGen :: Int -> IO [Term]
mkInitGen n = do
  lcoeffs <- shuf n
  qcoeffs <- shuf (n + n `div` 2)
  let lts = map mkLinearTerm (mkPairs $ adjust (n `div` 2) lcoeffs)
      qts = map mkQuadraticTerm (mkTripplets $ adjust ((n + n `div` 2) `div` 2) qcoeffs)
      adjust y = map ((/ fromIntegral (y `div` 2)) . fromIntegral . (y -))
      ts = concat $ zipWith f lts qts
      f x y = [x, y]
  return ts


haveSexAndDie :: (Int, Term) -> (Int, Term) -> [Term]
haveSexAndDie (x, t) (y, s) = [t', s']
  where tps = paths t
        tp = tps !! (x `mod` length tps)
        sps = paths s
        sp = sps !! (y `mod` length sps)
        t' = replaceSubterm t tp (subterm s sp)
        s' = replaceSubterm s sp (subterm t tp)


eucdist :: [Int] -> [([Double], Double)] -> Term -> Double
eucdist vs ys t = val
  where (cs, ds) = unzip ys
        envs = map (M.fromList . zip vs) cs
        val = L.foldl' f 0 (zip ds envs)
        f acc (d, env) = acc + (d - (interpret t env))^(2 :: Int)

refs :: [([Double], Double)]
refs = [([-9], -0.7), ([-6.2], 6.2), ([-2.1], -12.9), ([1.0], 0.1), ([2.3], -15.2), ([5.0], 10.1), ([8.9], -0.9)]


instance Genetic Term' Int where
         mkInitGeneration = mkInitGen (2^(10 :: Int))
         crossover = haveSexAndDie
         distance = eucdist [var] refs
