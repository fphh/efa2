{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

module EFA2.Interpreter.Eta where

import Data.Maybe
import qualified Data.Map as M

import qualified Data.List as L
import Data.Ord
import Data.Ratio

import EFA2.Interpreter.Arith
import EFA2.Utils.Utils
import EFA2.Signal.Signal

import Debug.Trace


type VariableEta a = M.Map a (a -> a)

data Pt a = Pt { xPt :: a, etaYPt :: a } deriving (Show, Eq)

{-
class MakePoint cont a where
      makePoint :: cont a -> cont a -> cont Pt

instance MakePoint Container Val where
         makePoint = zipWith Pt
-}

--mkVariableEta :: (Arith a, Ord a, Show a) => [Pt a] -> VariableEta a
mkVariableEta ps = M.fromList $ zip lst (map (uncurry func) pps)
  where lst = map xPt ps
        pps = pairs ps

-- | Linear interpolation between two points.
--func :: (Arith a, Show a) =>  Pt a -> Pt a -> a -> a
func a@(Pt x1 y1) b@(Pt x2 y2) x = m .* x .+ t
  where m = (y2 .- y1)./(x2 .- x1)
        t = y1 .- (m .* x1)
        str = show m ++ " * " ++ show x ++ " + " ++ show t

-- TODO: How should we extrapolate?
--getValue :: (Arith a, Ord a, Show a) => VariableEta a -> a -> a
getValue m x
  | (Just (_, f), Just _) <- (bot, top) = f x
  | (_, Just (_, g)) <- (bot, top) = error "getValue: tried to extrapolate eta to the left!"
  | (Just (_, f), _) <- (bot, top) = error "getValue: tried to extrapolate eta to the right!"
  | otherwise = error "getValue: empty map?"
  where (m1, m2) = M.split x m
        bot = if (M.null m1) then Nothing else Just $ M.findMax m1
        top = if (M.null m2) then Nothing else Just $ M.findMin m2

-- | Main function of this module.
-- Given a list of points, it returns an interpolation function.
-- See above for issues of extrapolation.

{-
class MkEtaFunc a where
      mkEtaFunc :: Pt a -> (a -> a)

instance MkEtaFunc [a] where
         mkEtaFunc = mkEtaFunc'
-}

--mkEtaFunc' :: (Arith a, Ord a, Show a) => Pt [a] -> ([a] -> [a])
mkEtaFunc (Pt xs ys) = trace ("\n-------------------\n" ++ prettyPts pts ++ "-----------\n") $  map (getValue ve)
  where pts = zipWith Pt xs ys
        pts' = L.sortBy (comparing xPt) pts
        ve = mkVariableEta pts'
        f2 = ve M.! (cst 3)

{-
getValue ve
  where ps' = L.sortBy (comparing xPt) ps
        ve = mkVariableEta ps'
-}
{-
-- TODO: What big number is appropriate?
reverseEta :: (Arith a, Eq a) => [Pt a] -> [Pt a]
reverseEta ps = map f ps
  where f (Pt x n) = Pt (n .* x) (if n == cst 0 then bigNumber else cst 1 ./ n)
        bigNumber = cst 10000000
-}

--reversePts :: (Arith a, Eq a) => Pt [a] -> Pt [a]
reversePts (Pt x n) = Pt (n .* x) (if n == cst 0 then error "reversePts: inverse of 0 eta unknown!" else cst 1 ./ n)


-- | Kurve mit Stuetzstellen
-- Je enger die Stuetzstellen, desto genauer wird die Umkehrfunktion
--eta :: Pt [Val]
eta = Pt (take n lst) (take n vs)
  where n = 50
        lst = map cst [1..]
        vs = map (uncurry (./)) $ pairs lst

{-
eta2 :: (Arith a) => [Pt a]
eta2 = (Pt (cst 0) (cst 0)):(take 500 $ zipWith Pt lst vs)
  where lst = map cst [1, 1.1 ..]
        vs = map (uncurry (./)) $ pairs lst

eta3 :: (Arith a) => [Pt a]
eta3 = (Pt (cst 0) (cst 0)):(take 500 $ zipWith Pt lst (map cst vs))
  where lst = map cst [1, 1.1 ..]
        vs = [0.3, 0.4, 0.5, 0.4] ++ vs
-}

prettyEta :: (Show a) => [a] -> ([a] -> [a]) -> String
prettyEta lst etaf = L.intercalate "\n" $ zipWith f lst $ etaf lst
  where f x y = show x ++ " " ++ show y

prettyPts :: (Show a) => [Pt a] -> String
prettyPts pts = L.intercalate "\n" (map f pts)
  where f (Pt x y) = show x ++ " " ++ show y

prettyPt :: (Show a) => Pt [a] -> String
prettyPt (Pt xs ys) = L.intercalate "\n" (zipWith f xs ys)
  where f x y = show x ++ " " ++ show y

{-
f :: [Val] -> [Val]
f = mkEtaFunc eta

g :: [Val] -> [Val]
g = mkEtaFunc (reversePts eta)
-}

--check :: (Arith a, Ord a, Num a, Eq a) => Pt [a] -> a -> [a] -> Bool
check pts eps x = trace str $ all (< eps) $ map absol (x .- y)
  where x' = x .* f x
        y = x' .* g x'
        f = mkEtaFunc pts
        g = mkEtaFunc (reversePts pts)
        str = concat $ zipWith h x y
        h x y = show x ++ " == " ++ show y ++ "\n"

{-
checkRev :: (Arith a, Ord a, Num a, Eq a) => [Pt a] -> a -> a -> Bool
checkRev pts eps x' = absol (x' .- y) < eps
  where x = x' .* g x'
        y = x .* f x
        f = mkEtaFunc pts
        g = mkEtaFunc (reverseEta pts)
-}

-- | This test is better with Ratio, because of numeric issues.
--reverseTest :: (Arith a, Eq a) => Pt [a] -> Bool
reverseTest pts = reversePts (reversePts pts) == pts
