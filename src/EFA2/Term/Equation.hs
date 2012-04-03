
module EFA2.Term.Equation where

import Data.Graph.Inductive

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.List.HT as HTL

import Debug.Trace

import EFA2.Utils.Utils
import EFA2.Graph.GraphData
import EFA2.Graph.Graph


data Abs
data Diff


data EqTerm a = EqTerm a := EqTerm a
          | Const Double
          | Energy PowerIdx
          | Eta EtaIdx
          | DEnergy DPowerIdx
          | DEta DEtaIdx
          | X XIdx
          | F (EqTerm a)
          | B (EqTerm a)
          | Given (EqTerm a)
          | Minus (EqTerm a)
          | Recip (EqTerm a)
          | Add (EqTerm a) (EqTerm a)
          | Mult (EqTerm a) (EqTerm a) deriving (Show, Eq, Ord)

infixl 1 :=


mkEta :: Int -> Int -> EqTerm a
mkEta x y = Eta (EtaIdx x y)

mkX :: Int -> Int -> EqTerm a
mkX x y = X (XIdx x y)

mkEnergy :: Int -> Int -> EqTerm a
mkEnergy x y = Energy (PowerIdx x y)


give :: [EqTerm a] -> [EqTerm a]
give ts = map Given ts

isGiven :: EqTerm a -> Bool
isGiven (Given _) = True
isGiven _ = False


showEqTerm :: EqTerm a -> String
showEqTerm (Const x) = show x
showEqTerm (Energy (PowerIdx x y)) = "E_" ++ show x ++ "_" ++ show y
showEqTerm (Eta (EtaIdx x y)) = "n_" ++ show x ++ "_" ++ show y
showEqTerm (DEnergy (DPowerIdx x y)) = "dE_" ++ show x ++ "_" ++ show y
showEqTerm (DEta (DEtaIdx x y)) = "dn_" ++ show x ++ "_" ++ show y
showEqTerm (X (XIdx x y)) =  "x_" ++ show x ++ "_" ++ show y
showEqTerm (Add x y) = "(" ++ showEqTerm x ++ " + " ++ showEqTerm y ++ ")"
showEqTerm (Mult x y) = showEqTerm x ++ " * " ++ showEqTerm y
showEqTerm (F x) = "f(" ++ showEqTerm x ++ ")"
showEqTerm (B x) = "b(" ++ showEqTerm x ++ ")"
showEqTerm (Given x) = "given(" ++ showEqTerm x ++ ")"
showEqTerm (Recip x) = "1/(" ++ showEqTerm x ++ ")"
showEqTerm (Minus x) = "-(" ++ showEqTerm x ++ ")"
showEqTerm (x := y) = showEqTerm x ++ " = " ++ showEqTerm y

showEqTerms :: [EqTerm a] -> String
showEqTerms ts = L.intercalate "\n" $ map showEqTerm ts

mkEdgeEq :: Gr a b -> [EqTerm c]
mkEdgeEq g = map f ns
  where ns = edges g
        f (x, y) = (mkEnergy y x) := F (mkEnergy x y)

mkNodeEq :: Gr a b -> [EqTerm c]
mkNodeEq g = concat $ mapGraph mkEq g

{- TODO: Rethink equations ineqs' and oeqs'. Currently they are invalid. What is wrong? -}
mkEq :: ([Node], Node, [Node]) -> [EqTerm a]
mkEq ([], _, _) = []
mkEq (_, _, []) = []
mkEq (ins, n, outs) = ieqs' ++ oeqs' ++ ieqs ++ oeqs
-- mkEq (ins, n, outs) = {- ieqs' ++ oeqs' ++ -} ieqs ++ oeqs
  where ins' = zip (repeat n) ins
        outs' = zip (repeat n) outs
        xis = map (uncurry mkX) ins'
        xos = map (uncurry mkX) outs'
        eis = map (uncurry mkEnergy) ins'
        eos = map (uncurry mkEnergy) outs'
        isum = L.foldl1' Add $ map (uncurry mkEnergy) ins'
        osum = L.foldl1' Add $ map (uncurry mkEnergy) outs'
        ieqs = zipWith3 f eis xis (repeat osum)
        oeqs = zipWith3 f eos xos (repeat isum)
        f x y z = x := Mult y z

        ieqs' | length eis > 1 = [g (head xis) (head eis) (tail eis)]
              | otherwise = []
        oeqs' | length eos > 1 = [g (head xos) (head eos) (tail eos)]
              | otherwise = []

        g x e es = e := (L.foldl1' Add $ map (Mult (Mult x (Recip (Add (Const 1.0) (Minus x))))) es)


mkVarSet :: EqTerm a -> S.Set (EqTerm a)
mkVarSet v@(Energy _) = S.singleton v
mkVarSet (Add x y) = S.union (mkVarSet x) (mkVarSet y)
mkVarSet (Mult x y) = S.union (mkVarSet x) (mkVarSet y)
mkVarSet (F x) = mkVarSet x
mkVarSet (B x) = mkVarSet x
mkVarSet (Given x) = mkVarSet x
mkVarSet (Minus x) = mkVarSet x
mkVarSet (Recip x) = mkVarSet x
mkVarSet (x := y) = S.union (mkVarSet x) (mkVarSet y)
mkVarSet _ = S.empty


-- The following functions transform an equation.

data Dir = L | R deriving (Show, Eq)

type TPath = [Dir]


-- test terms
x1 = Energy (PowerIdx 0 1)
x2 = Energy (PowerIdx 0 2)
x3 = Energy (PowerIdx 0 3)
x4 = Energy (PowerIdx 0 4)
c = Const 1.0

t = Add (Mult x1 x2) c := x4


findVar :: EqTerm a -> EqTerm a -> Maybe TPath
findVar t s | t == s = Just []
findVar t s
  | (Nothing, x) <- h = fmap (R:) x
  | (x, Nothing) <- h = fmap (L:) x
  | otherwise = error $ "error in looking for path to (" ++ show t ++ ") in (" ++ show s ++ ")"
  where h = help t s
        help t (u := v) = (findVar t u, findVar t v)
        help t (Add u v) = (findVar t u, findVar t v)
        help t (Mult u v) = (findVar t u, findVar t v)
        help t (Minus u) = (findVar t u, Nothing)  -- coding: Minus has only left operand.
        help t (Recip u) = (findVar t u, Nothing)  -- coding: Recip has only left operand.
        help t (F u) = (findVar t u, Nothing)      -- etc.
        help t (B u) = (findVar t u, Nothing)
        help _ _ = (Nothing, Nothing)

isolateVar :: EqTerm a -> EqTerm a -> TPath -> EqTerm a
isolateVar s t@(u := v) (L:p) = (s := transform v)
  where transform = isolateVar' u p
isolateVar s t@(u := v) (R:p) = (s := transform u)
  where transform = isolateVar' v p

isolateVar' :: EqTerm a -> TPath -> (EqTerm a -> EqTerm a)
isolateVar' _ [] = id
isolateVar' (Add u v) (L:p) = isolateVar' u p . Add (Minus v)
isolateVar' (Add u v) (R:p) = isolateVar' v p . Add (Minus u)
isolateVar' (Mult u v) (L:p) = isolateVar' u p . Mult (Recip v)
isolateVar' (Mult u v) (R:p) = isolateVar' v p . Mult (Recip u)
isolateVar' (Minus u) (L:p) = isolateVar' u p . Minus
isolateVar' (Recip u) (L:p) = isolateVar' u p . Recip
isolateVar' (F u) (L:p) = isolateVar' u p . B
isolateVar' (B u) (L:p) = isolateVar' u p . F

-- this is the main function for transforming Equations
-- It takes an unknown variable and an equation.
-- The resulting equation has
-- the unknown variable isolated on its left hand side (lhs),
-- such that we can evaluate the rhs in order to calculate
-- the value of the unknown variable.
transformEq :: EqTerm a -> EqTerm a -> EqTerm a
transformEq unknown t
  | Nothing <- fv = t
  | Just p <- fv = isolateVar unknown t p
  where fv = findVar unknown t


