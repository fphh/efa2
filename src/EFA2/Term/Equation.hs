
module EFA2.Term.Equation where

import Data.Graph.Inductive

import qualified Data.Set as S
import qualified Data.List as L

import EFA2.Graph.Graph

data EqTerm = EqTerm := EqTerm
          | Eta Int Int
          | Energy Int Int
          | X Int Int
          | F EqTerm
          | B EqTerm
          | Given EqTerm
          | Minus EqTerm
          | Recip EqTerm
          | Add EqTerm EqTerm
          | Mult EqTerm EqTerm deriving (Show, Eq, Ord)

infixl 1 :=

mkEta :: Int -> Int -> EqTerm
mkEta = Eta

mkX :: Int -> Int -> EqTerm
mkX = X

mkEnergy :: Int -> Int -> EqTerm
mkEnergy = Energy

give :: [EqTerm] -> [EqTerm]
give ts = map Given ts

isGiven :: EqTerm -> Bool
isGiven (Given _) = True
isGiven _ = False

sumEdges :: [LEdge ELabel] -> EqTerm
sumEdges es = L.foldl1' Add es'
  where es' = map (\(x, y, _) -> Energy x y) es

toString :: EqTerm -> String
toString (Energy x y) = "E_" ++ show x ++ "_" ++ show y
toString (Eta x y) = "n_" ++ show x ++ "_" ++ show y
toString (X x y) =  "x_" ++ show x ++ "_" ++ show y
toString (Add x y) = "(" ++ toString x ++ " + " ++ toString y ++ ")"
toString (Mult x y) = toString x ++ " * " ++ toString y
toString (F x) = "f(" ++ toString x ++ ")"
toString (B x) = "b(" ++ toString x ++ ")"
toString (Given x) = "given(" ++ toString x ++ ")"
toString (Recip x) = "1/(" ++ toString x ++ ")"
toString (Minus x) = "-(" ++ toString x ++ ")"
toString (x := y) = toString x ++ " = " ++ toString y

termsStr :: [EqTerm] -> String
termsStr ts = L.intercalate "\n" $ map toString ts

mkEdgeEq :: Gr a b -> [EqTerm]
mkEdgeEq g = map f ns
  where ns = labEdges g
        f (x, y, _) = (mkEnergy y x) := F (mkEnergy x y)

mkNodeEq :: Gr NLabel ELabel -> [EqTerm]
mkNodeEq g = concatMap mkEq eqs
  where ns = nodes g
        inns = map (inn g) ns
        outs = map (out g) ns
        eqs = zip3 ns outs inns

mkEq :: (Node, [LEdge ELabel], [LEdge ELabel]) -> [EqTerm]
mkEq (_, [], _) = []
mkEq (_, _, []) = []
mkEq (i, os, is) = map f os
  where sos = sumEdges os
        sis = sumEdges (map reverseEdge is)
        reverseEdge (x, y, l) = (y, x, l)
        f (x, y, _) = (mkEnergy x y) := Mult (X i y) sis


mkVarSet :: EqTerm -> S.Set EqTerm
mkVarSet v@(Energy _ _) = S.singleton v
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

{-
v = Energy 1 2
t = Add (Energy 1 2) (Energy 1 4) := Mult (Energy 1 3) (Energy 1 5)
s = Add (Minus (Energy 1 2)) (Energy 1 4) := Energy 1 3
u = Add (Energy 1 2) (Minus (Energy 1 4)) := F (Energy 1 3)
-}

findVar :: EqTerm -> EqTerm -> Maybe TPath
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

isolateVar :: EqTerm -> EqTerm -> TPath -> EqTerm
isolateVar s t@(u := v) (L:p) = (s := transform v)
  where transform = isolateVar' u p
isolateVar s t@(u := v) (R:p) = (s := transform u)
  where transform = isolateVar' v p

isolateVar' :: EqTerm -> TPath -> (EqTerm -> EqTerm)
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
transformEq :: EqTerm -> EqTerm -> EqTerm
transformEq unknown t
  | Nothing <- fv = t
  | Just p <- fv = isolateVar unknown t p
  where fv = findVar unknown t


