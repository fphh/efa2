{-# LANGUAGE FlexibleInstances #-}


module EFA2.Term.Horn where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import Data.Graph.Inductive

import Debug.Trace


import EFA2.Term.Term

data Formula = Zero
             | One
             | Atom Int
             | Formula :* Formula
             | Formula :-> Formula deriving (Ord, Eq)

infix 8 :->
infix 9 :*

instance Show Formula where
         show Zero = "F"
         show One = "T"
         show (Atom x) = show x
         show (f :* g) = "(" ++ show f ++ " ∧ " ++ show g ++ ")"
         show (f :-> g) = show f ++ " → " ++ show g

type Step = Int

hornsToStr :: [Formula] -> String
hornsToStr fs = L.intercalate " ∧ " $ map (("(" ++) . (++ ")") . show) fs


isAtom :: Formula -> Bool
isAtom (Atom _) = True
isAtom _ = False

getAtoms :: Formula -> S.Set Formula
getAtoms v@(Atom _) = S.singleton v
getAtoms (f :* g) = S.union (getAtoms f) (getAtoms g)
getAtoms (f :-> g) = S.union (getAtoms f) (getAtoms g)
getAtoms _ = S.empty

leftMarked :: S.Set Formula -> Formula -> Bool
leftMarked _ (One :-> _) = True
leftMarked vs (lhs :-> _) | S.size (S.difference (getAtoms lhs) vs) == 0 = True
leftMarked _ _ = False

rightMarked :: S.Set Formula -> Formula -> Bool
rightMarked vs (_ :-> v) | S.member v vs = True
rightMarked _ _ = False

step :: Step -> S.Set (Step, Formula) -> [Formula] -> (S.Set (Step, Formula), [Formula])
step i vs fs = (unionVs, filter (not . rightMarked onlyVars') bs)
  where (as, bs) = L.partition (leftMarked onlyVars) fs
        vs' = S.fromList $ zip (repeat i) (map (\(_ :-> v) -> v) as)
        unionVs = S.union vs' vs
        onlyVars = S.map snd vs
        onlyVars' = S.map snd unionVs

horn' :: Step -> S.Set (Step, Formula) -> [Formula] -> Maybe (S.Set (Step, Formula))
horn' i vs fs
  | noZero = if (vs == vs') then Just vs else horn' (i+1) vs' fs'
  | otherwise = Nothing             
  where (vs', fs') = step i vs fs
        noZero = all ((Zero /=) . snd) (S.toList vs)


horn :: [Formula] -> Maybe (S.Set (Step, Formula))
horn fs = fmap atomsOnly res
  where atomsOnly = S.filter (isAtom . snd)
        res = horn' 0 S.empty fs

makeAnd :: [Formula] -> Maybe Formula
makeAnd [] = Nothing
makeAnd fs = Just (L.foldl1' (:*) fs)

graphToHorn :: Gr Term () -> [Formula]
graphToHorn g = ufold foldFunc [] g

foldFunc :: Context a b -> [Formula] -> [Formula]
foldFunc (ins, x, _, outs) acc = res
  where v = Atom x
        outs' = map snd outs
        outFs = map ((v :->) . Atom) outs'
        insFs = map ((:-> v) . Atom) (map snd ins)
        res = case makeAnd (map Atom outs') of
                   Nothing -> outFs ++ acc
                   Just ands -> (ands :-> v):(insFs ++ outFs ++ acc)


makeHornFormulae :: S.Set Term -> Gr Term () -> [Formula]
makeHornFormulae given g = given' ++ graphToHorn g
  where given' = map (\(Energy x _) -> (One :-> Atom x)) (S.toList given)


makeHornOrder :: [Formula] -> Gr Term () -> [Term]
makeHornOrder formulae g = catMaybes ts
  where Just fs = horn formulae
        fs' = map snd (S.toList fs)
        ts = map (lab g . fromAtom) fs'
        fromAtom (Atom x) = x

hornOrder :: S.Set Term -> Gr Term () -> [Term]
hornOrder given g = makeHornOrder given' g
  where given' = makeHornFormulae given g