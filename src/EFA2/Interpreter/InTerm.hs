{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}


module EFA2.Interpreter.InTerm where


import Data.Maybe

-- import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env
import EFA2.Signal.Base
import EFA2.Signal.Signal
import EFA2.Signal.Data



data InTerm a = EIdx EnergyIdx
              | DEIdx DEnergyIdx
              | PIdx PowerIdx
              | DPIdx DPowerIdx
              | FNIdx FEtaIdx
              | DNIdx DEtaIdx
              | ScaleIdx XIdx
              | DTIdx DTimeIdx
              | VIdx VarIdx
              | SIdx StorageIdx
              | InConst Val
              | InGiven a
              | InFunc (a -> a)
              | InFEdge (InTerm a) (InTerm a)
              | InBEdge (InTerm a) (InTerm a)
              | InNEdge (InTerm a) (InTerm a)
              | InMinus (InTerm a)
              | InRecip (InTerm a)
              | InAdd (InTerm a) (InTerm a)
              | InMult (InTerm a) (InTerm a)
              | InEqual (InTerm a) (InTerm a) deriving (Eq, Ord, Show)


-- What for?
instance (Show a, Eq a) => Num (InTerm a) where
         (+) = InAdd
         (*) = InMult
         abs = undefined
         signum = undefined
         fromInteger = undefined
     
         
instance BProd (InTerm a) (InTerm a) (InTerm a) where
         (..*) = InMult
         x ../ y = InMult x (InRecip y)

instance BSum (InTerm a) (InTerm a) (InTerm a) where
         (..+) = InAdd
         x ..- y = InAdd x (InMinus y)
 
instance DArith0 (InTerm a) where
         neg = InMinus
         rec = InRecip

instance SConst Scalar (Data Nil) (InTerm a) where   
         toConst _ x = undefined --toScalar x 


toAbsEq :: InTerm a -> InTerm a
toAbsEq (InFEdge p n) = InMult p n
toAbsEq (InBEdge p n) = InMult p (InRecip n)
toAbsEq (InNEdge p0 p1) = InMult p0 (InRecip p1)
toAbsEq (InMinus x) = InMinus (toAbsEq x)
toAbsEq (InRecip x) = InRecip (toAbsEq x)
toAbsEq (InAdd x y) = InAdd (toAbsEq x) (toAbsEq y)
toAbsEq (InMult x y) = InMult (toAbsEq x) (toAbsEq y)
toAbsEq (InEqual x y) = InEqual (toAbsEq x) (toAbsEq y)
toAbsEq t = t

toAbsEquations :: [InTerm a] -> [InTerm a]
toAbsEquations ts = map toAbsEq ts



mkDiffEq :: Int -> InTerm a -> Maybe (InTerm a)
mkDiffEq rec (InEqual q@(PIdx (PowerIdx s'' _ f'' t'')) 
                      (InFEdge p@(PIdx (PowerIdx s _ f t)) n@(FNIdx (FEtaIdx s' _ f' t')))) = Just res
  where res = InEqual dq (InAdd (InAdd (InMult dp n) (InMult p dn)) (InMult dp dn))
        dq = DPIdx (DPowerIdx s'' rec f'' t'')
        dn = DNIdx (DEtaIdx s' rec f' t')
        dp = DPIdx (DPowerIdx s rec f t)
mkDiffEq rec (InEqual q@(PIdx (PowerIdx s'' _ f'' t'')) 
                      (InBEdge p@(PIdx (PowerIdx s _ f t)) n@(FNIdx (FEtaIdx s' _ f' t')))) = Just res
  where res = InEqual dq (InAdd (InAdd (InMult dp (InRecip n)) (InMinus (InMult (InMult p dn) nom))) 
                                (InMinus (InMult (InMult dp dn) nom)))
        dq = DPIdx (DPowerIdx s'' rec f'' t'')
        nom = InRecip (InAdd (InMult dn n) (InMult n n))
        dn = DNIdx (DEtaIdx s' rec f' t')
        dp = DPIdx (DPowerIdx s rec f t)
mkDiffEq _ (InEqual _ (InNEdge _ _)) = error "mkDiffEq: eta cannot be computed with Differenzenrechnung"
mkDiffEq _ _ = Nothing

mkDiffEquations :: Int -> [InTerm a] -> [InTerm a]
mkDiffEquations rec ts = catMaybes (map (mkDiffEq rec) ts)

