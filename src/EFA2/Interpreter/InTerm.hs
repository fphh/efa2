{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module EFA2.Interpreter.InTerm where

import EFA2.Interpreter.Env
import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Base as B
import EFA2.Signal.Signal (Scalar)
import EFA2.Signal.Data (Data, Nil)
import EFA2.Signal.Base (Val)

import Data.Maybe (mapMaybe)



data InTerm a = EIdx EnergyIdx
              | DEIdx DEnergyIdx
              | PIdx PowerIdx
              | DPIdx DPowerIdx
              | FNIdx FEtaIdx
              | DNIdx DEtaIdx
              | ScaleIdx XIdx
              | DScaleIdx DXIdx

              | DTIdx DTimeIdx
              | VIdx VarIdx
              | SIdx StorageIdx
              | InConst Val
              | InGiven a
              | InFunc (a -> a)

              | InFEdge (InTerm a) (InTerm a)
              | InBEdge (InTerm a) (InTerm a)
              | InNEdge (InTerm a) (InTerm a)

              | InFNode (InTerm a) (InTerm a)
              | InBNode (InTerm a) (InTerm a)
              | InXNode (InTerm a) (InTerm a)


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


instance B.BProd (InTerm a) (InTerm a) where
         (..*) = InMult
         x ../ y = InMult x (InRecip y)

instance B.BSum (InTerm a) where
         (..+) = InAdd
         x ..- y = InAdd x (InMinus y)

instance B.DArith0 (InTerm a) where
         neg = InMinus
         rec = InRecip

instance S.Const Scalar (Data Nil) where
         toConst _ _x = undefined --toScalar x


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
mkDiffEq rec (InEqual (PIdx (PowerIdx s'' _ f'' t''))
                      (InFEdge p@(PIdx (PowerIdx s _ f t)) n@(FNIdx (FEtaIdx s' _ f' t')))) = Just res
  where res = InEqual dq (InAdd (InAdd (InMult dp n) (InMult p dn)) (InMult dp dn))
        dq = DPIdx (DPowerIdx s'' rec f'' t'')
        dn = DNIdx (DEtaIdx s' rec f' t')
        dp = DPIdx (DPowerIdx s rec f t)
mkDiffEq rec (InEqual (PIdx (PowerIdx s'' _ f'' t''))
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
mkDiffEquations rec ts = mapMaybe (mkDiffEq rec) ts
