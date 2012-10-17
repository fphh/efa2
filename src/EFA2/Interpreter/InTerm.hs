{-# LANGUAGE MultiParamTypeClasses #-}

module EFA2.Interpreter.InTerm where

import EFA2.Interpreter.Env as Env
import EFA2.Solver.Equation (EqTerm)
-- import qualified EFA2.Signal.Base as B

-- import Data.Maybe (mapMaybe)


data InEquation a =
        InEqual Env.Index (InRhs a) deriving (Eq, Ord, Show)

data InRhs a =
          InTerm EqTerm
        | InGiven a
        | InFunc (a -> a)
            deriving (Eq, Ord, Show)


{- What for?
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
-}


{-
mkDiffEq :: Int -> InEquation a -> Maybe (InEquation a)

mkDiffEq rec (InEqual (Power (PowerIdx s'' _ f'' t''))
                      (InFEdge p@(InIndex (Power (PowerIdx s _ f t)))
                               n@(InIndex (FEta (FEtaIdx s' _ f' t'))))) = Just res
  where res = InEqual dq (InAdd (InAdd (InMult dp n) (InMult p dn)) (InMult dp dn))
        dq = DPower $ DPowerIdx s'' rec f'' t''
        dn = InIndex $ DEta $ DEtaIdx s' rec f' t'
        dp = InIndex $ DPower $ DPowerIdx s rec f t

mkDiffEq rec (InEqual (Power (PowerIdx s'' _ f'' t''))
                      (InBEdge p@(InIndex (Power (PowerIdx s _ f t)))
                               n@(InIndex (FEta (FEtaIdx s' _ f' t'))))) = Just res
  where res = InEqual dq (InAdd (InAdd (InMult dp (InRecip n)) (InMinus (InMult (InMult p dn) nom)))
                                (InMinus (InMult (InMult dp dn) nom)))
        dq = DPower $ DPowerIdx s'' rec f'' t''
        nom = InRecip (InAdd (InMult dn n) (InMult n n))
        dn = InIndex $ DEta $ DEtaIdx s' rec f' t'
        dp = InIndex $ DPower $ DPowerIdx s rec f t

mkDiffEq _ (InEqual _ (InNEdge _ _)) = error "mkDiffEq: eta cannot be computed with Differenzenrechnung"
mkDiffEq _ _ = Nothing

mkDiffEquations :: Int -> [InEquation a] -> [InEquation a]
mkDiffEquations rec ts = mapMaybe (mkDiffEq rec) ts
-}
