{-# LANGUAGE TypeSynonymInstances, ExistentialQuantification, StandaloneDeriving #-}


module EFA2.Solver.TermData where

import EFA2.Signal.Arith
import EFA2.Solver.Env

data InTerm = PIdx PowerIdx
              | EIdx EtaIdx
              | DPIdx DPowerIdx
              | DEIdx DEtaIdx
              | ScaleIdx XIdx
              | InConst Val
              | InRConst Val  
              | InMinus InTerm
              | InRecip InTerm
              | InAdd InTerm InTerm
              | InMult InTerm InTerm
              | InEqual InTerm InTerm deriving (Eq, Ord, Show)


instance Arith InTerm where
         zero = InConst 0.0
         cst = InConst
         neg = InMinus
         rec = InRecip
         (.+) = InAdd
         (.*) = InMult
         x ./ y = InMult x (InRecip y)


mkSymPowerEnv :: PowerEnv [InTerm]
mkSymPowerEnv idx = repeat (PIdx idx)

mkSymEtaEnv :: EtaEnv [InTerm]
mkSymEtaEnv idx = repeat (EIdx idx)

mkSymDPowerEnv :: DPowerEnv [InTerm]
mkSymDPowerEnv idx = repeat (DPIdx idx)

mkSymDEtaEnv :: DEtaEnv [InTerm]
mkSymDEtaEnv idx = repeat (DEIdx idx)

mkSymXEnv :: XEnv [InTerm]
mkSymXEnv idx = repeat (ScaleIdx idx)
