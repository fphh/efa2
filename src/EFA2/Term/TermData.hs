{-# LANGUAGE TypeSynonymInstances, ExistentialQuantification, StandaloneDeriving #-}


module EFA2.Term.TermData where

import EFA2.Graph.GraphData
import EFA2.Signal.Arith

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


{-
type LRNodeEnv a = NodeIdx -> IdxErrorMonad a
type LREtaEnv a = EtaIdx -> IdxErrorMonad a
type LRPowerEnv a = PowerIdx -> IdxErrorMonad a
type LRDEtaEnv a = DEtaIdx -> IdxErrorMonad a
type LRDPowerEnv a = DPowerIdx -> IdxErrorMonad a
type LRXEnv a = XIdx -> IdxErrorMonad a

type NodeEnv a = NodeIdx -> a
type EtaEnv a = EtaIdx -> a
type PowerEnv a = PowerIdx -> a
type DEtaEnv a = EtaIdx -> a
type DPowerEnv a = PowerIdx -> a
type XEnv a = XIdx -> a
-}

{-
class Interpreter env where
      interpret :: env -> InTerm -> a
-}