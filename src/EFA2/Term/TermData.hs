{-# LANGUAGE TypeSynonymInstances, ExistentialQuantification, StandaloneDeriving #-}


module EFA2.Term.TermData where

import EFA2.Graph.GraphData
import EFA2.Signal.Arith

data InTerm = PIdx PowerIdx
              | EIdx EtaIdx
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

class Interpreter a where
      interpret :: PowerEnv a -> EtaEnv a -> XEnv a -> InTerm -> a
