{-# LANGUAGE TypeSynonymInstances #-}

module EFA2.Example.SymSig where

import Data.Graph.Inductive

import EFA2.Graph.GraphData
import EFA2.Term.TermData
import EFA2.Signal.Arith

symSig :: LRPowerEnv [Val] -> LRPowerEnv [InTerm]
symSig sigs idx
  | Right xs <- res = Right (map InConst xs)
  | Left str <- res = Left str
  where res = sigs idx


class Signal a where
      signal :: LRPowerEnv [Val] -> LRPowerEnv [a]
      toSignal :: Val -> a

instance Signal Val where
         signal = id
         toSignal = id

instance Signal InTerm where
         signal sigs = symSig sigs
         toSignal = InConst

data TheGraph a = TheGraph { theGraph :: Gr NLabel ELabel,
                             powerEnv :: LRPowerEnv a }