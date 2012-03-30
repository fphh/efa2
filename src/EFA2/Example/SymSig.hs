

module EFA2.Example.SymSig where

import EFA2.Graph.GraphData
import EFA2.Term.TermData
import EFA2.Signal.Arith

symSig :: LRPowerEnv [Val] -> LRPowerEnv [InTerm a]
symSig sigs idx
  | Right xs <- res = Right (map InConst xs)
  | Left str <- res = Left str
  where res = sigs idx
