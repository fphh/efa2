module EFA2.Example.SymSig where

-- import Data.Graph.Inductive
-- import qualified Data.Map as M

import EFA2.Interpreter.Env (EnergyMap)
-- import EFA2.Topology.Topology
import EFA2.Topology.TopologyData (FlowTopology)

{-

symSig ::  LRPowerEnv [Val] -> LRPowerEnv [InTerm]
symSig sigs idx
  | Right xs <- res = Right (map InConst xs)
  | Left err <- res = Left (err { getMap = M.map (map InConst) (getMap err) })
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
-}

data TheGraph a = TheGraph { theGraph :: FlowTopology,
                             powerEnv :: EnergyMap a }
