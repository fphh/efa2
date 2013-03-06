module EFA.Hack.Env where


import qualified EFA.Utility as Ut
import qualified Data.Map as M
import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.Result as Result
import qualified EFA.Graph.Topology.Index as Idx

lookupStack :: (Ord node, Show node) =>
                              Env.Env node (Env.Delta (Result.Result t))
                              -> Idx.Energy node -> t
lookupStack env key = case M.lookup key (Env.energyMap env) of
    Nothing -> error ("Key not in Map - available Keys" ++ Ut.myShowList (M.keys (Env.energyMap env)))
    Just d -> 
      case Env.delta d of
        Result.Undetermined -> error ("undetermined Value in Map for key " ++ show key)
        Result.Determined x -> x
 