module EFA.Action.Optimisation.Sweep where

import EFA.Utility(Caller,
                  -- merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
  
import qualified EFA.Data.OrdData as OrdData

-- TODO: Modul so verallgemeinern, dass mit verschiedenen Datentypen gesweept werden kann
modul :: ModuleName
modul = ModuleName "DoubleSweep"

nc :: FunctionName -> Caller
nc = genCaller modul


data Dmnd a
data Srch a

type Demand inst = OrdData.Edge (Dmnd inst)
type Search inst = OrdData.Edge (Srch inst)
