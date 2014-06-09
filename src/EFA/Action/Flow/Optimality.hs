
module EFA.Action.Flow.Optimality where


--import qualified EFA.Flow.Topology.Quantity as TopoQty

--import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Arithmetic as Arith
--import EFA.Equation.Arithmetic ((~+), (~/))
--import EFA.Equation.Result (Result)

--import qualified EFA.Graph as Graph

import qualified Data.Map as Map

--import Control.Applicative (liftA2)

--import Data.Foldable (Foldable, foldMap)
import qualified EFA.Flow.SequenceState.Index as Idx

--import qualified EFA.Report.Format as Format

--import qualified Data.Maybe as Maybe
--import Control.Applicative (liftA2)
import Control.Monad(join)
--import Data.Foldable (Foldable, foldMap)

import EFA.Utility(Caller,
--                 merror,
               --    (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.Optimality"

nc :: FunctionName -> Caller
nc = genCaller modul

newtype GenerationEfficiency a = GenerationEfficiency a deriving Show
newtype UsageEfficiency a = UsageEfficiency a deriving Show

-- | Indicates that values are Sign-Corrected for Storage-Sign-Convention positive == charging, negative == discharging
newtype StorageFlow a = StorageFlow a deriving Show 

newtype LifeCycleMap node a = 
  LifeCycleMap (Map.Map Idx.AbsoluteState (Map.Map node (GenerationEfficiency a,UsageEfficiency a))) deriving Show
  
  
newtype SinkMap node a = SinkMap (Map.Map node a) deriving Show  
newtype SourceMap node a = SourceMap (Map.Map node a) deriving Show  
newtype StorageMap node a =  StorageMap (Map.Map node a) deriving Show  

lookupLifeCycleEta :: (Ord node, Ord a, Arith.Constant a, Arith.Product a) =>
  LifeCycleMap node a -> 
  Idx.AbsoluteState -> 
  node -> 
  Maybe (GenerationEfficiency a, UsageEfficiency a) 
lookupLifeCycleEta (LifeCycleMap m) state node = join $ fmap (Map.lookup node) $ Map.lookup state m 

newtype EtaSys a = EtaSys a deriving Show
newtype LossSys a = LossSys a deriving Show
newtype TotalBalanceForce a = TotalBalanceForce a deriving Show
