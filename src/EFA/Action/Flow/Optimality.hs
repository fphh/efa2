
module EFA.Action.Flow.Optimality where

--import qualified EFA.Flow.Topology.Quantity as TopoQty

--import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Value.State as ValueState
import qualified EFA.Data.Interpolation as Interp
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

--import qualified EFA.Action.DemandAndControl as DemandAndControl

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

lookupLifeCycleEta :: (Ord node, Ord a, Arith.Constant a, Arith.Product a) =>
  LifeCycleMap node a -> 
  Idx.AbsoluteState -> 
  node -> 
  Maybe (GenerationEfficiency a, UsageEfficiency a) 
lookupLifeCycleEta (LifeCycleMap m) state node = join $ fmap (Map.lookup node) $ Map.lookup state m 
  
newtype SinkMap node a = SinkMap (Map.Map node a) deriving Show  
newtype SourceMap node a = SourceMap (Map.Map node a) deriving Show  
newtype StorageMap node a =  StorageMap (Map.Map node a) deriving Show  

-- data EndNodeEnergies node a = EndNodeEnergies (SinkMap node a) (SourceMap node a) (StorageMap node (Maybe (TopoQty.Sums a)))

data Eta2Optimise a = EtaSys a deriving Show -- TODO add | SelectedEta a deriving Show
data Loss2Optimise a = LossSys a deriving Show -- TODO add | SelectedLoss a deriving Show
newtype TotalBalanceForce a = TotalBalanceForce a deriving Show

data OptimalityValues a = OptimalityValues (Eta2Optimise a,Loss2Optimise a) (TotalBalanceForce a) deriving Show

-- data Essence node a = Essence (OptimalityValues a) (StorageMap node a) (DemandAndControl.ControlMap node a)

interpolateOptimality :: (Ord a, Show a, Arith.Product a)=> 
 Caller ->
 Interp.Method a ->
 Interp.ExtrapMethod a ->
 String ->
 (a,a) ->
 (Interp.Val a) ->
 OptimalityValues a ->
 OptimalityValues a ->
 (OptimalityValues (Interp.Val a))
interpolateOptimality caller inmethod exmethod label (x0,x1) x
  (OptimalityValues (EtaSys eta0,LossSys loss0) (TotalBalanceForce force0))
  (OptimalityValues (EtaSys eta1,LossSys loss1) (TotalBalanceForce force1)) = 
  OptimalityValues (EtaSys eta,LossSys loss) (TotalBalanceForce force)
  where
    eta = Interp.dim1 caller inmethod exmethod label (x0,x1) (eta0,eta1) x
    loss = Interp.dim1 caller inmethod exmethod label (x0,x1) (loss0,loss1) x
    force = Interp.dim1 caller inmethod exmethod label (x0,x1) (force0,force1) x
    
interpolateOptimalityPerState ::  
  (Ord a, Show a, Arith.Product a) =>   
 Caller ->
 Interp.Method a ->
 Interp.ExtrapMethod a ->
 String ->
 (a,a) ->
 (ValueState.Map (OptimalityValues a),ValueState.Map (OptimalityValues a)) ->
 (Interp.Val a) ->
 (ValueState.Map (OptimalityValues (Interp.Val a)))  
interpolateOptimalityPerState caller inmethod exmethod label (x0,x1) (m0,m1) x = 
  ValueState.zipWith (interpolateOptimality caller inmethod exmethod label (x0,x1) x) m0 m1
  