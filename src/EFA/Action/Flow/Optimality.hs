
module EFA.Action.Flow.Optimality where

--import qualified EFA.Flow.Topology.Quantity as TopoQty
--import qualified EFA.Data.Axis.Strict as Strict
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

data Eta2Optimise a = EtaSys a deriving Show -- TODO add | SelectedEta a deriving Show
data Loss2Optimise a = LossSys a deriving Show -- TODO add | SelectedLoss a deriving Show
newtype TotalBalanceForce a = TotalBalanceForce a deriving Show



data OptimalityValues a = OptimalityValues (Eta2Optimise a,Loss2Optimise a) (TotalBalanceForce a) deriving Show

instance Functor OptimalityValues where
  fmap f (OptimalityValues (EtaSys e,LossSys l) (TotalBalanceForce fo)) = 
             (OptimalityValues (EtaSys $ f e,LossSys $ f l) (TotalBalanceForce $ f fo)) 

-- TODO:: getEtaVal & Co -- mangelhafte Typsicherheit
getEtaVal :: OptimalityValues a -> a             
getEtaVal (OptimalityValues (EtaSys x,_) _) = x

getLossVal :: OptimalityValues a -> a   
getLossVal (OptimalityValues (_,LossSys x) _) = x

getForceVal :: OptimalityValues a -> a 
getForceVal (OptimalityValues _ (TotalBalanceForce x)) = x

getOptEtaVal :: (Arith.Sum a) => OptimalityValues a -> a             
getOptEtaVal (OptimalityValues (EtaSys x,_) (TotalBalanceForce y)) = x Arith.~+ y

getOptLossVal :: (Arith.Sum a) => OptimalityValues a -> a             
getOptLossVal (OptimalityValues (_,LossSys x) (TotalBalanceForce y)) = x Arith.~+ y

interpolateOptimalityPerState :: (Ord a, Show a, Arith.Product a,Arith.Constant a)=> 
 Caller ->
 Interp.Method a ->
 String ->
 (a,a) ->
 (ValueState.Map (OptimalityValues (Interp.Val a)),
  ValueState.Map (OptimalityValues (Interp.Val a))) ->
 a ->
 ValueState.Map (OptimalityValues (Interp.Val a))
interpolateOptimalityPerState caller inmethod label xPair yPair x = ValueState.zipWith3 f eta loss force
  where
    f e l fo = OptimalityValues (EtaSys e,LossSys l) (TotalBalanceForce fo)
    eta = Interp.dim1PerState caller inmethod label xPair 
          ((\(a,b) -> (ValueState.map getEtaVal a, ValueState.map getEtaVal b)) yPair) x
    loss = Interp.dim1PerState caller inmethod label xPair 
           ((\(a,b) -> (ValueState.map getLossVal a, ValueState.map getLossVal b)) yPair) x
    force = Interp.dim1PerState caller inmethod label xPair 
            ((\(a,b) -> (ValueState.map getForceVal a, ValueState.map getForceVal b)) yPair) x
   
