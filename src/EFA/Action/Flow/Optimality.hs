
module EFA.Action.Flow.Optimality where

import qualified EFA.Flow.Topology.Quantity as TopoQty
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

import qualified Data.Maybe as Maybe
--import Control.Applicative (liftA2)
import Control.Monad(join)
--import Data.Foldable (Foldable, foldMap)

--import qualified EFA.Action.DemandAndControl as DemandAndControl

import EFA.Utility(Caller,
                 merror,
               --    (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.Optimality"

nc :: FunctionName -> Caller
nc = genCaller modul

{-
data EndNodeEnergies node v = EndNodeEnergies 
                              {getSinkMap:: (SinkMap node v),
                              getSourceMap :: SourceMap node v,
                              getStorageMap :: StorageMap node (Maybe (TopoQty.Sums v))} deriving Show
-}
data EndNodeEnergies node v = EndNodeEnergies 
                              {getSinkMap:: (SinkMap node v),
                               getSourceMap :: SourceMap node v,
                               getStorageMap :: StorageMap node (Maybe (StorageFlow v))} deriving Show



newtype GenerationEfficiency a = GenerationEfficiency {unGenerationEfficiency::a}  deriving (Show,Eq)
newtype UsageEfficiency a = UsageEfficiency {unUsageEfficiency :: a} deriving  (Show,Eq)

-- | Indicates that values are Sign-Corrected for Storage-Sign-Convention positive == charging, negative == discharging
newtype StorageFlow a = StorageFlow {unStorageFlow :: a} deriving Show 

newtype GlobalLifeCycleMap node a = GlobalLifeCycleMap (Map.Map node (GenerationEfficiency a,UsageEfficiency a)) deriving (Show, Eq)


lookupLifeCycleEtaGlobalLifeCycleEta :: Ord node =>
  GlobalLifeCycleMap node a ->
  node ->
  Maybe (GenerationEfficiency a, UsageEfficiency a)
lookupLifeCycleEtaGlobalLifeCycleEta  (GlobalLifeCycleMap m) node = Map.lookup node m 

 
lookupLifeCycleEta :: 
  (Show node, Ord node, Ord a, Arith.Constant a, Arith.Product a) =>
  Caller -> 
  LifeCycleMap node (Interp.Val a) -> 
  Maybe Idx.AbsoluteState -> 
  node -> 
  (GenerationEfficiency (Interp.Val a), UsageEfficiency (Interp.Val a)) 
lookupLifeCycleEta caller (LifeCycleMap m) (Just state) node = 
  Maybe.fromMaybe err $ join $ fmap (Map.lookup node) $ Map.lookup state m 
  where err = merror caller modul "lookupLifeCycleEta" 
                    ("State or Node not in LifeCycleEfficiencyMap - State: " ++ show state ++ "- Node: " ++ show node)
lookupLifeCycleEta _ _ Nothing _ = (GenerationEfficiency $ Interp.Invalid ["lookupLifeCycleEta"], 
                                                         UsageEfficiency $ Interp.Invalid ["lookupLifeCycleEta"])


lookupUsageEfficiency ::
  (Ord a,
   Ord node,
   Show node,
   Arith.Constant a) =>
  Caller ->
  LifeCycleMap node (Interp.Val a) ->
  Maybe Idx.AbsoluteState ->
  node ->
  Interp.Val a
lookupUsageEfficiency  caller m state node = 
  unUsageEfficiency $ snd $ lookupLifeCycleEta caller m state node

lookupGenerationEfficiency ::
    (Ord a,
     Ord node,
     Show node,
     Arith.Constant a) =>
    Caller ->
    LifeCycleMap node (Interp.Val a) ->
    Maybe Idx.AbsoluteState ->
    node ->
    Interp.Val a
lookupGenerationEfficiency  caller m state node = 
  unGenerationEfficiency $ fst $ lookupLifeCycleEta caller m state node

newtype LifeCycleMap node a = 
  LifeCycleMap (Map.Map (Idx.AbsoluteState) (Map.Map node (GenerationEfficiency a,UsageEfficiency a))) deriving Show
{-
lookupLifeCycleEta :: (Ord node, Ord a, Arith.Constant a, Arith.Product a) =>
  LifeCycleMap node (Interp.Val a) -> 
  Maybe Idx.AbsoluteState -> 
  node -> 
  Maybe (GenerationEfficiency (Interp.Val a), UsageEfficiency (Interp.Val a)) 
lookupLifeCycleEta (LifeCycleMap m) (Just state) node = fmap g $ join $ fmap (Map.lookup node) $ Map.lookup state m 
  where g (GenerationEfficiency x, UsageEfficiency y) = (GenerationEfficiency x, UsageEfficiency y)
lookupLifeCycleEta _ Nothing _ = Just (GenerationEfficiency $ Interp.Invalid ["lookupLifeCycleEta"], 
                                                         UsageEfficiency $ Interp.Invalid ["lookupLifeCycleEta"]) -}
  
newtype SinkMap node a = SinkMap {unSinkMap :: (Map.Map node a)} deriving Show  
newtype SourceMap node a = SourceMap {unSourceMap :: (Map.Map node a)} deriving Show  
newtype StorageMap node a =  StorageMap {unStorageMap :: (Map.Map node a)} deriving Show  

data Eta2Optimise a = EtaSys {unEta2Optimise :: a} deriving (Show,Eq,Ord) -- TODO add | SelectedEta a deriving Show
data Loss2Optimise a = LossSys {unLoss2Optimise :: a} deriving Show -- TODO add | SelectedLoss a deriving Show
newtype TotalBalanceForce a = TotalBalanceForce a deriving Show

data OptimalityValues a = OptimalityValues (OptimalityMeasure a) (TotalBalanceForce a) deriving Show

data OptimalityMeasure a = OptimalityMeasure {getEta :: Eta2Optimise a, 
                                              getLoss:: Loss2Optimise a} deriving Show

-- TODO :: pretty ugly
instance Functor OptimalityValues where
  fmap f (OptimalityValues (OptimalityMeasure(EtaSys e) (LossSys l)) (TotalBalanceForce fo)) = 
             (OptimalityValues (OptimalityMeasure (EtaSys $ f e) (LossSys $ f l)) (TotalBalanceForce $ f fo)) 

-- TODO:: getEtaVal & Co -- mangelhafte Typsicherheit
getEtaVal :: OptimalityValues a -> a             
getEtaVal (OptimalityValues (OptimalityMeasure (EtaSys x) _) _) = x

getLossVal :: OptimalityValues a -> a   
getLossVal (OptimalityValues (OptimalityMeasure _ (LossSys x)) _) = x

getForceVal :: OptimalityValues a -> a 
getForceVal (OptimalityValues _ (TotalBalanceForce x)) = x

getOptEtaVal :: (Arith.Sum a) => OptimalityValues a -> a             
getOptEtaVal (OptimalityValues (OptimalityMeasure (EtaSys x) _) (TotalBalanceForce y)) = x Arith.~+ y

getOptLossVal :: (Arith.Sum a) => OptimalityValues a -> a             
getOptLossVal (OptimalityValues (OptimalityMeasure _ (LossSys x)) (TotalBalanceForce y)) = x Arith.~+ y

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
    f e l fo = OptimalityValues (OptimalityMeasure (EtaSys e) (LossSys l)) (TotalBalanceForce fo)
    eta = Interp.dim1PerState caller inmethod label xPair 
          ((\(a,b) -> (ValueState.map getEtaVal a, ValueState.map getEtaVal b)) yPair) x
    loss = Interp.dim1PerState caller inmethod label xPair 
           ((\(a,b) -> (ValueState.map getLossVal a, ValueState.map getLossVal b)) yPair) x
    force = Interp.dim1PerState caller inmethod label xPair 
            ((\(a,b) -> (ValueState.map getForceVal a, ValueState.map getForceVal b)) yPair) x
   

newtype ScaleSource a = ScaleSource {unScaleSource :: a}
newtype ScaleSink a = ScaleSink {unScaleSink :: a}

data ScaleMap a = ScaleMap (Map.Map Idx.AbsoluteState (ScaleSource a, ScaleSink a))

lookupScaleSink :: Caller -> ScaleMap (Interp.Val a) ->Maybe Idx.AbsoluteState ->  (Interp.Val a)
lookupScaleSink caller (ScaleMap m) state = f state
  where
  f Nothing =  Interp.Invalid ["lookupScaleSource"]               
  f (Just st) = unScaleSink $ snd $ Maybe.fromMaybe err $ Map.lookup st m
  err = merror caller modul "lookupSourceScale" $ "State not in scalemap: " ++ show state

lookupScaleSource ::  Caller -> ScaleMap  (Interp.Val a) ->Maybe Idx.AbsoluteState ->  (Interp.Val a)
lookupScaleSource caller (ScaleMap m) state = f state
  where
  f Nothing =  Interp.Invalid ["lookupScaleSource"]               
  f (Just st) = unScaleSource $ fst $ Maybe.fromMaybe err $ Map.lookup st m  
  err = merror caller modul "lookupSourceScale" $ "State not in scalemap: " ++ show state


-- TODO: move to right place -- use in applyGenerationEfficiency,applyUsageEfficiency
getStoragePowerWithSignNew :: (Arith.Sum v) => Caller ->  Maybe (TopoQty.Sums v) -> Maybe (StorageFlow v)
getStoragePowerWithSignNew caller Nothing =  merror caller modul "getStoragePowerWithSign" "Completely inactive edge" 
getStoragePowerWithSignNew caller (Just sums) = case sums of                 
  TopoQty.Sums Nothing (Just energy) -> Just $ StorageFlow $ Arith.negate energy
  TopoQty.Sums  (Just energy) Nothing -> Just $ StorageFlow  energy
  TopoQty.Sums Nothing Nothing -> Nothing 
  TopoQty.Sums (Just _) (Just _) -> 
    merror caller modul "getStoragePowerWithSign" "Inconsistent energy flow - both directions active"