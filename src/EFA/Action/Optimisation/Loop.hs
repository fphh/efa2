{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE UndecidableInstances #-}     

module EFA.Action.Optimisation.Loop where

import qualified EFA.Action.Flow.Balance as Balance
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.StateFlow.Optimality as StateFlowOpt
--import qualified EFA.Data.Interpolation as Interp

import Debug.Trace (trace)
import qualified EFA.Equation.Arithmetic as Arith
--import qualified Data.Map as Map
import qualified EFA.Utility.List as UtList


--import qualified EFA.Report.FormatValue as FormatValue
--import qualified EFA.Report.Format as Format

import qualified Data.Map as Map
--import Text.Printf (printf, PrintfArg) --,IsChar)
import EFA.Utility(Caller,
                   --merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "EFA.Action.Optimisation.Loop"

nc :: FunctionName -> Caller
nc = genCaller modul

newtype EtaCounter = EtaCounter Int deriving Show
newtype MaxEtaIterations = MaxEtaIterations Int deriving Show

incrementEtaCounter :: EtaCounter -> EtaCounter
incrementEtaCounter (EtaCounter cnt) = EtaCounter $ cnt+1

class Display a where
  disp :: a -> String

instance (Show node, Show a) =>
  Display (Map.Map node a) where
  disp m = show $ Map.toList m 

instance  (Show node, Show a) =>
  Display (Balance.ForcingMap Balance.Absolute (Map.Map node (Balance.SocDrive a))) where
  disp (Balance.ForcingMap m) = "Fo: " ++ disp m

instance   (Show node, Show a) => 
  Display (Balance.ForcingMap Balance.Step (Map.Map node (Balance.SocDrive a))) where
  disp (Balance.ForcingMap m) = "St: " ++ disp m

instance   (Show node, Show a) => 
  Display (Balance.Balance node a) where
  disp (Balance.Balance m) = "Bal: " ++ disp m

instance (Show a) => Display (Balance.BalanceCounter a) where
  disp (Balance.BalanceCounter m) = "Cnt: " ++ disp m

instance Show a => Display (a, a) where
  disp  (x, y) = show x ++ " " ++ show y

instance (Show a, Arith.Constant a) => Display (Balance.SocDrive a) where
  disp x = show $ Balance.getSocDrive x 

instance (Show a,Arith.Constant a) => Display (Maybe (Balance.SocDrive a, a)) where
  disp  (Just (x, y)) = "Fo: " ++ disp x ++ "Bal: " ++ show y
  disp Nothing = "Nothing"

instance 
  (Show a, 
   Display (Maybe (Balance.SocDrive a, a), Maybe (Balance.SocDrive a, a))) => 
  Display (Balance.BestForcingPair a) where
  disp (Balance.BestForcingPair x) = "BP: " ++ disp x

data EtaLoopParams a = 
  EtaLoopParams
  { accessMaxEtaIterations :: MaxEtaIterations, 
    accLifeCycleMethod :: StateFlowOpt.LifeCycleMethod,
    accGlobalEtas :: (FlowOpt.GenerationEfficiency a, FlowOpt.UsageEfficiency a) }
  
  
data BalanceLoopParams node a =  
  BalanceLoopParams 
  {accessMaxIterationsPerStorage :: Balance.MaxIterationsPerStorage , 
   accessMaxIterations :: Balance.MaxIterations,
   accessThreshold :: Balance.Threshold a, 
   accessInitialForcing :: Balance.Forcing node a,
   accessInitialStep :: Balance.ForcingStep node a,   
   accessInitialSto :: node}

data EtaLoopItem node a z = 
  EtaLoopItem {
    accEtaCounter :: EtaCounter, 
    accLifeCycleMap :: FlowOpt.LifeCycleMap node a, 
    accBalLoop :: [BalanceLoopItem node a z]}
  
instance 
  (Display (Maybe (Balance.SocDrive a, a), Maybe (Balance.SocDrive a, a)), 
   Show node, Show a) => Show (EtaLoopItem node a z) where  
  show (EtaLoopItem etaCounter lifeCycleMap balLoop) = "Eta-Loop Counter: " ++ show etaCounter ++ "\n" ++
                                                       show lifeCycleMap ++ "\n" ++ show balLoop 
  
instance 
  (Display (Maybe (Balance.SocDrive a, a), Maybe (Balance.SocDrive a, a)), Show node, Show a) => 
  Show (BalanceLoopItem node a z) where  
  show (BalanceLoopItem cnt node force step balance bestPair _) = 
    disp cnt ++ " | " ++
    "Node: " ++ show node ++" | " ++
    disp force ++" | " ++
    disp step ++ " | " ++
    disp balance ++" | " ++
    disp bestPair ++ "\n"


data BalanceLoopItem node a z = 
  BalanceLoopItem 
  {accBalCounter :: Balance.BalanceCounter node,
   accBalNode :: node,
   accForcing :: Balance.Forcing node a,
   accForcingStep :: Balance.ForcingStep node a, 
   accBalance :: Balance.Balance node a,
   accBestPair :: Balance.BestForcingPair a,
   accResult :: z}

resetBalCounter :: BalanceLoopItem node a z -> BalanceLoopItem node a z
resetBalCounter (BalanceLoopItem cnt n f s l b r) = BalanceLoopItem (g cnt) n f s l b r
 where
   g (Balance.BalanceCounter m) = Balance.BalanceCounter $ Map.map (\_ -> 0) m 

etaLoop :: (Ord node,Ord a, Show a, Show node, Arith.Constant a) =>
  Caller ->
  [node] ->
  EtaLoopParams a ->
  BalanceLoopParams node a ->
  (FlowOpt.LifeCycleMap node a -> Balance.Forcing node a -> z) ->
  (z -> Balance.Balance node a) ->
  FlowOpt.LifeCycleMap node a ->
  (z -> FlowOpt.LifeCycleMap node a -> FlowOpt.LifeCycleMap node a)->
  [EtaLoopItem node a z]
etaLoop caller storages etaParams balParams systemFunction getBalance initialLifeCycleMap updateLifeCycleMap = 
   UtList.takeUntil check $ 
   go (EtaLoopItem (EtaCounter 0) initialLifeCycleMap [BalanceLoopItem initialCount initialSto initialForcing 
                                                        initialStep initialBlance initialBestPair initialResult] )
  where
    check (EtaLoopItem (EtaCounter count) _ _) = count > maxEtaIterations 
    (MaxEtaIterations maxEtaIterations) = accessMaxEtaIterations etaParams
    initialStep = accessInitialStep balParams
    initialForcing = accessInitialForcing balParams
    initialSto = accessInitialSto balParams
    initialCount = Balance.initialiseBalanceCounter storages
    initialResult = systemFunction initialLifeCycleMap initialForcing
    initialBlance = getBalance initialResult
    initialBestPair = Balance.emptyBestForcingPair
      
    go lastItem@(EtaLoopItem lastCount lastLifeCycleMap lastBalLoop) = [lastItem] ++ go (EtaLoopItem count lifeCycleMap balLoop)
      where
        count = incrementEtaCounter lastCount
        lifeCycleMap = updateLifeCycleMap (accResult $ last balLoop) lastLifeCycleMap
        -- TODO :: check if last is OK
        balLoop = balanceLoop (caller |> nc "etaLoop") balParams (systemFunction lastLifeCycleMap) getBalance (last lastBalLoop)

f str = id 
-- f str x = trace (str ++": " ++ show x) x

balanceLoop :: 
  (Ord a, Ord node, Show node, Arith.Constant a,Show a) =>
  Caller ->                               
  BalanceLoopParams node a ->
  (Balance.Forcing node a -> z) ->
  (z -> Balance.Balance node a) ->
  BalanceLoopItem node a z ->
  [BalanceLoopItem node a z]
balanceLoop caller balParams systemFunction getBalance lastBalItem = UtList.takeUntil check $ 
  go $ resetBalCounter lastBalItem
  where
    check (BalanceLoopItem cnt _ _ _ bal _ _) = 
      (Balance.checkBalance threshold bal) || 
      Balance.checkBalanceCounter cnt maxIterations
    threshold = accessThreshold balParams
    maxIterations = accessMaxIterations balParams   
    maxIterationsPerStorage = accessMaxIterationsPerStorage balParams
    go lastItem@(BalanceLoopItem lastCount lastSto lastForcing step lastBalance lastBestPair _) = [lastItem] ++ go 
      (BalanceLoopItem 
      (f "count" count) 
      (f "sto" sto) 
      (f "forcing" forcing)
      (f "nextStep " nextStep) 
      (f "balance" balance)
      (f "bestPair" bestPair) 
      (trace "result" result))
      where 
        sto = Balance.selectStorageToForce (caller |> nc "balanceOneStorageLoop") 
               lastBalance threshold lastCount maxIterationsPerStorage lastSto 
        forcing = Balance.addForcingStep (caller |> nc "balanceOneStorageLoop") lastForcing step sto
        result = systemFunction forcing
        balance = getBalance result
        nextStep = Balance.calculateNextBalanceStep caller balance bestPair step sto
        count = Balance.incrementBalanceCounter lastCount sto
        bestPair = Balance.rememberBestBalanceForcing (caller |> nc "balanceOneStorageLoop") lastBestPair (forcing, balance) sto
        
        

getLastResult :: [EtaLoopItem node a z] -> z
getLastResult = accResult . last . accBalLoop . last