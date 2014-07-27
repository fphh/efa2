{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE UndecidableInstances #-}     

module EFA.Action.Optimisation.Loop where

import qualified EFA.Action.Flow.Balance as Balance
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.StateFlow.Optimality as StateFlowOpt
import qualified EFA.Data.Interpolation as Interp

import Debug.Trace (trace)
import qualified EFA.Equation.Arithmetic as Arith
--import qualified Data.Map as Map
import qualified EFA.Utility.List as UtList


import qualified EFA.Report.FormatValue as FormatValue
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
  EtaLoopItem EtaCounter (FlowOpt.LifeCycleMap node a) [BalanceLoopItem node a z]
  
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
        lastResult = accResult $ head lastBalLoop
        lifeCycleMap = updateLifeCycleMap lastResult lastLifeCycleMap
        balLoop = balanceLoop (caller |> nc "etaLoop") balParams (systemFunction lifeCycleMap) getBalance (head lastBalLoop)

-- f str x = trace (str ++": " ++ show x) x
f str = id

balanceLoop :: 
  (Ord a, Ord node, Show node, Arith.Constant a,Show a) =>
  Caller ->                               
  BalanceLoopParams node a ->
  (Balance.Forcing node a -> z) ->
  (z -> Balance.Balance node a) ->
  BalanceLoopItem node a z ->
  [BalanceLoopItem node a z]
balanceLoop caller balParams systemFunction getBalance lastBalItem = UtList.takeUntil check $ 
  go lastBalItem
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
        
        

{-
balanceOneStorageLoopM :: 
  (Ord a, Ord node, Show node, Arith.Constant a,Show a) =>
  Caller ->                               
  BalanceLoopParams node a ->
  (Balance.Forcing node a -> z) ->
  (z -> Balance.Balance node a) ->
  Balance.BalanceCounter node ->
  (Balance.Forcing node a) ->
  (Balance.ForcingStep node a) -> 
  node ->
  [BalanceLoopItem node a z]
balanceOneStorageLoopM caller params systemFunction getBalance initialCount initialForcing initialStep sto = do
  let  
    intialResult = systemFunction initialForcing
    initialBalance = getBalance intialResult
    check (BalanceLoopItem cnt _ _ bal _ _ ) = 
      (Balance.checkBalance caller threshold bal sto) || 
      Balance.checkBalanceCounter cnt maxIterations
    threshold = accessThreshold params
    maxIterations = accessMaxIterationsPerStorage params   
    go (BalanceLoopItem lastCount lastForcing step lastBalance lastBestPair _) = do 
      let 
--        (BalanceLoopItem lastCount lastForcing step lastBalance lastBestPair _) = last xs
        forcing = Balance.addForcingStep (caller |> nc "balanceOneStorageLoopIO") lastForcing step sto
        result = systemFunction forcing
        balance = getBalance result
        nextStep = Balance.calculateNextBalanceStep caller balance bestPair step sto
        count = incrementBalanceCounter lastCount sto
        bestPair = Balance.rememberBestBalanceForcing (caller |> nc "balanceOneStorageLoopIO") lastBestPair (forcing, balance) sto
      return $ BalanceLoopItem count forcing nextStep balance bestPair result
        
  return $ go (BalanceLoopItem initialCount initialForcing initialStep 
                                   initialBalance Balance.emptyBestForcingPair intialResult)
 
  
-}           
           

{-

showBalanceLoopItem::(Show a, Show node,PrintfArg a,Arith.Constant a )=>
  (Counter, BalanceLoopItem node a z) ->
  String
showBalanceLoopItem (bStp, BalanceLoopItem bForc _bFStep bal _) =
  printf " BL: %2d | " bStp ++ printfBalanceFMap bForc bal
-}

-- Ein Allgemeiner Loop wird unnötig kompliziert
{-
loopIO :: 
  (cnt -> cnt) ->
  (result -> quality) ->    
  (input -> result) ->
  (result -> input) ->
  (quality -> cnt -> Bool) ->
  (cnt,quality,z) ->
  [(cnt,quality,z)]
loopIO increment measureQuality deriveNewInput calculateNewResult exitCriteria  (initialCount,initialInput,initialResult) = 
  UtList.takeUntil g $ iterate (calculateNewResult . deriveNewInput) (initialCount,initialInput,initialResult) 
  where
  g (counter,quality,_) = not . exitCriteria counter quality 
  f (counter,_,_,resultIn) = (increment counter, measureQuality newResult, newInput , newResult)
    where newInput = deriveNewInput resultIn
          newResult = calculateNewResult newInput
          

-- ein loop mit IO hat klare Vorteile, besseres Debuggen !!
showEtaLoop ::
  (Show node, Show a, PrintfArg a, Arith.Constant a) =>
  Params.Optimisation node [] Sweep UV.Vector a ->
  [EtaLoopItem node Sweep UV.Vector a z] ->
  [String]
showEtaLoop optParams loop =
  iterateLoops optParams showEtaLoopItem showBalanceLoopItem (zip [0..] loop)

showEtaLoopItem::
  Params.Optimisation node [] Sweep UV.Vector a ->
  (Counter, EtaLoopItem node Sweep UV.Vector a z) ->
  String
showEtaLoopItem _optParams (step, EtaLoopItem _sfgIn _sweep _) =
  printf "EL: %8d" step

-}