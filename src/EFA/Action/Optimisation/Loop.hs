{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE UndecidableInstances #-}     

module EFA.Action.Optimisation.Loop where

import qualified EFA.Action.Flow.Balance as Balance
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.StateFlow.Optimality as StateFlowOpt
import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Flow.SequenceState.Index as Idx

import Debug.Trace (trace)
import qualified EFA.Equation.Arithmetic as Arith
--import qualified Data.Map as Map
import qualified EFA.Utility.List as UtList


--import qualified EFA.Report.FormatValue as FormatValue
import qualified EFA.Report.Format as Format

import qualified Data.Map as Map
import qualified Data.List as List

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

instance Display Int where
  disp =  show

instance Display Double where
  disp =  Format.realExp      

instance (Show a,Display a) => Display (Interp.Val a) where
    disp (Interp.Inter x) =  disp x
    disp (Interp.Extra x) =  disp x
    disp x =  show x
        
instance (Show a,Display a) => Display (Maybe a) where
    disp (Just x) =  disp x
    disp (Nothing ) =  "Nothing"

instance (Show a, Arith.Constant a, Display a) => Display (Balance.SocDrive a) where
  disp x = disp $ Balance.getSocDrive x

instance (Display a) => Display (FlowOpt.GenerationEfficiency a) where
  disp (FlowOpt.GenerationEfficiency x) = "Eta-Gen: " ++ disp x 

instance(Display a)=> Display (FlowOpt.UsageEfficiency a) where
  disp (FlowOpt.UsageEfficiency x) = "Eta-Use: " ++ disp x 

  {-
instance  (Show node, Show a) =>
  Display (Balance.ForcingMap Balance.Absolute (Map.Map node (Balance.SocDrive a))) where
  disp (Balance.ForcingMap m) = "Fo: " ++ disp m
-}

instance (Display a,Display b) => Display (a,b) where
  disp (x,y) = "(" ++ disp x ++"," ++ disp y ++ ")"
{-
instance Display (Maybe (Balance.SocDrive (Interp.Val Double), Interp.Val Double)) where
  disp (Just (x,y)) = "(" ++ disp x ++"," ++ disp y ++ ")"
  disp Nothing = "Nothing"
-}
instance (Show a, Show node, Display a) => Display [(node,a)] where
  disp xs = List.intercalate ", " $ map f xs   
    where
      f (n,x) = "(" ++ show n ++ "," ++ disp x ++ ")"

instance (Show node, Show a,  Display a) => Display (Map.Map node a) where
  disp m = disp $ Map.toList m 

instance  (Show node, Show a, Arith.Constant a,  Display a) =>
  Display (Balance.ForcingMap Balance.Absolute (Map.Map node (Balance.SocDrive a))) where
  disp (Balance.ForcingMap m) = "Fo: " ++ disp m

instance   (Show node, Show a, Arith.Constant a,  Display a) => 
  Display (Balance.ForcingMap Balance.Step (Map.Map node (Balance.SocDrive a))) where
  disp (Balance.ForcingMap m) = "St: " ++ disp m

instance   (Show node, Show a,  Display a) => 
  Display (Balance.Balance node a) where
  disp (Balance.Balance m) = "Bal: " ++ disp m

instance (Show a, Display a, Display Int) => Display (Balance.BalanceCounter a) where
  disp (Balance.BalanceCounter m) = "Cnt: " ++ disp m
{-
instance (Show a,Display a) => Display (a, a) where
  disp  (x, y) = show x ++ " " ++ disp y


instance (Show a, Arith.Constant a) => Display (Balance.SocDrive a) where
  disp x = show $ Balance.getSocDrive x 

instance (Show a,Arith.Constant a,Display a) => Display (a, b) where
  disp  (Just (x, y)) = "Fo: " ++ disp x ++ "Bal: " ++ show y
  disp Nothing = "Nothing"
-}

instance 
  (Show a, 
   Display (Maybe (Balance.SocDrive a, a), Maybe (Balance.SocDrive a, a))) => 
  Display (Balance.BestForcingPair a) where
  disp (Balance.BestForcingPair x) = "BP: " ++ disp x


instance 
  (Show node,
   Show a,
   Display (FlowOpt.GenerationEfficiency a),
   Display (FlowOpt.UsageEfficiency a)) => 
  Display (FlowOpt.LifeCycleMap node a) where
  disp (FlowOpt.LifeCycleMap m) = "LC:" ++ (List.intercalate "\n" $ map f $ Map.toList m)
    where f (Idx.AbsoluteState st, x) = "( AbsState: " ++ show st ++ ", " ++ disp x ++ ")"


data EtaLoopParams node a evalParam = 
  EtaLoopParams
  { accessMaxEtaIterations :: MaxEtaIterations, 
--    accLifeCycleMethod :: StateFlowOpt.LifeCycleMethod,
    accInitialEvalParam :: evalParam,
    accGlobalLifeCycleMap :: FlowOpt.GlobalLifeCycleMap node a}
  
  
data BalanceLoopParams node a =  
  BalanceLoopParams 
  {accessMaxIterationsPerStorage :: Balance.MaxIterationsPerStorage , 
   accessMaxIterations :: Balance.MaxIterations,
   accessThreshold :: Balance.Threshold a, 
   accessInitialForcing :: Balance.Forcing node a,
   accessInitialStep :: Balance.ForcingStep node a,   
   accessInitialSto :: node}

data EtaLoopItem node a evalParam evalSwp res = 
  EtaLoopItem {
    accEtaCounter :: EtaCounter, 
    accEtaSys :: a,
    --accLifeCycleMap :: FlowOpt.LifeCycleMap node a, 
    accEvalParam :: evalParam,
    accEvalSweep :: evalSwp,
    accBalLoop :: [BalanceLoopItem node a res]}
  
instance 
  (Display (FlowOpt.GenerationEfficiency a),Display evalParam,
   Display (FlowOpt.UsageEfficiency a), 
   Display (Maybe (Balance.SocDrive a, a), Maybe (Balance.SocDrive a, a)), Arith.Constant a,Display a,
   Show node, Show a) => Show (EtaLoopItem node a evalParam evalSwp res) where  
  show (EtaLoopItem etaCounter etaSys evalParam _ balLoop) = 
    "Eta-Cnt: " ++ show etaCounter ++ "\n" ++
    "Eta-Sys: " ++ disp etaSys ++ "\n" ++
    disp evalParam ++ "\n" ++ show balLoop 
  
instance 
  (Show node, Show a, Arith.Constant a, Display a, 
   Display (Maybe (Balance.SocDrive a, a), Maybe (Balance.SocDrive a, a))) => 
  Show (BalanceLoopItem node a z) where  
  show (BalanceLoopItem cnt node force step balance bestPair _) = 
    show cnt ++ " | " ++
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

etaLoop:: 
  (Ord node,Ord a, Show a, Show node, Arith.Constant a) =>
  Caller ->
  [node] ->
  EtaLoopParams node a evalParam ->
  BalanceLoopParams node a ->
  (evalParam -> evalSwp) ->
  (evalSwp -> Balance.Forcing node a -> res) ->
  (res -> Balance.Balance node a) ->
  (res -> evalParam -> (a,evalParam)) ->
  [EtaLoopItem node a evalParam evalSwp res]
etaLoop caller storages etaParams balParams evalFunction systemFunction getBalance updateEvalParam = 
   UtList.takeUntil check $ 
   go (EtaCounter (-1)) initialEvalParam initialForcing
  where
    check (EtaLoopItem (EtaCounter count) _ _ _ _) = (count+1) >= maxEtaIterations 
    (MaxEtaIterations maxEtaIterations) = accessMaxEtaIterations etaParams
    initialForcing = accessInitialForcing balParams
    initialEvalParam = accInitialEvalParam etaParams
      
    go lastCount evalParam lastLatestForcing = [EtaLoopItem count etaSys evalParam evalSweep balLoop] ++ 
                                                  go count nextEvalParam latestForcing 
      where
        count = incrementEtaCounter lastCount
        evalSweep = evalFunction evalParam 
        balLoop = balanceLoop (caller |> nc "etaLoop") storages balParams (systemFunction evalSweep)
                  getBalance lastLatestForcing
        (etaSys,nextEvalParam) = updateEvalParam (accResult $ last balLoop) evalParam
        latestForcing = accForcing $ last balLoop          

f str = id 
-- f str x = trace (str ++": " ++ show x) x

balanceLoop :: 
  (Ord a, Ord node, Show node, Arith.Constant a,Show a) =>
  Caller ->       
  [node] ->                          
  BalanceLoopParams node a ->
  (Balance.Forcing node a -> res) ->
  (res -> Balance.Balance node a) ->
  Balance.Forcing node a ->
  [BalanceLoopItem node a res]
balanceLoop caller storages balParams systemFunction getBalance initialForcing = 
  UtList.takeUntil check $ 
  go $ resetBalCounter  (BalanceLoopItem initialCount initialSto initialForcing initialStep initialBal initialPair initialResult)
  where
    initialPair = Balance.emptyBestForcingPair
    initialCount = Balance.initialiseBalanceCounter storages
    initialSto = accessInitialSto balParams
    initialStep = accessInitialStep balParams
    initialResult = systemFunction initialForcing
    initialBal = getBalance initialResult
    
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
        
        

getLastResult :: [EtaLoopItem node a evalParam evalSwp res] -> res
getLastResult = accResult . last . accBalLoop . last