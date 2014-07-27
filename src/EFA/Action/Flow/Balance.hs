-- {-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE EmptyDataDecls #-}

module EFA.Action.Flow.Balance where

import qualified EFA.Flow.State.Quantity as StateQty
--import qualified EFA.Flow.SequenceState.Index as Idx
--import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+))

import EFA.Equation.Result (Result)

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Maybe as Maybe

--import Data.Bimap (Bimap)
import Data.Maybe(fromMaybe)
import Text.Printf (printf, PrintfArg) --,IsChar)

import EFA.Utility(Caller,
                 merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.Balance"

nc :: FunctionName -> Caller
nc = genCaller modul

-- | The 'SocDrive' data type should always contain positive values.
-- Use 'getSocDrive' to get the drive with signs corrected.
data SocDrive a =
  NoDrive               -- ^ No drive
  | ChargeDrive a       -- ^ Charging states should be prefered
  | DischargeDrive a    -- ^ Discharging states should be prefered
  deriving (Show)


instance Functor SocDrive where
  fmap f (ChargeDrive x) = ChargeDrive (f x)
  fmap f (DischargeDrive x) = DischargeDrive (f x)
  fmap _ NoDrive = NoDrive

getSocDrive ::
  (Arith.Sum a, Arith.Constant a) => SocDrive a -> a
getSocDrive soc =
  case soc of
       DischargeDrive x -> Arith.negate x
       ChargeDrive x -> x
       NoDrive -> Arith.zero

setSocDrive ::
  (Arith.Sum a, Arith.Constant a,Ord a) => a -> SocDrive a
setSocDrive x =
  case Arith.sign x of
       Arith.Positive -> ChargeDrive x
       Arith.Zero -> NoDrive
       Arith.Negative -> DischargeDrive $ Arith.negate x


noforcing ::
  (Arith.Constant v) =>
  SocDrive v -> StateQty.Graph node b (Result v) -> v
noforcing _ _ = Arith.zero

newtype Threshold a =
  Threshold { unThreshold :: a } deriving Show

newtype Balance node a = Balance (Map node a) deriving Show


data Absolute
data Step

newtype ForcingMap x a =
  ForcingMap { unForcingMap :: a } deriving (Show)

instance Functor (ForcingMap x) where
  fmap f (ForcingMap m) = ForcingMap (f m)

type Forcing node a =
  ForcingMap Absolute (Map node (SocDrive a))

type ForcingStep node a =
  ForcingMap Step (Map node (SocDrive a))


unMaybeBalance :: (Show node, Show a) => Caller -> Balance node (Maybe a) -> Balance node a
unMaybeBalance caller (Balance m) = Balance (Map.map (Maybe.fromMaybe err) m)
  where err = merror caller modul "unMaybeBalance" $ "Undefined SOC: " ++ show m  

--newtype BestForcingPair node a = BestForcingPair (Map node (Maybe (SocDrive a, a), Maybe (SocDrive a, a)))
newtype BestForcingPair a = BestForcingPair (Maybe (SocDrive a, a), Maybe (SocDrive a, a)) deriving Show

emptyBestForcingPair :: BestForcingPair a
emptyBestForcingPair = BestForcingPair (Nothing,Nothing) 

checkBalance ::
  (Ord a, Arith.Sum a) =>
  Threshold a ->
  Balance node a ->
  Bool
checkBalance threshold (Balance bal) =
  Map.foldl' (\acc v -> acc && Arith.abs v <= bt) True bal
  where bt = unThreshold threshold

checkBalanceSingle ::
  (Ord a, Arith.Sum a, Ord node, Show node) =>
  Caller ->
  Threshold a ->
  Balance node a ->
  node ->
  Bool
checkBalanceSingle caller threshold bal sto =
  Arith.abs x <= unThreshold threshold
  where x = lookupStorageBalance (caller |> nc "checkBalanceSingle") bal sto

-- | Rate Balance Deviation by sum of Standard Deviation and Overall Sum
balanceDeviation ::
  (Arith.Product a,
   Ord a,
   Arith.Constant a,
   Arith.Sum a) =>
  Balance node a -> a
balanceDeviation (Balance m) =
  Map.foldl (\acc x -> acc ~+ Arith.square x) Arith.zero m
  ~+
  Arith.abs (Map.foldl (~+) Arith.zero m)


newtype MaxIterationsPerStorage = MaxIterationsPerStorage Int deriving Show

newtype BalanceCounter node = BalanceCounter (Map.Map node Int) deriving Show
newtype MaxIterations = MaxIterations Int deriving Show

-- data BalanceCounter node = BalanceCounter (Map.Map node Int)

initialiseBalanceCounter :: (Ord node) => [node] -> BalanceCounter node
initialiseBalanceCounter storages = 
  BalanceCounter $ Map.fromList $ zip storages $ map (\_ -> 0) storages


incrementBalanceCounter :: (Ord node) => BalanceCounter node -> node -> BalanceCounter node
incrementBalanceCounter (BalanceCounter m) sto =  
  BalanceCounter (Map.adjust (+1) sto m)
  
checkBalanceCounterOne :: (Ord node,Show node) => Caller -> BalanceCounter node -> node -> MaxIterationsPerStorage -> Bool 
checkBalanceCounterOne caller (BalanceCounter m) sto (MaxIterationsPerStorage ma) = f $ Map.lookup sto m
  where f (Just cnt) = cnt >= ma
        f Nothing = merror caller modul  "checkBalanceCounterOne" $ "Storage not found: " ++ show sto 

checkBalanceCounter :: (Ord node) => BalanceCounter node -> MaxIterations -> Bool 
checkBalanceCounter (BalanceCounter m) (MaxIterations ma) = Map.foldl (+) 0 m > ma

selectStorageToForce ::(Ord node, Ord a, Show node, Arith.Sum a) =>
  Caller ->
  Balance node a ->
  Threshold a ->
  BalanceCounter node ->
  MaxIterationsPerStorage ->
  node ->
  node
selectStorageToForce caller bal@(Balance b) threshold cnt maxIterationsPerStorage sto = sto --if check then sto else nextSto 
  where
    check = (checkBalanceSingle (caller |> nc "selectStorageToForce") threshold bal sto) || 
            checkBalanceCounterOne (caller |> nc "selectStorageToForce") cnt sto maxIterationsPerStorage
    nextSto = if not $ null $ nextStos then head nextStos else head $ Map.keys b           
    nextStos = tail $ snd $ break (==sto) $ Map.keys b  
      

calculateNextBalanceStep ::
  (Ord a, Arith.Constant a,Arith.Sum a,Arith.Product a, Show a,
   Ord node, Show node) =>
  Caller ->
  Balance node a ->
  BestForcingPair a -> -- Maybe (SocDrive a,a), Maybe (SocDrive a,a)) ->
  (ForcingStep node a) ->
  node ->
  (ForcingStep node a)
calculateNextBalanceStep caller balMap (BestForcingPair bestPair) stepMap sto = -- stepMap
  updateForcingStep stepMap sto $ setSocDrive step1
  where
    bal = lookupStorageBalance (caller |> nc "calculateNextBalanceStep") balMap sto
    step = lookupBalanceForcingStep (caller |> nc "calculateNextBalanceStep") stepMap sto
    fact = Arith.fromRational 2.0
    divi = Arith.fromRational 1.7
    intervall = getForcingIntervall bestPair


    step1 =
      case (intervall, Arith.sign bal) of

           -- Zero Crossing didn't occur so far -- increase step to search faster
           (Nothing, Arith.Negative) -> Arith.abs $ (getSocDrive step) Arith.~* fact
           (Nothing, Arith.Positive) -> Arith.negate $ (Arith.abs $ getSocDrive step) Arith.~* fact

           -- The Zero Crossing is contained in the intervall
           -- defined by bestPair - step just a little over the middle
           (Just int, Arith.Negative) -> (getSocDrive int) Arith.~/ divi
           (Just int, Arith.Positive) -> (Arith.negate $ getSocDrive  int) Arith.~/ divi
           (_, Arith.Zero)  -> Arith.zero
           

rememberBestBalanceForcing ::
  (Arith.Constant a, Ord a, Ord node, Show node, Show a) =>
  Caller ->
  BestForcingPair a ->
   --(Maybe (SocDrive a,a), Maybe (SocDrive a,a)) ->
  (Forcing node a, Balance node a) ->
  node ->
  BestForcingPair a
--   (Maybe (SocDrive a, a), Maybe (SocDrive a, a))
rememberBestBalanceForcing caller (BestForcingPair (neg, pos)) (forceMap, balMap) sto = 
  BestForcingPair $ if bal >= Arith.zero then (neg, g pos) else (h neg, pos)
  where
    bal = lookupStorageBalance (caller |> nc "rememberBestBalanceForcing") balMap sto
    force = lookupBalanceForcing (caller |> nc "rememberBestBalanceForcing") forceMap sto

    -- TODO :: Fix is compensating for non-Monotonic behaviour of Forcing -> Balance
    -- Correct solution would be to work with an Balance Intervall
    -- Which is calculated by maximum duration charging states to max. duration discharging states


    g (Just (f, b)) =
      if bal <= b || (getSocDrive force < getSocDrive force && bal > b)
         then Just (force, bal)
         else Just (f, b)
    g Nothing = Just (force, bal)

    h (Just (f, b)) =
      if (Arith.abs bal <= Arith.abs b) || (getSocDrive force > getSocDrive f && bal < b)
         then Just (force, bal)
         else Just (f, b)
    h Nothing = Just (force, bal)


checkCrossingEverOccured ::
  (Maybe (SocDrive a,a), Maybe (SocDrive a, a)) -> Bool
checkCrossingEverOccured (Just _, Just _) = True
checkCrossingEverOccured _ = False

getForcingIntervall ::  (Ord a, Arith.Constant a) =>
  (Maybe (SocDrive a,a), Maybe (SocDrive a,a)) ->
  Maybe (SocDrive a)
getForcingIntervall (Just (n, _), Just (p, _)) =
  Just $ setSocDrive $ (Arith.abs $ getSocDrive n) ~+ getSocDrive p
getForcingIntervall _ = Nothing


addForcingStep ::
  (Ord node, Ord a, Arith.Constant a, Show node) =>
  Caller ->
  Forcing node a ->
  ForcingStep node a ->
  node ->
  Forcing node a
addForcingStep caller forcing stepMap sto =
  fmap (Map.adjust f sto) forcing
  where f force = setSocDrive $ getSocDrive force ~+ getSocDrive step
        step = lookupBalanceForcingStep (caller |> nc "addForcingStep") stepMap sto

updateForcingStep ::
  (Ord node, Ord a, Arith.Constant a) =>
  ForcingStep node a ->
  node ->
  SocDrive a ->
  ForcingStep node a
updateForcingStep forcing storage step =
  fmap (Map.adjust (const step) storage) forcing

lookupBalanceForcing ::
  (Ord node, Show node) =>
  Caller ->
  Forcing node a ->
  node ->
  SocDrive a
lookupBalanceForcing caller forcing sto =
  fromMaybe e $ Map.lookup sto $ unForcingMap forcing
  where e = merror caller modul "lookupBalanceForcing" 
            ("Storage not in Map: " ++ show sto)


lookupBalanceForcingStep ::
  (Ord node, Show node) =>
  Caller ->
  ForcingStep node a ->
  node ->
  SocDrive a
lookupBalanceForcingStep caller forcing sto =
  fromMaybe e $ Map.lookup sto $ unForcingMap forcing
  where e = merror caller modul "lookupBalanceForcingStep"
            (" - gStorage not in Map: " ++ show sto)


lookupStorageBalance ::
  (Ord node, Show node) =>
  Caller ->  Balance node a ->
  node -> a
lookupStorageBalance caller (Balance balance) sto =
  fromMaybe e $ Map.lookup sto balance
  where e = merror caller modul "lookupStorageBalance"
          (" - Storage not in Map: " ++ show sto)


-- PrintF - Functions

concatZipMapsWith :: ((k0, a) -> (k1, b) -> [c]) -> Map k0 a -> Map k1 b -> [c]
concatZipMapsWith f s t = concat $ zipWith f (Map.toList s) (Map.toList t)

printfMap :: (Show k, Show a) => Map k a -> String
printfMap m = Map.foldWithKey f "" m
  where f k v acc = printf "%16s\t%16s\n" (show k) (show v) ++ acc

printfBalanceFMap ::
  (Show node, PrintfArg a1, PrintfArg t1, Arith.Constant a1) =>
  Forcing node a1 ->
  Map t t1 ->
  [Char]
printfBalanceFMap forceMap balanceMap =
  concatZipMapsWith f (unForcingMap forceMap) balanceMap
  where f (k, x) (_, y) =
          printf "   | Sto: %7s   | F: %10.15f   | B: %10.15f"
                 (show k) (getSocDrive x) y

printfBalanceMap ::
  (Show a, PrintfArg t) =>
  Map a t -> String
printfBalanceMap balanceMap = Map.foldWithKey f "" balanceMap
  where f k v acc = printf " Sto: %7s B: %5.3f" (show k) v ++ acc

