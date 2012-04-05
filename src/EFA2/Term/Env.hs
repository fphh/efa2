
module EFA2.Term.Env where

import Control.Monad.Error

import qualified Data.Map as M

-- Variable types of the solver.
data EtaIdx = EtaIdx !Int !Int deriving  (Show)
data PowerIdx = PowerIdx !Int !Int deriving (Show, Ord, Eq)
data XIdx = XIdx !Int !Int deriving (Show, Ord, Eq)
data DEtaIdx = DEtaIdx !Int !Int deriving  (Show)
data DPowerIdx = DPowerIdx !Int !Int deriving (Show, Ord, Eq)


-- EtaIdx x y == EtaIdx y x
instance Eq EtaIdx where
         (EtaIdx a b) == (EtaIdx x y) = f a b == f x y
           where f u v = if u < v then (u, v) else (v, u)

instance Ord EtaIdx where
         compare as@(EtaIdx a b) bs@(EtaIdx x y)
           | as == bs = EQ
           | otherwise = compare (f a b) (f x y)
               where f u v = if u < v then (u, v) else (v, u)

instance Eq DEtaIdx where
         (DEtaIdx a b) == (DEtaIdx x y) = f a b == f x y
           where f u v = if u < v then (u, v) else (v, u)

instance Ord DEtaIdx where
         compare as@(DEtaIdx a b) bs@(DEtaIdx x y)
           | as == bs = EQ
           | otherwise = compare (f a b) (f x y)
               where f u v = if u < v then (u, v) else (v, u)


data IdxError a = PowerIdxError { getError :: PowerIdx, getMap :: a }
                | EtaIdxError EtaIdx a
                | DPowerIdxError DPowerIdx a
                | DEtaIdxError DEtaIdx a
                | XIdxError XIdx a
                | OtherError String deriving (Eq)

instance (Show a) => Show (IdxError a) where
         show (PowerIdxError m idx) = "(" ++ show idx ++ ") not found in " ++ show m

instance Error (IdxError a) where
         noMsg = OtherError "Unknown index error!" 
         strMsg str = OtherError str

type IdxErrorMonad a = Either (IdxError a)

-- Environments
type PowerMap a = M.Map PowerIdx a
type EtaMap a = M.Map EtaIdx a
type DPowerMap a = M.Map DPowerIdx a
type DEtaMap a = M.Map DEtaIdx a
type XMap a = M.Map XIdx a

type LRPowerEnv a = PowerIdx -> IdxErrorMonad (PowerMap a) a
type LREtaEnv a = EtaIdx -> IdxErrorMonad (EtaMap a) a
type LRDPowerEnv a = DPowerIdx -> IdxErrorMonad (DPowerMap a) a
type LRDEtaEnv a = DEtaIdx -> IdxErrorMonad (DEtaMap a) a
type LRXEnv a = XIdx -> IdxErrorMonad (XMap a) a

type EtaEnv a = EtaIdx -> a
type PowerEnv a = PowerIdx -> a
type DEtaEnv a = DEtaIdx -> a
type DPowerEnv a = DPowerIdx -> a
type XEnv a = XIdx -> a


e1 = M.fromList [(PowerIdx 1 0, "abc"), (PowerIdx 2 0, "def")]
e2 = M.fromList [(PowerIdx 2 0, "uvw"), (PowerIdx 3 0, "xyz")]


composeLREnv :: (Ord k) => (a -> IdxErrorMonad (M.Map k v) b) -> (a -> IdxErrorMonad (M.Map k v) b) -> (a -> IdxErrorMonad (M.Map k v) b)
composeLREnv env1 env2 x =
  case (env1 x, env2 x) of
       (y@(Right _), _) -> y
       (_, y@(Right _)) -> y
       (Left err1, Left err2) -> Left (err1 { getMap = M.union (getMap err1) (getMap err2)})

checkIdx :: (Ord a, Show a) => (a -> IdxError (M.Map a b)) -> M.Map a b -> (a -> IdxErrorMonad (M.Map a b) b)
checkIdx err m idx | Just x <- M.lookup idx m = Right x
                 | otherwise = Left (err idx)

mkEnv :: (Show c) => (a -> IdxErrorMonad c b) -> (a -> b)
mkEnv env x
  | Left err <- res = error (show err)
  | Right y <- res = y
  where res = env x

mkPowerEnv :: M.Map PowerIdx a -> LRPowerEnv a -- PowerIdx -> IdxErrorMonad (PowerMap b) b
mkPowerEnv m = checkIdx (flip PowerIdxError m) m

mkEtaEnv ::  M.Map EtaIdx a -> LREtaEnv a -- EtaIdx -> IdxErrorMonad (EtaMap b) b
mkEtaEnv m = checkIdx (flip EtaIdxError m) m

mkDPowerEnv :: M.Map DPowerIdx a -> LRDPowerEnv a -- DPowerIdx -> IdxErrorMonad (DPowerMap b) b
mkDPowerEnv m = checkIdx (flip DPowerIdxError m) m

mkDEtaEnv ::  M.Map DEtaIdx a -> LRDEtaEnv a -- DEtaIdx -> IdxErrorMonad (DEtaMap b) b
mkDEtaEnv m = checkIdx (flip DEtaIdxError m) m

mkXEnv ::  M.Map XIdx a -> LRXEnv a -- XIdx -> IdxErrorMonad (XMap b) b
mkXEnv m = checkIdx (flip XIdxError m) m


{-
class EnvClass a where
      mkPowerEnv :: (M.Map PowerIdx a) -> LRPowerEnv a
      mkEtaEnv :: Gr b c -> LRPowerEnv a -> LREtaEnv a
      mkXEnv :: Gr b c -> LRPowerEnv a -> LRXEnv a

-}