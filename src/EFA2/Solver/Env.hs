
module EFA2.Solver.Env where

import Control.Monad.Error

import qualified Data.Map as M

-- | Variable types of the solver. The solver, in fact, is
-- ignorant of the provenance of the variables. However, to
-- facilitate life, we introduce variable types, that make
-- it easy to express things needed in energy flow analysis,
-- that is:
--
-- * a section number
-- * a data record number
-- * two numbers to identify a place in the topology
data PowerIdx = PowerIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)
data EtaIdx = EtaIdx !Int !Int !Int !Int deriving  (Show)
data XIdx = XIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)
data DEtaIdx = DEtaIdx !Int !Int !Int !Int deriving  (Show)
data DPowerIdx = DPowerIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)

-- | This variable type can be used to express arbitrary relations.
-- You can variables also make dependent on section and record.
data VarIdx = VarIdx !Int !Int !Int deriving (Show, Ord, Eq)

{-
class EnvIndex a where
      sectionNum :: a -> Int
      recordNum :: a -> Int
      fromNode :: a -> Int
      toNode :: a -> Int

instance EnvIndex PowerIdx where
         sectionNum (PowerIdx x _ _ _) = x
         recordNum (PowerIdx _ x _ _) = x
         fromNode (PowerIdx _ _ x _) = x
         toNode (PowerIdx _ _ _ x) = x
-}

-- EtaIdx x y == EtaIdx y x
instance Eq EtaIdx where
         (EtaIdx s1 r1 a b) == (EtaIdx s2 r2 x y) = (s1, r1, f a b) == (s2, r2, f x y)
           where f u v = if u < v then (u, v) else (v, u)

instance Ord EtaIdx where
         compare as@(EtaIdx s1 r1 a b) bs@(EtaIdx s2 r2 x y)
           | as == bs && s1 == s2 && r1 == r2 = EQ
           | otherwise = compare (s1, r1, (f a b)) (s2, r2, (f x y))
               where f u v = if u < v then (u, v) else (v, u)

instance Eq DEtaIdx where
         (DEtaIdx s1 r1 a b) == (DEtaIdx s2 r2 x y) = (s1, r1, f a b) == (s2, r2, f x y)
           where f u v = if u < v then (u, v) else (v, u)

instance Ord DEtaIdx where
         compare as@(DEtaIdx s1 r1 a b) bs@(DEtaIdx s2 r2 x y)
           | as == bs && s1 == s2 && r1 == r2 = EQ
           | otherwise = compare (s1, r1, (f a b)) (s2, r2, (f x y))
               where f u v = if u < v then (u, v) else (v, u)


data IdxError a = PowerIdxError { getIdx :: PowerIdx, getMap :: a }
                | EtaIdxError EtaIdx a
                | DPowerIdxError DPowerIdx a
                | DEtaIdxError DEtaIdx a
                | XIdxError XIdx a
                | VarIdxError VarIdx a
                | OtherError String deriving (Eq)

instance (Show a) => Show (IdxError a) where
         show err = "(" ++ show (getIdx err) ++ ") not found in: " ++ show (getMap err)



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
type VarMap a = M.Map VarIdx a




{-
type LRPowerEnv a = PowerIdx -> IdxErrorMonad (PowerMap a) a
type LREtaEnv a = EtaIdx -> IdxErrorMonad (EtaMap a) a
type LRDPowerEnv a = DPowerIdx -> IdxErrorMonad (DPowerMap a) a
type LRDEtaEnv a = DEtaIdx -> IdxErrorMonad (DEtaMap a) a
type LRXEnv a = XIdx -> IdxErrorMonad (XMap a) a
type LRVarEnv a = VarIdx -> IdxErrorMonad (VarMap a) a

type EtaEnv a = EtaIdx -> a
type PowerEnv a = PowerIdx -> a
type DEtaEnv a = DEtaIdx -> a
type DPowerEnv a = DPowerIdx -> a
type XEnv a = XIdx -> a
type VarEnv a = VarIdx -> a


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

mkPowerEnv :: M.Map PowerIdx a -> LRPowerEnv a
mkPowerEnv m = checkIdx (flip PowerIdxError m) m

mkEtaEnv ::  M.Map EtaIdx a -> LREtaEnv a
mkEtaEnv m = checkIdx (flip EtaIdxError m) m

mkDPowerEnv :: M.Map DPowerIdx a -> LRDPowerEnv a
mkDPowerEnv m = checkIdx (flip DPowerIdxError m) m

mkDEtaEnv ::  M.Map DEtaIdx a -> LRDEtaEnv a
mkDEtaEnv m = checkIdx (flip DEtaIdxError m) m

mkXEnv ::  M.Map XIdx a -> LRXEnv a
mkXEnv m = checkIdx (flip XIdxError m) m

-}