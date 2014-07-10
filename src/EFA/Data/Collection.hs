{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.Collection where

import EFA.Utility(Caller,merror,ModuleName(..),FunctionName, genCaller)
import qualified Data.Map as Map
import qualified Prelude as P
import Prelude hiding (map,filter)

import qualified EFA.Equation.Result as Result

type family OrdData a
type family ValData a


{-
class OrdData a b where
  getOrdData :: a -> b

class ValData a b where
  getValData :: a -> b


class (OrdData a b,ValData a c) => Unpack a where
  unpack :: a -> (b,c)
  pack :: (b,c) -> a
-}

class Unpack a where
  unpack :: a -> (OrdData a, ValData a)
  pack :: (OrdData a,ValData a) -> a
  
data Collection key a = Collection (OrdData a) (Map.Map key (ValData a)) -- deriving (Show,Eq)

instance (Show (ValData a), Show (OrdData a), Show key) => Show (Collection key a) where
  show (Collection grid val) = "Collection " ++ show grid ++ " "  ++ show  val

modul :: ModuleName
modul = ModuleName "Collection"

nc :: FunctionName -> Caller
nc = genCaller modul

mapOrdData ::
 ValData a ~ ValData b =>
 (OrdData a ->
 OrdData b) ->
 Collection key a ->
 Collection key b
mapOrdData f (Collection o m) = Collection (f o) m

mapWithKey :: (OrdData a ~ OrdData b, Unpack b, Unpack a) => (key -> a -> b) -> Collection key a -> Collection key b
mapWithKey f (Collection o m) = Collection o $ Map.mapWithKey (\ k d -> snd $ unpack $ f k $ pack (o,d)) m

mapData :: (ValData a -> ValData a) -> Collection key a -> Collection key a
mapData f (Collection o m) = Collection o $ Map.map f m

mapDataWithOrd :: (OrdData a -> ValData a -> ValData a) -> Collection key a -> Collection key a
mapDataWithOrd f (Collection o m) = Collection o $ Map.map (f o) m


map :: (Unpack a, Unpack b,OrdData a ~ OrdData b) => (a -> b) -> Collection key a -> Collection key b
map f (Collection o m) = Collection o $ Map.map (snd . unpack . f . pack . (,) o) m

mapOrdAndData :: (OrdData a -> OrdData b) -> (ValData a -> ValData b) -> Collection key a -> Collection key b
mapOrdAndData f g (Collection o m) = Collection (f o) (Map.map g m)

getOrdData :: Collection key a -> OrdData a
getOrdData  (Collection o _) = o

getValData :: Collection key a -> Map.Map key (ValData a)
getValData  (Collection _ m) = m

getKeys :: Collection key a -> [key]
getKeys (Collection _ m) = Map.keys m

fromList ::
  (Unpack a,Ord key, Eq (OrdData a)) => Caller ->
  [(key, a)] -> Collection key a
fromList caller xs = Collection o $ Map.fromList datList
   where xs' = P.map (\(x,y) -> (x,unpack y)) xs
         datList = P.map (\(x,y) -> (x, snd $ y)) xs'
         o = ordFromList caller $ P.map fst $ P.map snd xs' 

toList ::
  (Unpack a,Ord key, Eq (OrdData a)) =>
  Collection key a -> [(key, a)]
toList (Collection o m) = P.map (\(x,y)-> (x, pack (o,y))) $ Map.toList m

{-
fromMap :: OrdData a -> Map.Map key a -> Collection (OrdData a) a
fromMap  grid = Collection grid 
-}

ordFromList :: Eq a => Caller -> [a] -> a
ordFromList caller [] = merror caller modul "getOrdFromList" "empty List"
ordFromList _ [o] = o
ordFromList caller (x:xs) =
  if all (==x) xs then x
  else merror caller modul "getOrdFromList" "OrdData differs"

lookupMaybe ::
  (Ord key, Unpack a) =>
  key -> Collection key a -> Maybe a
lookupMaybe key (Collection o m) = case Map.lookup key m of
  Just d -> Just $ pack(o,d)
  Nothing -> Nothing

lookup ::
  (Ord key, Show key,Unpack a) =>
  Caller -> key -> Collection key a -> a
lookup caller key collection = case lookupMaybe key collection of
  Just x -> x 
  Nothing -> merror caller modul "lookup" ("not in collection: " ++ show key)

lookupUnsafe ::
  (Ord key, Show key,Unpack a) =>
  key -> Collection key a -> a
lookupUnsafe key (Collection o m)  = pack (o,m Map.! key) 


filter :: (Ord key) => (ValData a -> Bool) -> Collection key a -> Collection key a
filter f (Collection o m) = Collection o (Map.filter f m)

-- getDetermined :: (Ord key) => Collection key (Result.Result a) -> Collection key a
getDetermined :: 
  (Ord key, OrdData a ~ Result.Result (OrdData b),
   ValData a ~ Result.Result (ValData b)) =>
  Collection key a -> Collection key b
getDetermined collection =   
  mapOrdAndData 
  (\(Result.Determined x) -> x) 
  (\(Result.Determined x) -> x) $ 
  filter (Result.isDetermined) collection