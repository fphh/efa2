{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.Collection where

import EFA.Utility(Caller,merror,ModuleName(..),FunctionName, genCaller)
import qualified Data.Map as Map
import qualified Prelude as P
import Prelude hiding (map)


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
  
data Collection key a = Collection (OrdData a) (Map.Map key (ValData a))

modul :: ModuleName
modul = ModuleName "Collection"

nc :: FunctionName -> Caller
nc = genCaller modul


mapData :: (ValData a -> ValData a) -> Collection label a -> Collection label a
mapData f (Collection o m) = Collection o $ Map.map f m

mapDataWithOrd :: (OrdData a -> ValData a -> ValData a) -> Collection label a -> Collection label a
mapDataWithOrd f (Collection o m) = Collection o $ Map.map (f o) m


map :: (Unpack a) => (a -> a) -> Collection label a -> Collection label a
map f (Collection o m) = Collection o $ Map.map (snd . unpack . f . pack . (,) o) m


getOrdData :: Collection label a -> OrdData a
getOrdData  (Collection o _) = o

getValData :: Collection label a -> Map.Map label (ValData a)
getValData  (Collection _ m) = m

fromList ::
  (Unpack a,Ord label, Eq (OrdData a)) => Caller ->
  [(label, a)] -> Collection label a
fromList caller xs = Collection o $ Map.fromList datList
   where xs' = P.map (\(x,y) -> (x,unpack y)) xs
         datList = P.map (\(x,y) -> (x, snd $ y)) xs'
         o = ordFromList caller $ P.map fst $ P.map snd xs' 

toList ::
  (Unpack a,Ord label, Eq (OrdData a)) =>
  Collection label a -> [(label, a)]
toList (Collection o m) = P.map (\(x,y)-> (x, pack (o,y))) $ Map.toList m


ordFromList :: Eq a => Caller -> [a] -> a
ordFromList caller [] = merror caller modul "getOrdFromList" "empty List"
ordFromList _ [o] = o
ordFromList caller (x:xs) =
  if all (==x) xs then x
  else merror caller modul "getOrdFromList" "OrdData differs"

lookup ::
  (Ord label, Unpack a) =>
  label -> Collection label a -> Maybe a
lookup label (Collection o m) = case Map.lookup label m of
  Just d -> Just $ pack(o,d)
  Nothing -> Nothing

