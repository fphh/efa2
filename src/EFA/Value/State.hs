module EFA.Value.State where

--import qualified EFA.Equation.Result as Result
import qualified Data.Map as Map
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Flow.SequenceState.Index as Idx
import Prelude  hiding (zipWith, map,zipWith3)
import qualified Prelude as P
import qualified Data.List as List

data Map a = Map (Map.Map (Maybe Idx.AbsoluteState) a) deriving Show

instance Functor Map where
  fmap f (Map m) = Map $ Map.map f m 
  
map :: (a -> b) -> Map a -> Map b 
map = fmap  

toList :: Map a -> [(Maybe Idx.AbsoluteState, a)]
toList (Map m) = Map.toList m

fromList :: [(Maybe Idx.AbsoluteState, a)] -> Map a
fromList xs = Map $ Map.fromList xs

intersection ::
 Map a ->
 Map b ->
 Map a
intersection (Map m) (Map m1) = Map $ Map.intersection m m1  

union ::
 Map a ->
 Map a ->
 Map a
union (Map m) (Map m1) = Map $ Map.union m m1  

zipWith ::
  (a -> b -> c) ->
 Map a->
 Map b ->
 Map c 
zipWith f m m1 = fromList $ P.zipWith (\(x,y) (_,y1) -> (x,f y y1)) xs xs1
  where
    xs = toList $ intersection m m1
    xs1 = toList $ intersection m1 m
    
zipWith3 :: 
  (a -> b -> c -> d) -> 
  Map a -> 
  Map b -> 
  Map c -> 
  Map d
zipWith3 f m m1 m2 = zipWith (\(x,y) z -> f x y z) (zipWith ((,)) m m1) m2     
  

instance Arith.Sum a => Arith.Sum (Map a) where
  (~+) x y = zipWith (Arith.~+) x y
  (~-) x y = zipWith (Arith.~-) x y
  negate x = fmap Arith.negate x

  
instance (Arith.Constant a, Arith.Sum a, Arith.Product a) => Arith.Product (Map a) where
  (~*) x y =  zipWith (Arith.~*) x y
  (~/) x y =  zipWith (Arith.~/) x y  
  recip x = fmap Arith.recip x
  constOne x = fmap (\ _ -> Arith.one) x 
  
getBest :: (a -> a -> Ordering) -> (Map a) -> ([Maybe Idx.AbsoluteState],Maybe a)
getBest compareFunction m = List.foldl' f ([],Nothing) $ toList m
  where f (_,Nothing) (st,x) = ([st],Just x)   
        f (stateList,Just ma) (st,x) = 
          case compareFunction x ma of
            LT -> (stateList,Just ma)                          
            EQ -> (stateList++[st],Just ma)
            GT -> ([st],Just x)  


zipWithUnion :: (a -> a -> a) -> Map a -> Map a -> Map a
zipWithUnion f m m1 = fromList $ P.zipWith (\(x,y) (_,y1) -> (x,f y y1)) xs xs1
  where
    xs = toList $ union m m1
    xs1 = toList $ union m1 m


maxWith :: (a -> a -> Ordering) -> Map a -> Map a -> Map a
maxWith f x y = zipWithUnion g x y
  where 
    g a b = case f a b of
            LT -> b
            EQ -> a
            GT -> a