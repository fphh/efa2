{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE  FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-} 
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE TypeFamilies #-} 

module EFA.Data.OD.Signal.Flow where

--import qualified EFA.Value as Value
--import qualified EFA.Data.OrdData as OrdData
--import qualified EFA.Data.ND as ND
--import qualified EFA.Data.Axis.Mono as Mono
import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.Vector as DV
--import qualified EFA.Data.Vector.NonEmpty as EV

import qualified EFA.Equation.Arithmetic as Arith
--import qualified Graphics.Gnuplot.Value.Atom as Atom
--import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified EFA.Report.FormatValue as FormatValue
import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Value.Type as Type

import qualified EFA.Data.OD.Signal as Signal
import qualified Data.Maybe as Maybe
--import qualified Data.NonEmpty.Map as NEMap
import qualified Data.Map as Map

import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

import EFA.Equation.Arithmetic as Arith
--import qualified EFA.IO.TableParserTypes as ParseTable
--import qualified EFA.Value.Type as Type

--import qualified Data.List as List
--import qualified Data.Map as Map
--import qualified EFA.Data.Vector.NonEmpty as EV

import qualified Data.NonEmpty.Set as NonEmptySet
--import qualified Data.Set as Set

import qualified Data.NonEmpty as NonEmpty
--import qualified Data.NonEmpty.Class as NonEmptyClass

import Prelude hiding (map,zipWith,foldl, length, sum) 
import qualified Prelude as P

modul :: ModuleName
modul = ModuleName "OD.Curve"

nc :: FunctionName -> Caller
nc = genCaller modul

data Data inst vec a = Data {getVector :: vec a} deriving Show

instance (FormatValue.FormatValue (vec a)) => FormatValue.FormatValue (Data inst vec a) where
  formatValue (Data vec) = FormatValue.formatValue vec 

data TimeStep a = TimeStep {getMidTime :: a,  
                            getTimeStep :: a} deriving (Show, Eq)
                                                       
instance  (Eq a, Ord a) => Ord (TimeStep a) where                                                     
  compare (TimeStep x _) (TimeStep y _) = compare x y

data Signal inst label vec a b = Signal {getTime :: Strict.Axis inst label vec (TimeStep a), 
                                         getData :: Data inst vec b}
{-
instance 
  (Show label,
   Show a,
   DV.Storage vec (TimeStep a),
   DV.FromList vec) =>
   Show (Strict.Axis inst label vec (TimeStep a)) where
  show (Strict.Axis label typ vec) = 
    "Strict.Axis " ++ show label ++ " " ++ show typ ++ " DV.fromList " ++ (show $ DV.toList vec) 
-}  
instance 
  (Show label, 
   Show (vec (TimeStep a)), 
   Show (vec b)) => 
  Show (Signal inst label vec a b) where
    show (Signal time dat) = "FSignal " ++ show time ++ " " ++ show dat

data HRecord key inst label vec a b = 
  HRecord {getHTime :: Strict.Axis inst label vec (TimeStep a),
           getHMap :: Map.Map key (Data inst vec b)}

instance 
  (Show label,
   Show (vec (TimeStep a)),
   Show key,
   Show (vec b),
   Ord (TimeStep a),
   DV.Storage vec (TimeStep a)) =>
   Show (HRecord key inst label vec a b) where
    show (HRecord time m) = "Flow.HRecord " ++ show time ++ " " ++ show m
  
                                               
data VRecord key inst label vec vec1 a b = 
  VRecord {getVRTime :: Strict.Axis inst label vec (TimeStep a),
           getVRMap ::  Map.Map key Signal.SampleIdx,
           getVRSignal :: Data inst vec (Signal.Samples vec1 b)}
                                                                              
mapHRecord :: 
  (Data inst vec b -> Data inst vec c) ->
  HRecord key inst label vec a b ->
  HRecord key inst label vec a c
mapHRecord f (HRecord t m) = HRecord t $ Map.map f m   

hRecordToList :: HRecord key inst label vec a b -> [(key,Signal inst label vec a b)] 
hRecordToList record = P.map (\(x,y)->(x,Signal time y)) xs 
  where xs = Map.toList $ getHMap record
        time = getHTime record

getHRecordKeys :: HRecord key inst label vec a b -> [key]
getHRecordKeys = P.map fst . hRecordToList 

fromList :: 
  (Ord a,
   DV.Zipper vec,
   DV.Storage vec Bool,
   DV.Singleton vec,
   DV.Storage vec a,
   DV.Storage vec b,
   DV.FromList vec,
   Ord (TimeStep a),
   DV.Storage vec (TimeStep a)) => 
  Caller -> label -> Type.Dynamic -> [(TimeStep a,b)] -> Signal inst label vec a b
fromList caller label typ xs = 
  let t = Strict.fromList (caller |> (nc "fromList")) label typ $ P.map fst xs
      v = Data $ DV.fromList $ P.map snd xs
  in Signal t v
     

fromListData :: 
  (DV.Storage vec a, DV.FromList vec) => 
  Caller -> [a] -> Data inst vec a
fromListData caller xs = 
  if P.length xs >=1 then Data $ DV.fromList xs
                   else merror caller modul "fromListData" "emptyList"     
                                               

lookupUnsafe :: (DV.LookupUnsafe vec b) => Signal inst label vec a b -> Strict.Idx -> b
lookupUnsafe signal (Strict.Idx idx) = applyOut (flip DV.lookupUnsafe idx) signal


apply :: ( vec b  ->  vec c) -> (Signal inst label vec a b) -> (Signal inst label vec a c)
apply f (Signal axis (Data vec)) = Signal axis $ Data $ f vec

applyOut :: ( vec b  -> c) -> (Signal inst label vec a b) ->  c
applyOut f (Signal _ (Data vec)) = f vec

applyOutData :: ( vec a  -> b) -> (Data inst vec a) ->  b
applyOutData f (Data vec) = f vec

apply2 :: ( vec b  ->  vec c ->  vec d) -> 
            Signal inst label vec a b -> 
            Signal inst label vec a c ->
            Signal inst label vec a d
apply2 f (Signal axis (Data vec)) (Signal _ (Data vec1)) = Signal axis $ Data $ f vec vec1

apply2Data :: (vec a  ->  vec b ->  vec c) ->
              Data inst vec a ->
              Data inst vec b ->
              Data inst vec c
apply2Data f (Data vec) (Data vec1) = Data $ f vec vec1


happly' :: (Data inst vec b -> c) -> HRecord key inst label vec a b -> Map.Map key c
happly' f (HRecord _ m) = Map.map f $ m 

mapData :: 
  (DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec a) => 
  (a -> b) ->  (Data inst vec a) -> (Data inst vec b)
mapData f (Data vec) = Data $ DV.map f vec

map :: 
  (DV.Walker vec,
   DV.Storage vec c,
   DV.Storage vec b) =>
  (b -> c) -> Signal inst label vec a b -> Signal inst label vec a c
map f signal = apply (DV.map f) signal

zipWith ::
  (DV.Zipper vec,
   DV.Storage vec b,
   DV.Storage vec c,
   DV.Storage vec d) =>  
  (b -> c -> d) ->
  Signal inst label vec a b ->
  Signal inst label vec a c ->
  Signal inst label vec a d
zipWith f signal signal1 = apply2 (DV.zipWith f) signal signal1

zipWithData ::  
  (DV.Zipper vec,
   DV.Storage vec c,
   DV.Storage vec b,
   DV.Storage vec a) =>
  (a -> b -> c) ->
  Data inst vec a ->
  Data inst vec b ->
  Data inst vec c
zipWithData f dat dat1 = apply2Data (DV.zipWith f) dat dat1

foldl :: 
  (DV.Walker vec, DV.Storage vec b) => 
  (acc -> b -> acc) ->
  acc -> Signal inst label vec a b -> acc
foldl f acc signal = applyOut (DV.foldl f acc) signal


foldlWithTime :: 
  (DV.Walker vec, 
   DV.Storage vec (a, b), 
   DV.Zipper vec,
   DV.Storage vec b,
   DV.Storage vec a, 
   DV.Storage vec (TimeStep a, b), 
   DV.Storage vec (TimeStep a)) => 
  (acc -> (TimeStep a,b) -> acc) ->
  acc -> Signal inst label vec a b -> acc
foldlWithTime f acc (Signal (Strict.Axis _ _ timeVec) (Data vec)) = DV.foldl f acc $ DV.zip timeVec vec


-- | delivers left border of sign sections
locateSignChanges :: 
  (DV.Walker vec,
   Ord Strict.Idx,
   DV.Length vec,
    DV.Len (vec b),
 DV.Storage vec b,
 Ord b,
 Arith.Constant b)=>
 Caller -> 
 Data inst vec b ->
 NonEmptySet.T Strict.Idx
locateSignChanges caller (Data vec)  = NonEmptySet.fromList indexList
 where
    err = merror caller modul "locateSignChanges" "empty Signal"
    indexList = (\(_,_,l) -> l) $ Maybe.fromMaybe err $  DV.foldl f Nothing vec
    f Nothing x = Just (0,Arith.sign x,NonEmpty.Cons (Strict.Idx 0) []) 
    f (Just (idx,sgn,lst)) x = if sgn /= Arith.sign x 
                         then Just (idx+1,Arith.sign x, NonEmpty.appendRight lst [Strict.Idx $ idx+1])
                         else Just (idx+1,sgn,lst)    


concatEvenEvenTimeShare :: 
  (DV.Zipper vec, 
   Constant a, 
   DV.Storage vec [a], DV.Storage vec b, 
   DV.Singleton vec,
   DV.FromList vec,
   DV.Walker vec, 
   DV.Storage vec [b],DV.Storage vec [TimeStep a],DV.Storage vec (TimeStep a),
   DV.Storage vec a) => 
  Signal inst label vec a [b] -> Signal inst1 label vec a b
concatEvenEvenTimeShare (Signal (Strict.Axis label typ timeVec) (Data vec)) = 
  Signal (Strict.Axis label typ  $ concatAlt newTime) (Data $ concatAlt2 vec)
  where newTime = DV.zipWith f timeVec vec
        f (TimeStep t dt) xs = let 
          n = P.length xs
          dtVec = replicate n (dt Arith.~/ (Arith.fromInteger $ fromIntegral n))
          in P.zipWith TimeStep (P.foldl (\acc x -> acc ++ [last acc Arith.~+ x]) [t] dtVec) dtVec
          -- TODO berechnung der Zeitmittenwerte prüfen      
        concatAlt = DV.foldl (\ acc x -> DV.append acc (DV.fromList x)) (DV.fromList []) 
        concatAlt2 = DV.foldl (\ acc x -> DV.append acc (DV.fromList x)) (DV.fromList []) 

len ::
 DV.Len (vec b) =>
 Signal inst label vec a b ->
 Int     
len sig = applyOut DV.len sig 



negate :: 
  (DV.Walker vec, DV.Storage vec b,Sum b) => 
  Signal inst label vec a b -> Signal inst label vec a b
negate sig = map (Arith.negate) sig 


negateData :: (DV.Walker vec, DV.Storage vec a, Sum a) => Data inst vec a -> Data inst vec a 
negateData dat = mapData (Arith.negate) dat

foldlData :: 
  (DV.Walker vec, DV.Storage vec a) => 
  (acc -> a -> acc) ->
  acc -> Data inst vec a -> acc
foldlData f acc dat = applyOutData (DV.foldl f acc) dat


sumData :: (DV.Walker vec, DV.Storage vec a, Sum a, Constant a) =>  Data inst vec a -> a
sumData dat = foldlData (Arith.~+) Arith.zero dat


deltaData :: 
  (DV.Zipper vec,
   DV.Storage vec a,
   DV.Singleton vec, 
   Sum a) =>
  Data inst vec a -> Data inst vec a 
deltaData (Data dat) = Data $ DV.deltaMap (Arith.~-) dat


signalMap2HRecord :: 
  Caller ->
  Map.Map key (Signal inst label vec a b) ->
  HRecord key inst label vec a b
signalMap2HRecord caller m = HRecord t $ Map.map getData m
 where
   ((Signal t _),_) = Maybe.fromMaybe err $ Map.minView m
   err = merror caller modul "signalMap2HRecord" "empty signal map"
   

-- TODO -- make sure this Integration only is applied to energy signals
instance  (Constant a, DV.Walker vec, DV.Storage vec a) => Arith.Integrate (Data inst vec a) where
   type Scalar (Data inst vec a) = a
   integrate = sumData
   
instance (DV.Zipper vec, DV.Storage vec a, Sum a, DV.Walker vec) => Arith.Sum (Data inst vec a) where
  x ~+ y = zipWithData (~+) x y
  {-# INLINE (~+) #-}
  
  x ~- y = zipWithData (~-) x y
  {-# INLINE (~-) #-}

  negate x = mapData Arith.negate x
  {-# INLINE negate #-}

instance (DV.Zipper vec, DV.Storage vec a, Sum a, Product a, DV.Walker vec,Constant a) =>
         Arith.Product (Data inst vec a) where
  x ~* y =  zipWithData (~*) x y
  {-# INLINE (~*) #-}

  x ~/ y =  zipWithData (~/) x y
  {-# INLINE (~/) #-}

  recip x = mapData Arith.recip x
  {-# INLINE recip #-}

  constOne x = mapData (\_ -> Arith.one) x
  {-# INLINE constOne #-}

instance 
  (Eq a,
   DV.Zipper vec,
   Product a,
   Constant a,
   DV.Storage vec a,
   DV.Singleton vec,
   DV.Storage vec (a, a)) => 
  Arith.ZeroTestable (Data inst vec a) where
  allZeros (Data x) = DV.all (==Arith.zero) x  
  coincidingZeros (Data x) (Data y) = DV.any (\(a,b)-> a==Arith.zero && b==Arith.zero) $ DV.zip x y
  
  
getDataSlice :: (DV.Storage vec a, DV.Slice vec) =>
  Strict.Range ->  
  Data inst vec a -> 
  Data inst vec a
getDataSlice  (Strict.Range (Strict.Idx startIdx) (Strict.Idx endIdx)) (Data vec) = 
    Data $ DV.slice startIdx (endIdx-startIdx+1) vec
 


replicateSamples :: 
  (DV.Storage vec b, 
   DV.Singleton vec, 
   DV.Storage vec (vec b), 
   DV.FromList vec, 
   DV.Zipper vec, 
   DV.Storage vec Int) =>
  Signal inst label vec a Int ->
  Signal inst label vec a b ->
  Signal inst1 label vec a b
replicateSamples (Signal time (Data x)) (Signal _ (Data y)) = 
  Signal (Strict.newInstance time) $ 
   Data $ DV.concat $ DV.toList $ DV.zipWith DV.replicate x y 

{-    

interp :: 
  (Eq a, Show a, Arith.Product a, Arith.Constant a,Show label,
   Ord a,
   DV.Storage vec a,
   DV.LookupUnsafe vec a,
   DV.Length vec,
   DV.Find vec) =>
  Caller -> 
  Interp.Method a -> 
  Interp.ExtrapMethod a -> 
  Signal inst label vec a (Interp.Val a) ->
  a ->  
  (Interp.Val a)
interp caller inmethod exmethod signal x = case x of 
  Interp.Invalid xs -> Interp.Invalid xs
  _  -> result
  where
    result = Interp.dim1 (caller |> nc "interpolate") inmethod exmethod (show label) (x1, x2) (y1, y2) x
    label = Strict.getLabel $ getTime signal
    ((idx1,idx2),(x1,x2)) = Strict.getSupportPoints (getTime signal) (Interp.unpack x)
    y1 = lookupUnsafe signal idx1
    y2 = lookupUnsafe signal idx2
-}   