{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module EFA.Data.Axis.Strict where

import EFA.Utility(Caller,merror, ModuleName(..),FunctionName, genCaller)
import qualified EFA.Data.Vector as DV

import qualified EFA.Data.Axis as Axis
import qualified EFA.Value.Type as Type
import qualified EFA.Value as Value
import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Reference.Base as Ref

--import qualified Data.Map as Map

modul :: ModuleName
modul = ModuleName "Axis.Axis"

nc :: FunctionName -> Caller
nc = genCaller modul

-- | Datatype with strict monotonically rising values
-- | typ can be edge oder mid and relates to whether data is related to an intervall or an edge

data Axis inst label vec a = Axis {
  getLabel :: label,
  getType :: Type.Dynamic,
  getVec :: vec a} deriving (Show,Eq)

newInstance :: Axis inst label vec a -> Axis inst2 label vec a
newInstance (Axis label vec a) = Axis label vec a

instance (Show label,Ref.ToData (vec a)) =>
         Ref.ToData (Axis inst label vec a) where
  toData (Axis label typ vec) = Ref.TripleType "Axis"
                                (Ref.StringData "Label" (show label))
                                (Ref.StringData "Typ" (show typ))
                                (Ref.toData vec)

newtype Idx = Idx {getInt :: Int} deriving (Show,Ord,Eq)

data Range = Range Idx Idx deriving (Show,Eq)

instance Ref.ToData Idx where
  toData (Idx x) = Ref.StringData "Idx" (show x)

instance
  (Ord a,Type.GetDynamicType a,
   DV.Storage vec a,
   DV.Singleton vec, 
   DV.Length vec)
  => Axis.GetInfo Axis vec a where
  getLabel axis = getLabel axis
  getVector axis = getVec axis
  getType axis = getType axis
  getRange axis = (\(x,y) -> Value.Range x y) $ DV.minmax $ getVec axis
  getLength axis = len axis
  
imap ::
  (DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec a) =>
  (Idx -> a -> b) -> Axis inst label vec a -> Axis inst label vec b
imap f (Axis label typ vec) = Axis label typ $ DV.imap (f . Idx) vec

map ::
  (DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec a) =>
  (a -> b) -> Axis inst label vec a -> Axis inst label vec b
map f (Axis label typ vec) = Axis label typ $ DV.map f vec

indexAdd :: Idx -> Int -> Idx
indexAdd (Idx idx) num = Idx $ (idx+num)

len ::
  (DV.Storage vec a, DV.Length vec)=>
  Axis inst label vec a -> Int
len (Axis _ _ vec) = DV.length vec

fromList :: 
  (Ord a, DV.Zipper vec, DV.Storage vec a, DV.Storage vec Bool,
   DV.Singleton vec, DV.FromList vec) =>
  Caller -> label -> Type.Dynamic -> [a] -> Axis inst label vec a
fromList caller label typ xs = fromVec caller label typ $ DV.fromList xs 

fromVec ::
  (DV.Storage vec Bool, DV.Singleton vec,
   DV.Zipper vec, DV.Storage vec a,Ord a) =>
  Caller -> label -> Type.Dynamic -> vec a -> Axis inst label vec a
fromVec caller label typ vec =
  if isMonoton then Axis label typ vec
  else merror caller modul "fromVec" "Vector of elements is not strict monotonically rising"
    where isMonoton = DV.all (==True) $ DV.deltaMap (\ x1 x2 -> x2 > x1) vec

findIndex ::
  (DV.Storage vec a, DV.Find vec)=>
  (a -> Bool) -> Axis inst label vec a -> Maybe Idx
findIndex f (Axis _ _ vec) = fmap Idx $ DV.findIndex f vec


lookupUnsafe ::
  DV.LookupUnsafe vec a =>
  Axis inst label vec a -> Idx -> a
lookupUnsafe (Axis _ _ vec) (Idx idx) = DV.lookupUnsafe vec idx

findRightInterpolationIndex ::
  (DV.Storage vec a, DV.Find vec, Ord a, DV.Length vec) =>
  Axis inst label vec a -> a -> Idx
findRightInterpolationIndex axis x = rightIndex
  where
    idx = findIndex (>x) axis
    rightIndex = case idx of
      Just (Idx ix) -> if ix==0 then Idx 1 else Idx ix
      Nothing   -> Idx $ (len axis)-1

-- | TODO -- Code ist wrong -- exact point hits have to be considered
-- | get all Points involved in the interpolation of a point on the given coordinates
getSupportPoints ::
  (Ord a,
   DV.Storage vec a,
   DV.Length vec,
   DV.Find vec,
   DV.LookupUnsafe vec a) =>
  Axis inst label vec a -> a -> ((Idx,Idx),(a,a))
getSupportPoints axis x = ((leftIndex,rightIndex),
                           (lookupUnsafe axis leftIndex, lookupUnsafe axis rightIndex))
  where rightIndex = findRightInterpolationIndex axis x
        leftIndex = indexAdd rightIndex (-1)

-- | delivers supporting points for interpolation
data SupportingPoints a = PairOfPoints a a | LeftPoint a | RightPoint a deriving Show

instance Functor SupportingPoints where
  fmap f (PairOfPoints x x1) = (PairOfPoints (f x) (f x1))
  fmap f (LeftPoint x) = LeftPoint (f x)
  fmap f (RightPoint x) = RightPoint (f x)

getSupportPoints2 :: 
  (Eq a,DV.LookupUnsafe vec a, 
   Ord a,
   DV.Storage vec a,
   DV.Length vec,
   DV.Find vec)=> 
  Caller -> Axis inst label vec a -> a -> SupportingPoints (Idx,a) 
getSupportPoints2 caller axis x = case (x==leftValue,x==rightValue) of  
  (False, False) -> PairOfPoints (leftIndex,leftValue) (rightIndex,rightValue)
  (True, False) -> LeftPoint (leftIndex,leftValue)
  (False,True) -> RightPoint (rightIndex,rightValue)
  (True,True) -> merror caller modul "getSupportPoints2" "axis not strictly monoton" 
  where rightIndex = findRightInterpolationIndex axis x
        leftIndex = indexAdd rightIndex (-1)
        leftValue = lookupUnsafe axis leftIndex
        rightValue = lookupUnsafe axis rightIndex




addLeft :: 
  (DV.Storage vec a, DV.Singleton vec,DV.FromList vec) => 
  [a] -> Axis inst label vec a  -> Axis inst label vec a     
addLeft xs (Axis label typ vec) = Axis label typ $ DV.append (DV.fromList xs) vec

addRight :: 
  (DV.Storage vec a, DV.Singleton vec,DV.FromList vec) => 
  Axis inst label vec a  -> [a] -> Axis inst label vec a     
addRight (Axis label typ vec) xs = Axis label typ $ DV.append vec $ DV.fromList xs

{- TODO: Add drops
dropLeft :: Int -> Axis inst label vec a  -> Axis inst label vec a     
dropLeft x (Axis label typ vec) = Axis label typ $ DV.drop x vec

dropRight :: [a] -> Axis inst label vec a  -> Axis inst label vec a     
dropRight xs (Axis label typ vec) = Axis label typ $ DV.take (DV.take x vec
-}

scale:: 
  (DV.Walker vec, DV.Storage vec a, Arith.Product a) => 
  a -> Axis inst label vec a -> Axis inst label vec a
scale x (Axis label typ vec) = (Axis label typ $ DV.map (Arith.~*x) vec)

offset:: 
  (DV.Walker vec, DV.Storage vec a, Arith.Sum a) => 
  a -> Axis inst label vec a -> Axis inst label vec a
offset x (Axis label typ vec) = (Axis label typ $ DV.map (Arith.~+x) vec)

flip :: (DV.Walker vec, DV.Storage vec a, Arith.Sum a, DV.Reverse vec) => Axis inst label vec a -> Axis inst label vec a
flip (Axis label typ vec) = (Axis label typ $ DV.map (Arith.negate) $ DV.reverse vec)

combine ::(DV.Storage vec a, DV.Singleton vec, Eq label, Ord a) => Caller -> Axis inst label vec a -> Axis inst label vec a -> Axis inst label vec a
combine caller (Axis label typ vec) (Axis label1 typ1 vec1) = let
  check1 = label == label1 && typ == typ1
  check2 = DV.last vec < DV.head vec1
  in case (check1,check2) of 
    (False,_) -> merror caller modul "combine" "Type or Label differ"
    (_,False) -> merror caller modul "combine" "Axes are overlapping"
    (_,True) -> Axis label typ (DV.append vec vec1)


modifyLabelWith :: (label -> label1) -> Axis inst label vec a -> Axis inst label1 vec a
modifyLabelWith f (Axis label typ vec) =  (Axis (f label) typ vec)


getSlice :: (DV.Storage vec a, DV.Slice vec) =>
  Range ->  
  Axis inst label vec a -> 
  Axis inst label vec a
getSlice  (Range (Idx startIdx) (Idx endIdx)) (Axis label typ vec) = 
    Axis label typ $ DV.slice startIdx (endIdx-startIdx) vec
