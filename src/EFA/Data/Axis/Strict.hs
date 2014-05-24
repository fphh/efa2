{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module EFA.Data.Axis.Strict where

import EFA.Utility(Caller,merror, ModuleName(..),FunctionName, genCaller)
import qualified EFA.Data.Vector as DV

import qualified EFA.Data.Axis as Axis
import qualified EFA.Value.Type as Type
import qualified EFA.Value as Value

import qualified EFA.Reference.Base as Ref

--import qualified Data.Map as Map

m :: ModuleName
m = ModuleName "Axis.Axis"

nc :: FunctionName -> Caller
nc = genCaller m

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

newtype Idx = Idx {getInt :: Int} deriving Show

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

indexAdd :: Idx -> Int -> Idx
indexAdd (Idx idx) num = Idx $ (idx+num)

len ::
  (DV.Storage vec a, DV.Length vec)=>
  Axis inst label vec a -> Int
len (Axis _ _ vec) = DV.length vec

fromVec ::
  (DV.Storage vec Bool, DV.Singleton vec,
   DV.Zipper vec, DV.Storage vec a,Ord a) =>
  Caller -> label -> Type.Dynamic -> vec a -> Axis inst label vec a
fromVec caller label typ vec =
  if isMonoton then Axis label typ vec
  else merror caller m "fromVec" "Vector of elements is not strict monotonically rising"
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
  Axis inst label vec a ->  a -> ((Idx,Idx),(a,a))
getSupportPoints axis x = ((leftIndex,rightIndex),
                                 (lookupUnsafe axis leftIndex, lookupUnsafe axis rightIndex))
  where rightIndex = findRightInterpolationIndex axis x
        leftIndex = indexAdd rightIndex (-1)