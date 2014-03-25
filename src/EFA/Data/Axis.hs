{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.Axis where

import EFA.Utility(Caller,merror, ModuleName(..),FunctionName, genCaller)
import qualified EFA.Signal.Vector as SV



m :: ModuleName
m = ModuleName "Axis"

nc :: FunctionName -> Caller
nc = genCaller m

-- | Datatype with monotonically rising values 
data Strict label vec a = Strict {
  getLabel :: label,
  getVec :: vec a} deriving (Show,Eq)

newtype Idx = Idx {getInt :: Int} deriving Show

imap :: 
  (SV.Walker vec,
   SV.Storage vec b,
   SV.Storage vec a) => 
  (Idx -> a -> b) -> Strict label vec a -> Strict label vec b 
imap f (Strict label vec) = Strict label $ SV.imap (f . Idx) vec 

indexAdd :: Idx -> Int -> Idx
indexAdd (Idx idx) num = Idx $ (idx+num) 

len ::
  (SV.Storage vec a, SV.Length vec)=>
  Strict label vec a -> Int
len (Strict _ vec) = SV.length vec

fromVec :: 
  (SV.Storage vec Bool, SV.Singleton vec, 
   SV.Zipper vec, SV.Storage vec a,Ord a) =>
  Caller -> label -> vec a -> Strict label vec a
fromVec caller label vec = 
  if isMonoton then Strict label vec
  else merror caller m "fromVec" "Vector of elements is not monotonically rising"   
    where isMonoton = SV.all (==True) $ SV.deltaMap (\ x1 x2 -> x2 > x1) vec
          
findIndex :: 
  (SV.Storage vec a, SV.Find vec)=>
  (a -> Bool) -> Strict label vec a -> Maybe Idx
findIndex f (Strict _ vec) = fmap Idx $ SV.findIndex f vec


lookupUnsafe :: 
  SV.LookupUnsafe vec a => 
  Strict label vec a -> Idx -> a
lookupUnsafe (Strict _ axis) (Idx idx) = SV.lookupUnsafe axis idx 

findRightInterpolationIndex :: 
  (SV.Storage vec a, SV.Find vec, Ord a, SV.Length vec) =>
  Strict label vec a -> a -> Idx
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
   SV.Storage vec a,
   SV.Length vec,
   SV.Find vec,
   SV.LookupUnsafe vec a) =>
  Strict label vec a ->  a -> ((Idx,Idx),(a,a))
getSupportPoints axis x = ((leftIndex,rightIndex),
                                 (lookupUnsafe axis leftIndex, lookupUnsafe axis rightIndex))
  where rightIndex = findRightInterpolationIndex axis x
        leftIndex = indexAdd rightIndex (-1)  