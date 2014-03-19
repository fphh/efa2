{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module EFA.Signal.Map.Dimension where

import EFA.Utility(Caller)

data Dim1
data Succ a

-- | apply nested functor to the functor argument
type family SubDim a

type instance SubDim Dim1 = Dim1
type instance SubDim (Succ a) = a

type Dim2 = Succ Dim1
type Dim3 = Succ Dim2
type Dim4 = Succ Dim3
type Dim5 = Succ Dim4
type Dim6 = Succ Dim5
type Dim7 = Succ Dim6  
type Dim8 = Succ Dim7  
type Dim9 = Succ Dim8  
type Dim10 = Succ Dim9  
  
data Data dim a = Data [a] deriving Show

instance Functor (Data dim) where
  fmap f (Data xs) = Data (fmap f xs) 

len :: Data dim a -> Int
len (Data xs) = length xs

fromList :: Dimensions dim => Caller -> [a] -> Data dim a
fromList caller xs = 
  let 
    dim = Data xs 
  in if len dim == num dim 
     then dim
     else error ("Error in Dimension.fromList called by " ++ caller ++ 
           " list length doesn't match dimension") 

class Dimensions dim where num :: Data dim a -> Int

instance Dimensions Dim1 where num _ = 1 
instance Dimensions Dim2 where num _ = 2 
instance Dimensions Dim3 where num _ = 3 
instance Dimensions Dim4 where num _ = 4 
instance Dimensions Dim5 where num _ = 5 
instance Dimensions Dim6 where num _ = 6 
instance Dimensions Dim7 where num _ = 7 
instance Dimensions Dim8 where num _ = 8 
instance Dimensions Dim9 where num _ = 9 
instance Dimensions Dim10 where num _ = 10 


dropFirst :: Caller -> Data dim a -> Data (SubDim dim) a 
dropFirst caller (Data []) = error $ "Error in Dimension.dropFirst called by " ++ 
                         caller ++ "- no Dimension left"
dropFirst _ (Data (x:xs)) = Data xs

getFirst :: Caller -> Data dim a -> a 
getFirst caller (Data []) = error $ "Error in Dimension.dropFirst called by " ++ 
                         caller ++ "- no Dimension left" 
getFirst _ (Data (x:xs)) = x