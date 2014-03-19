{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EFA.Signal.Map.Dimension where

import EFA.Utility(Caller)

{-
newtype Data ab c = Data {getData :: Apply ab c}

data Nil a
data ((a :: * -> *)  :>  (b :: * -> *)) c

infixr 9 :>

-- | apply nested functor to the functor argument
type family Apply (a :: * -> *) c

type instance Apply Nil a = a
type instance Apply (a :> b) c = a (Apply b c)
-}


data Dim1
data Succ a

type Dim2 = Succ Dim1
type Dim3 = Succ Dim2
type Dim4 = Succ Dim3
type Dim5 = Succ Dim4
type Dim6 = Succ Dim5
type Dim7 = Succ Dim6


data Data dim a = Data [a] deriving Show

instance Functor (Data dim) where
  fmap f (Data xs) = Data (fmap f xs) 

one :: a -> Data Dim1 a
one x = Data [x]

append ::  Data dim a -> a -> Data (Succ dim) a
append (Data xs) x = Data (xs++[x])

class FromList dim where
 fromList :: Caller -> [a] -> Data dim a
 
instance FromList Dim1 where 
  fromList caller xs = if length xs == 1 then (Data xs) else error (m caller) 

instance FromList Dim2 where 
  fromList caller xs = if length xs == 2 then (Data xs) else error (m caller) 

instance FromList Dim3 where 
  fromList caller xs = if length xs == 3 then (Data xs) else error (m caller) 

instance FromList Dim4 where 
  fromList caller xs = if length xs == 4 then (Data xs) else error (m caller) 

instance FromList Dim5 where 
  fromList caller xs = if length xs == 5 then (Data xs) else error (m caller) 

instance FromList Dim6 where 
  fromList caller xs = if length xs == 5 then (Data xs) else error (m caller) 

m caller = "Error in Dimension.fromList called by " ++ caller ++ 
           " list length doesn't match dimension" 


dropFirst :: Caller -> Data (Succ dim) a -> Data dim a 
dropFirst caller (Data [x]) = error $ "Error in Dimension.dropFirst called by " ++ 
                         caller ++ "-only one Dimension left"
dropFirst _ (Data (x:xs)) = Data xs

getFirst :: Caller -> Data dim a -> a 
getFirst caller (Data [x]) = error $ "Error in Dimension.dropFirst called by " ++ 
                         caller ++ "-only one Dimension left" 
getFirst _ (Data (x:xs)) = x