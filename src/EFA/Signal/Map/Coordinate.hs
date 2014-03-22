{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Map.Coordinate where

--import qualified Prelude as P
--import Prelude hiding (map)

import EFA.Utility(Caller,ModuleName(..),(|>),FunctionName, genCaller)
import qualified EFA.Signal.Vector as SV

import qualified EFA.Signal.Map.Dimension as Dim 
import qualified EFA.Signal.Map.Axis as Axis 
import EFA.Signal.Map.Axis(Axis(..))

import qualified Data.Map as Map

m :: ModuleName
m = ModuleName "Coordinate"

nc :: FunctionName -> Caller
nc = genCaller m

-- newtype Idx = Idx {getInt :: Int} deriving Show

type System dim label vec a = Dim.Data dim (Axis label vec a)
data Point dim a = Point (Dim.Data dim a) deriving Show

pointFromList :: [a] -> Point dim a
pointFromList xs = Point $ Dim.Data xs

pointtoList :: Point dim a -> [a]
pointtoList (Point (Dim.Data xs)) = xs

mapPoint :: (a -> b) -> Point dim a -> Point dim b
mapPoint f (Point (Dim.Data xs)) = Point $ Dim.Data $ map f xs


type DimIdx dim = Dim.Data dim Axis.Idx

newtype LinIdx = LinIdx {getInt:: Int} deriving Show

dimensionMultiplicators :: 
  (SV.Storage vec a, SV.Length vec) => 
  System dim label vec a -> Dim.Data dim Int
dimensionMultiplicators (Dim.Data axes) = Dim.Data $ (init $ map Axis.len axes) ++ [1]

toLinear :: 
 (SV.Storage vec a, SV.Length vec)=>  
  System dim label vec a -> DimIdx dim -> LinIdx
toLinear axes (Dim.Data indices) = LinIdx $
  foldl (+) (0) $ zipWith (*) 
  (map Axis.getInt indices) (Dim.toList $ dimensionMultiplicators axes)
        
fromLinear :: 
  (SV.Storage vec a, SV.Length vec) =>
  System dim label vec a -> LinIdx -> DimIdx dim
fromLinear axes (LinIdx idx) = Dim.Data $ (map Axis.Idx . snd) $ 
                               foldl f (idx,[]) $ Dim.toList $ dimensionMultiplicators axes
  where f (rest,list) x = (mod rest x,list++[div rest x])

createSystem :: 
  (Dim.Dimensions dim,
   Ord a,
   SV.Zipper vec,
   SV.Storage vec a,
   SV.Storage vec Bool,
   SV.Singleton vec) =>
            Caller -> [(label,vec a)] -> System dim label vec a
createSystem caller xs = Dim.fromList newCaller 
                         $ map (\(label,vec) -> Axis.fromVec newCaller label vec) xs
  where newCaller = caller |> (nc "createSystem")


-- | generate a vector als linear listing of all coordinates in a coordinate system  
vector:: 
  (SV.Walker vec,
   SV.Storage vec (Dim.Data dim a),
   SV.Singleton vec,
   SV.Storage vec [a],
   SV.Storage vec a, 
   SV.Storage vec (vec [a]), 
   SV.FromList vec) => 
  System dim label vec a -> 
  vec (Dim.Data dim a)
vector axs = SV.map Dim.Data $ g axs   
  where   
    g (Dim.Data [Axis _ axis]) = SV.map (\x -> [x]) $ axis
    g (Dim.Data axes) = 
      SV.concat $ SV.toList $ SV.map (\x -> SV.map (\xs -> x:xs) vec) axis
      where axis = getVec $ head axes
            vec = g $ (Dim.Data $ tail axes)

-- | Get Sizes of alle Axes
sizes :: 
  (SV.Storage vec a, SV.Length vec) => 
  System dim label vec a -> (Dim.Data dim Int)
sizes (Dim.Data axes) = Dim.Data $ map Axis.len axes


-- | Remove axes of specified dimensions 
extractSubSystem ::
  Caller ->
  System dim label vec a -> 
  Dim.Data dim2 Dim.Idx ->  
  System dim2 label vec a
extractSubSystem caller system dims = Dim.map f dims  
  where f dim = Dim.lookup (caller |> nc "extractSubSystem") system dim
        
-- | Generate a complete index room, but restrain index for dimension to be reduced to the specified value 
reductionIndexVector :: 
  (SV.Walker vec,SV.Storage vec LinIdx,SV.Length vec,
   SV.Storage vec (vec [Axis.Idx]),
   SV.Storage vec [Axis.Idx],
   SV.Storage vec (Dim.Data dim Axis.Idx),
   SV.Singleton vec,
   SV.Storage vec Axis.Idx,
   SV.Storage vec a,
   SV.FromList vec) => 
  System dim label vec a -> Map.Map Dim.Idx Axis.Idx -> vec LinIdx
reductionIndexVector axes dimensionsToReduce = SV.map (toLinear axes) $ vector $ Dim.imap f axes
  where f dim axis@(Axis l _) = case Map.lookup dim dimensionsToReduce of  
          Just index -> Axis l $ SV.fromList $ [index]
          Nothing -> Axis.imap (\index _ -> index) axis