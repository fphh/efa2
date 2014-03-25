{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.ND.Cube.Grid where

--import qualified Prelude as P
--import Prelude hiding (map)

import EFA.Utility(Caller,ModuleName(..),(|>),FunctionName, genCaller)
import qualified EFA.Signal.Vector as SV

import qualified EFA.Data.ND as ND 
import qualified EFA.Data.Axis as Axis 
--import EFA.Data.Axis(Strict(..))

import qualified Data.Map as Map

m :: ModuleName
m = ModuleName "Grid"

nc :: FunctionName -> Caller
nc = genCaller m

-- newtype Idx = Idx {getInt :: Int} deriving Show

type Grid dim label vec a = ND.Data dim (Axis.Strict label vec a)


type DimIdx dim = ND.Data dim Axis.Idx

newtype LinIdx = LinIdx {getInt:: Int} deriving Show

dimensionMultiplicators :: 
  (SV.Storage vec a, SV.Length vec) => 
  Grid dim label vec a -> ND.Data dim Int
dimensionMultiplicators (ND.Data axes) = ND.Data $ (init $ map Axis.len axes) ++ [1]

toLinear :: 
 (SV.Storage vec a, SV.Length vec)=>  
  Grid dim label vec a -> DimIdx dim -> LinIdx
toLinear axes (ND.Data indices) = LinIdx $
  foldl (+) (0) $ zipWith (*) 
  (map Axis.getInt indices) (ND.toList $ dimensionMultiplicators axes)
        
fromLinear :: 
  (SV.Storage vec a, SV.Length vec) =>
  Grid dim label vec a -> LinIdx -> DimIdx dim
fromLinear axes (LinIdx idx) = ND.Data $ (map Axis.Idx . snd) $ 
                               foldl f (idx,[]) $ ND.toList $ dimensionMultiplicators axes
  where f (rest,list) x = (mod rest x,list++[div rest x])

create :: 
  (ND.Dimensions dim,
   Ord a,
   SV.Zipper vec,
   SV.Storage vec a,
   SV.Storage vec Bool,
   SV.Singleton vec) =>
            Caller -> [(label,vec a)] -> Grid dim label vec a
create caller xs = ND.fromList newCaller 
                         $ map (\(label,vec) -> Axis.fromVec newCaller label vec) xs
  where newCaller = caller |> (nc "create")


-- | generate a vector als linear listing of all coordinates in a grid  
toVector:: 
  (SV.Walker vec,
   SV.Storage vec (ND.Data dim a),
   SV.Singleton vec,
   SV.Storage vec [a],
   SV.Storage vec a, 
   SV.Storage vec (vec [a]), 
   SV.FromList vec) => 
  Grid dim label vec a -> 
  vec (ND.Data dim a)
toVector axs = SV.map ND.Data $ g axs   
  where   
    g (ND.Data [Axis.Strict _ axis]) = SV.map (\x -> [x]) $ axis
    g (ND.Data axes) = 
      SV.concat $ SV.toList $ SV.map (\x -> SV.map (\xs -> x:xs) vec) axis
      where axis = Axis.getVec $ head axes
            vec = g $ (ND.Data $ tail axes)

-- | Get Sizes of alle Axes
sizes :: 
  (SV.Storage vec a, SV.Length vec) => 
  Grid dim label vec a -> (ND.Data dim Int)
sizes (ND.Data axes) = ND.Data $ map Axis.len axes


-- | Remove axes of specified dimensions 
extract ::
  Caller ->
  Grid dim label vec a -> 
  ND.Data dim2 ND.Idx ->  
  Grid dim2 label vec a
extract caller grid dims = ND.map f dims  
  where f dim = ND.lookup (caller |> nc "extract") grid dim
        
-- | Generate a complete index room, but restrain index for dimension to be reduced to the specified value 
reductionIndexVector :: 
  (SV.Walker vec,SV.Storage vec LinIdx,SV.Length vec,
   SV.Storage vec (vec [Axis.Idx]),
   SV.Storage vec [Axis.Idx],
   SV.Storage vec (ND.Data dim Axis.Idx),
   SV.Singleton vec,
   SV.Storage vec Axis.Idx,
   SV.Storage vec a,
   SV.FromList vec) => 
  Grid dim label vec a -> Map.Map ND.Idx Axis.Idx -> vec LinIdx
reductionIndexVector axes dimensionsToReduce = SV.map (toLinear axes) $ toVector $ ND.imap f axes
  where f dim axis@(Axis.Strict l _) = case Map.lookup dim dimensionsToReduce of  
          Just index -> Axis.Strict l $ SV.fromList $ [index]
          Nothing -> Axis.imap (\index _ -> index) axis