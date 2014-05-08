{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.ND.Cube.Grid where

--import qualified Prelude as P
--import Prelude hiding (map)

import EFA.Utility(Caller,ModuleName(..),(|>),FunctionName, genCaller)
import qualified EFA.Data.Vector as DV

import qualified EFA.Data.ND as ND
import qualified EFA.Data.Axis.Strict as Axis
--import EFA.Data.Axis(Strict(..))

import qualified Data.Map as Map

m :: ModuleName
m = ModuleName "Grid"

nc :: FunctionName -> Caller
nc = genCaller m

-- newtype Idx = Idx {getInt :: Int} deriving Show

type Grid typ dim label vec a = ND.Data dim (Axis.Axis typ label vec a)


type DimIdx dim = ND.Data dim Axis.Idx

newtype LinIdx = LinIdx {getInt:: Int} deriving (Show,Eq)

dimensionMultiplicators ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid typ dim label vec a -> ND.Data dim Int
dimensionMultiplicators (ND.Data axes) = ND.Data $ (init $ map Axis.len axes) ++ [1]

toLinear ::
 (DV.Storage vec a, DV.Length vec)=>
  Grid typ dim label vec a -> DimIdx dim -> LinIdx
toLinear axes (ND.Data indices) = LinIdx $
  foldl (+) (0) $ zipWith (*)
  (map Axis.getInt indices) (ND.toList $ dimensionMultiplicators axes)

fromLinear ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid typ dim label vec a -> LinIdx -> DimIdx dim
fromLinear axes (LinIdx idx) = ND.Data $ (map Axis.Idx . snd) $
                               foldl f (idx,[]) $ ND.toList $ dimensionMultiplicators axes
  where f (rest,list) x = (mod rest x,list++[div rest x])

create ::
  (ND.Dimensions dim,
   Ord a,
   DV.Zipper vec,
   DV.Storage vec a,
   DV.Storage vec Bool,
   DV.Singleton vec) =>
            Caller -> [(label,vec a)] -> Grid typ dim label vec a
create caller xs = ND.fromList newCaller
                         $ map (\(label,vec) -> Axis.fromVec newCaller label vec) xs
  where newCaller = caller |> (nc "create")


-- | generate a vector als linear listing of all coordinates in a grid
toVector::
  (DV.Walker vec,
   DV.Storage vec (ND.Data dim a),
   DV.Singleton vec,
   DV.Storage vec [a],
   DV.Storage vec a,
   DV.Storage vec (vec [a]),
   DV.FromList vec) =>
  Grid typ dim label vec a ->
  vec (ND.Data dim a)
toVector axs = DV.map ND.Data $ g axs
  where
    g (ND.Data [Axis.Axis _ axis]) = DV.map (\x -> [x]) $ axis
    g (ND.Data axes) =
      DV.concat $ DV.toList $ DV.map (\x -> DV.map (\xs -> x:xs) vec) axis
      where axis = Axis.getVec $ head axes
            vec = g $ (ND.Data $ tail axes)

-- | Get Sizes of alle Axes
sizes ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid typ dim label vec a -> (ND.Data dim Int)
sizes (ND.Data axes) = ND.Data $ map Axis.len axes


-- | Remove axes of specified dimensions
extract ::
  Caller ->
  Grid typ dim label vec a ->
  ND.Data dim2 ND.Idx ->
  Grid typ dim2 label vec a
extract caller grid dims = ND.map f dims
  where f dim = ND.lookup (caller |> nc "extract") grid dim

-- | Generate a complete index room, but restrain index for dimension to be reduced to the specified value
reductionIndexVector ::
  (DV.Walker vec,DV.Storage vec LinIdx,DV.Length vec,
   DV.Storage vec (vec [Axis.Idx]),
   DV.Storage vec [Axis.Idx],
   DV.Storage vec (ND.Data dim Axis.Idx),
   DV.Singleton vec,
   DV.Storage vec Axis.Idx,
   DV.Storage vec a,
   DV.FromList vec) =>
  Grid typ dim label vec a -> Map.Map ND.Idx Axis.Idx -> vec LinIdx
reductionIndexVector axes dimensionsToReduce = DV.map (toLinear axes) $ toVector $ ND.imap f axes
  where f dim axis@(Axis.Axis l _) = case Map.lookup dim dimensionsToReduce of
          Just index -> Axis.Axis l $ DV.fromList $ [index]
          Nothing -> Axis.imap (\index _ -> index) axis