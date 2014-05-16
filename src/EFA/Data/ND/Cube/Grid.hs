{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.ND.Cube.Grid where

--import qualified Prelude as P
--import Prelude hiding (map)

import EFA.Utility(Caller,ModuleName(..),(|>),FunctionName, genCaller)
import qualified EFA.Data.Vector as DV
--import qualified EFA.Reference.Base as Ref
import qualified EFA.Value.Type as Type

import qualified EFA.Data.ND as ND
import qualified EFA.Data.Axis.Strict as Strict
--import EFA.Data.Axis(Strict(..))

import qualified Data.Map as Map


m :: ModuleName
m = ModuleName "Grid"

nc :: FunctionName -> Caller
nc = genCaller m

-- newtype Idx = Idx {getInt :: Int} deriving Show

type Grid inst dim label vec a = ND.Data dim (Strict.Axis inst label vec a)

--instance Ref.ToData Grid where
--  toData (Grid vec) = Ref.SingleData "Grid" $ toData vec

type DimIdx dim = ND.Data dim Strict.Idx

newtype LinIdx = LinIdx {getInt:: Int} deriving (Show,Eq)

dimensionMultiplicators ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid inst dim label vec a -> ND.Data dim Int
dimensionMultiplicators (ND.Data axes) = ND.Data $ (init $ map Strict.len axes) ++ [1]

toLinear ::
 (DV.Storage vec a, DV.Length vec)=>
  Grid inst dim label vec a -> DimIdx dim -> LinIdx
toLinear axes (ND.Data indices) = LinIdx $
  foldl (+) (0) $ zipWith (*)
  (map Strict.getInt indices) (ND.toList $ dimensionMultiplicators axes)

fromLinear ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid inst dim label vec a -> LinIdx -> DimIdx dim
fromLinear axes (LinIdx idx) = ND.Data $ (map Strict.Idx . snd) $
                               foldl f (idx,[]) $ ND.toList $ dimensionMultiplicators axes
  where f (rest,list) x = (mod rest x,list++[div rest x])

create ::
  (ND.Dimensions dim,
   Ord a,
   DV.Zipper vec,
   DV.Storage vec a,
   DV.Storage vec Bool,
   DV.Singleton vec) =>
            Caller -> [(label,Type.Dynamic,vec a)] -> Grid inst dim label vec a
create caller xs = ND.fromList newCaller
                         $ map (\(label,typ,vec) -> Strict.fromVec newCaller label typ vec) xs
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
  Grid inst dim label vec a ->
  vec (ND.Data dim a)
toVector axs = DV.map ND.Data $ g axs
  where
    g (ND.Data [Strict.Axis _ _ vec]) = DV.map (\x -> [x]) $ vec
    g (ND.Data axes) =
      DV.concat $ DV.toList $ DV.map (\x -> DV.map (\xs -> x:xs) vec) axis
      where axis = Strict.getVec $ head axes
            vec = g $ (ND.Data $ tail axes)

-- | Get Sizes of alle Axes
sizes ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid inst dim label vec a -> (ND.Data dim Int)
sizes (ND.Data axes) = ND.Data $ map Strict.len axes


-- | Remove axes of specified dimensions
extract ::
  Caller ->
  Grid inst dim label vec a ->
  ND.Data dim2 ND.Idx ->
  Grid inst dim2 label vec a
extract caller grid dims = ND.map f dims
  where f dim = ND.lookup (caller |> nc "extract") grid dim

-- | Generate a complete index room, but restrain index for dimension to be reduced to the specified value
reductionIndexVector ::
  (DV.Walker vec,DV.Storage vec LinIdx,DV.Length vec,
   DV.Storage vec (vec [Strict.Idx]),
   DV.Storage vec [Strict.Idx],
   DV.Storage vec (ND.Data dim Strict.Idx),
   DV.Singleton vec,
   DV.Storage vec Strict.Idx,
   DV.Storage vec a,
   DV.FromList vec) =>
  Grid inst dim label vec a -> Map.Map ND.Idx Strict.Idx -> vec LinIdx
reductionIndexVector axes dimensionsToReduce = DV.map (toLinear axes) $ toVector $ ND.imap f axes
  where f dim axis@(Strict.Axis label typ _) = case Map.lookup dim dimensionsToReduce of
          Just index -> Strict.Axis label typ $ DV.fromList $ [index]
          Nothing -> Strict.imap (\index _ -> index) axis
