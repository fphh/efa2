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

import EFA.Utility.Trace(mytrace)

import qualified Data.Map as Map
--import qualified Data.List as List


modul :: ModuleName
modul = ModuleName "Grid"

nc :: FunctionName -> Caller
nc = genCaller modul

-- newtype Idx = Idx {getInt :: Int} deriving Show

type Grid inst dim label vec a = ND.Data dim (Strict.Axis inst label vec a)


--instance Ref.ToData Grid where
--  toData (Grid vec) = Ref.SingleData "Grid" $ toData vec

type DimIdx dim = ND.Data dim Strict.Idx

newtype LinIdx = LinIdx {getInt:: Int} deriving (Show,Eq)

-- | dimension Multiplicators help to convert from dimensional to linear index
dimensionMultiplicators ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid inst dim label vec a -> ND.Data dim Int
dimensionMultiplicators (ND.Data grid) = ND.Data $ foldr f [1::Int] (init $ map Strict.len grid)
  where f x acc = [x*(head acc)]++acc

linearLength :: 
  (DV.Storage vec a, DV.Length vec) => 
  Grid inst dim label vec a -> Int
linearLength (ND.Data grid) =  foldl (*) 1 $ map Strict.len $ grid

-- | Converts a dimensional index to a linear Index
toLinear ::
 (DV.Storage vec a, DV.Length vec)=>
  Grid inst dim label vec a -> DimIdx dim -> LinIdx
toLinear grid (ND.Data indices) = LinIdx $
  foldl (+) (0) $ zipWith (*)
  (map Strict.getInt indices) (ND.toList $ dimensionMultiplicators grid)

fromLinear ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid inst dim label vec a -> LinIdx -> DimIdx dim
fromLinear grid (LinIdx idx) = ND.Data $ (map Strict.Idx . snd) $
                               foldl f (idx,[]) $ ND.toList $ dimensionMultiplicators grid
  where f (rest,list) x = (mod rest x,list++[div rest x])

-- | Build a Grid
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
    g (ND.Data grid) =
      DV.concat $ DV.toList $ DV.map (\x -> DV.map (\xs -> x:xs) vec) axis
      where axis = Strict.getVec $ head grid
            vec = g $ (ND.Data $ tail grid)

-- | Get Sizes of alle Axes
sizes ::
  (DV.Storage vec a, DV.Length vec) =>
  Grid inst dim label vec a -> (ND.Data dim Int)
sizes (ND.Data grid) = ND.Data $ map Strict.len grid


getAxis :: Caller -> Grid inst dim label vec a -> ND.Idx -> Strict.Axis inst label vec a
getAxis caller grid dimIdx = ND.lookup (caller |> nc "getAxis") grid dimIdx

-- | Extract grid of specified dimensions
extract ::
  Caller ->
  Grid inst dim label vec a ->
  ND.Data dim2 ND.Idx ->
  Grid inst2 dim2 label vec a
extract caller grid dims = ND.map f dims
  where f dim = Strict.newInstance $ getAxis (caller |> nc "extract") grid dim


-- | Generate a complete index room, but restrain index for dimension to be reduced to the specified value
reductionIndexVector ::
  (DV.Walker vec,
   DV.Storage vec LinIdx,
   DV.Length vec,
   DV.Storage vec (vec [Strict.Idx]),
   DV.Storage vec [Strict.Idx],
   DV.Storage vec (ND.Data dim Strict.Idx),
   DV.Singleton vec,
   DV.Storage vec Strict.Idx,Show (vec (ND.Data dim Strict.Idx)),Show label, Show (vec Strict.Idx),
   DV.Storage vec a,
   Show (vec LinIdx),
   DV.FromList vec) =>
  Grid inst dim label vec a ->
  Map.Map ND.Idx Strict.Idx ->
  vec LinIdx
reductionIndexVector grid location = linearIndexVector
  where
  -- | convert the dimensional indexing to linear indexing, related to the original cube
    linearIndexVector =  DV.map (toLinear grid) $ reducedIndexGridLinear
    -- | create a Grid with selected indices for the chosen dimensions, which are reduced == location and
    -- | convert it to a linear Vector
    reducedIndexGridLinear =  toVector $ ND.imap f grid
    f dim axis@(Strict.Axis label typ _) = case Map.lookup dim location of
          Just index -> Strict.Axis label typ $ DV.fromList $ [index]
          Nothing -> Strict.imap (\index _ -> index) axis


genExtractList::
  (ND.Dimensions dim,
   DV.Storage vec Strict.Idx,
   DV.Walker vec,
   DV.Storage vec a,
   DV.FromList vec) =>
  Caller ->
  Grid inst dim label vec a  ->
  ND.Data dim2 (ND.Idx) ->
  [Map.Map ND.Idx Strict.Idx]
genExtractList caller grid dims2Keep = mytrace 1 "grid" "genExtractList" $ map Map.fromList $ permute xs
  where
    xs = map f $ ND.getDims2Drop grid dims2Keep
    f dimIdx = (dimIdx, DV.toList $ Strict.getVec $
                            Strict.imap (\ i _ -> i) $
                            getAxis (caller |> nc "genExtractList") grid dimIdx)


permute:: [(ND.Idx,[Strict.Idx])] -> [[(ND.Idx,Strict.Idx)]]
permute xss = mytrace 1 "grid" "permute" $ foldl f [] xss
 where f [] (dimIdx, axIndices) = map (:[]) $ zip (repeat dimIdx) axIndices
       f xs (dimIdx, axIndices) = concat $ map (\ newItem -> map (++ [newItem]) xs) $ zip (repeat dimIdx) axIndices
       
{-       
TODO:
haveNoCommonAxes :: (Eq label) =>
  Grid inst dim label vec a  -> 
  Grid inst1 dim1 label vec1 a1  ->
  Bool
haveNoCommonAxes grid grid1 = (List.intersect (f grid) (f grid1) == []) 
  where f gr = ND.toList $ ND.map (Strict.getLabel) gr
-}

getSupportingPoints :: 
  (Ord a,
   DV.Storage vec a,
   DV.LookupUnsafe vec a,
   DV.Length vec,
   DV.Find vec)=>
  Caller ->
  Grid inst dim label vec a ->
  ND.Data dim a ->
  ND.Data dim (Strict.SupportingPoints  (Strict.Idx,a))
getSupportingPoints caller grid coordinates = 
  ND.zipWith (Strict.getSupportPoints2 (caller |> nc "getSupportingPoints")) grid coordinates


getSupportingPointLinearIndices :: 
  (DV.Storage vec a, DV.Length vec, ND.Dimensions dim) => 
  Caller -> 
  Grid inst dim label vec a ->
  ND.Data dim (Strict.SupportingPoints (Strict.Idx,a)) -> 
  [LinIdx]
getSupportingPointLinearIndices caller grid supp = convertToLinear $ foldl f [] $ ND.toList supp
  where 
    f acc (Strict.PairOfPoints (idx,_) (idx1,_)) = mapIndex acc idx ++ mapIndex acc idx1 
    f acc (Strict.LeftPoint (idx,_)) = mapIndex acc idx
    f acc (Strict.RightPoint (idx,_)) = mapIndex acc idx
    
    mapIndex [] idx = [[idx]]
    mapIndex acc idx = map (++[idx]) acc 
    
    convertToLinear acc = map (toLinear grid . ND.fromList (caller |> nc "supportingPoints2ND")) acc


data ExtractInfo dim = All | Edges | EdgesAndStep Int | Linear [LinIdx] | Dim [DimIdx dim]  | ReduceDims (Map.Map ND.Idx [Strict.Idx])
  
extractIndexList ::
  (DV.Storage vec a,
   DV.Length vec,
   ND.Dimensions dim) =>
  Caller ->
  Grid inst dim label vec a ->
  ExtractInfo dim ->
 [LinIdx] 
extractIndexList _ grid All = map LinIdx [0..(linearLength grid-1)]
extractIndexList caller grid Edges = map (toLinear grid) $ map (ND.fromList (caller |> nc "extractIndexList")) 
                                $ sequence $ map (\x->[Strict.Idx 0, Strict.Idx x]) $ ND.toList $ sizes grid
extractIndexList caller grid (EdgesAndStep st) = map (toLinear grid) $ map (ND.fromList  (caller |> nc "extractIndexList")) 
                                            $ sequence $ map (\x->map Strict.Idx [0,st .. x]) $ ND.toList $ sizes grid
extractIndexList _ _ (Linear xs) = xs 
extractIndexList _ grid (Dim xs) = map (toLinear grid) xs 
extractIndexList caller grid (ReduceDims m) = map (toLinear grid) $ map (ND.fromList  (caller |> nc "extractIndexList")) 
                                            $ sequence $ ND.toList $ ND.imap f grid
  where f idx axis = case Map.lookup idx m of  
          (Just idxList) -> idxList
          Nothing -> map Strict.Idx [0 .. (Strict.len axis -1)] 
