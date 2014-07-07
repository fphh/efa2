module EFA.Data.OD.Signal where

-- import qualified Data.NonEmpty as NonEmpty
import qualified EFA.Data.Vector as DV

newtype SampleIdx = SampleIdx Int deriving Show
data Samples vec a = Samples (vec a) deriving Show

mapSamples :: 
  (DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec a) => 
  (a -> b) -> Samples vec a -> Samples vec b
mapSamples f (Samples xs) = Samples (DV.map f xs)  

zipWithSamples :: 
  (DV.Zipper vec,
   DV.Storage vec c,
   DV.Storage vec b,
   DV.Storage vec a) => 
  (a -> b -> c) -> Samples vec a -> Samples vec b -> Samples vec c
zipWithSamples f (Samples xs) (Samples ys) = Samples (DV.zipWith f xs ys)  