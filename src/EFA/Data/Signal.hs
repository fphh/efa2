module EFA.Data.Signal where

import EFA.Utility(Caller,merror,ModuleName(..),FunctionName, genCaller)

import qualified EFA.Data.Axis.Mono as Mono

{-
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.ND as ND
import qualified EFA.Data.ND.Cube.Grid as Grid
import qualified EFA.Data.Axis.Strict as Axis
import EFA.Data.ND.Cube.Grid (Grid)
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Data(Nil,(:>))
import qualified EFA.Signal.Data as SD
import qualified Data.Map as Map  
-}

--import EFA.Signal.Interp as Interp
import EFA.Data.Vector as DV


import qualified Prelude as P
import Prelude hiding (zipWith, map, foldl)
--import Data.Maybe(fromMaybe) 

--import EFA.Utility.Trace(mytrace)

modul :: ModuleName
modul = ModuleName "Data.Signal"

nc :: FunctionName -> Caller
nc = genCaller modul

data Signal typ label vec t a = Signal {
  getTime :: Time typ label vec t, 
  getData :: Data typ vec a } deriving (Show,Eq)
        
newtype Data typ vec a = Data {getVec :: vec a} deriving (Show,Eq)

type Time typ label vec a = Mono.Axis typ label vec a 
                                
dataMap :: 
  (Walker vec, Storage vec b, Storage vec a) =>
  (a -> b) -> Data typ vec a -> Data typ vec b
dataMap f (Data vec) = Data $ DV.map f vec

dataZipWith :: 
  (Storage vec b, Storage vec a, Zipper vec, Storage vec c) =>
  (a -> b -> c) -> Data typ vec a -> Data typ vec b -> Data typ vec c
dataZipWith f (Data vec) (Data vec1) = Data $ DV.zipWith f vec vec1

mapTime :: 
  (Walker vec, Storage vec t, Storage vec t1) => 
  (t -> t1)  -> Signal typ label vec t a -> Signal typ label vec t1 a
mapTime f (Signal t d) = Signal (Mono.map f t) d


map :: (Walker vec, Storage vec b, Storage vec c) => 
       (b -> c) ->  Signal typ label vec t b ->  Signal typ label vec t c
map f (Signal t d) = Signal t $ dataMap f d

mapWithTime :: 
  (Zipper vec, Storage vec t,Storage vec a, Storage vec b) => 
       (t -> a -> b) ->  Signal typ label vec t a ->  Signal typ label vec t b
mapWithTime f (Signal t d) = Signal t $ Data $ DV.zipWith f (Mono.getVec t) (getVec d)

zipWith :: 
  (Eq label, Eq (vec t), 
   Zipper vec,
   Storage vec c,
   Storage vec a,
   Storage vec b) => 
  Caller -> (a -> b -> c) -> Signal typ label vec t a ->  
  Signal typ label vec t b ->  Signal typ label vec t c
zipWith caller f (Signal t d)  (Signal t1 d1) = 
  if t == t1 
  then Signal t1 $ dataZipWith f d d1 
  else merror caller modul "zipWith" 
            $ "time axes don't match"