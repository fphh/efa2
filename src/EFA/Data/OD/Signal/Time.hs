{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.OD.Signal.Time where

--import qualified EFA.Value as Value
--import qualified EFA.Data.OrdData as OrdData
import qualified EFA.Data.Axis.Mono as Mono
--import qualified EFA.Data.Interpolation as Interpolation
import qualified EFA.Data.Vector as DV
-- import qualified EFA.Data.Vector.NonEmpty2 as DVNE2
--import qualified EFA.Data.Vector.NonEmpty as EV

--import qualified EFA.Equation.Arithmetic as Arith
--import qualified Graphics.Gnuplot.Value.Atom as Atom
--import qualified Graphics.Gnuplot.Value.Tuple as Tuple
--import qualified Data.NonEmpty as NonEmpty

import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

--import qualified EFA.IO.TableParserTypes as ParseTable
--import qualified EFA.Value.Type as Type

--import qualified Data.List as List
--import qualified Data.Map as Map

import Prelude hiding (map,zipWith) 
import qualified Prelude as P

modul :: ModuleName
modul = ModuleName "OD.Curve"

nc :: FunctionName -> Caller
nc = genCaller modul


data Signal inst label vec a b = Signal {getTime :: Mono.Axis inst label vec a, 
                                         getData :: vec b} deriving Show

lookupUnsafe :: DV.LookupUnsafe vec b =>
 Signal inst label vec a b -> Mono.Idx -> b
lookupUnsafe signal (Mono.Idx idx) = DV.lookupUnsafe (getData signal) idx

map :: 
  (DV.Walker vec,
   DV.Storage vec c,
   DV.Storage vec b) =>
  (b -> c) -> Signal inst label vec a b -> Signal inst label vec a c
map f (Signal axis vec) = Signal axis $ DV.map f vec      

-- | the Phantom-Type inst garantees, that both time vectors are identical 
zipWith :: 
  (DV.Zipper vec,
   DV.Storage vec d,
   DV.Storage vec c,
   DV.Storage vec b) =>
  (b -> c -> d) ->
  Signal inst label vec a b ->
  Signal inst label vec a c ->
  Signal inst label vec a d
zipWith f (Signal axis vec) (Signal _ vec1) = Signal axis $ DV.zipWith f vec vec1  

{-
partIntegrate :: 
  (DV.Zipper vec,
   DV.Storage vec b,
   Arith.Constant b,
   DV.Storage vec a,
   DV.Storage vec (Value.Intervall a),
   DV.Singleton vec) => TimeSignal inst label vec a b -> FlowSignal inst label vec a b
partIntegrate (Signal axis vec) = 
  Signal (Mono.toIntervall axis) 
         (DV.deltaMap Arith.mean vec)    
-}


{-
data ModifyOps a = 
  FlipX |
  FlipY |
  RecipY |
  Scale a a |
--  Offset a a |
  AddPntsL [(a,a)] |
  AddPntsR [(a,a)] 
--  AddAtZero b 
  deriving (Show) 

modify :: 
  (Arith.Product a, DV.Walker vec, DV.Storage vec a,
   DV.Singleton vec, DV.Reverse vec, DV.FromList vec) =>
  [ModifyOps a] -> 
  Signal inst label vec a a -> Signal inst label vec a a
modify xs signal = foldl (\crv op  -> modi op crv) signal xs 

modi :: 
  (Arith.Sum a,
   DV.Singleton vec, 
   DV.FromList vec,
   Arith.Product a,
   DV.Walker vec,
   DV.Storage vec a,
   DV.Reverse vec) => 
  ModifyOps a -> Signal inst label vec a a -> Signal inst label vec a a      
modi FlipX  signal = flipX  signal
modi FlipY  signal = flipY  signal
modi RecipY  signal = recipY  signal
modi (Scale x y)  signal = scale x y  signal
modi (AddPntsL xs)  signal = addPntsL xs  signal
modi (AddPntsR xs)  signal = addPntsR xs  signal
-- modi (ModifLabel f) signal = modifyLabelWith f signal
-- TODO : mod (AddZero x y)  signal = Signal axis vec

flipY ::  
  (Arith.Sum b, DV.Walker vec, DV.Storage vec b) =>
  Signal inst label vec a b -> Signal inst label vec a b

flipY (Signal axis vec) = Signal axis (DV.map Arith.negate vec)

recipY :: 
  (Arith.Product b, DV.Walker vec, DV.Storage vec b) =>
  Signal inst label vec a b -> Signal inst label vec a b
recipY (Signal axis vec) = Signal axis (DV.map Arith.recip vec)

scale :: 
  (Arith.Product b, Arith.Product a, DV.Walker vec, DV.Storage vec b,
   DV.Storage vec a) =>
  a -> b -> Signal inst label vec a b -> Signal inst label vec a b
scale x y (Signal axis vec) = Signal (Mono.scale x axis) (DV.map (Arith.~*y) vec)

addPntsL :: 
  (DV.Storage vec b, DV.Storage vec a, DV.Singleton vec,
   DV.FromList vec) =>
  [(a, b)] -> Signal inst label vec a b -> Signal inst label vec a b
addPntsL xs (Signal axis vec) = Signal (Mono.addLeft (P.map fst xs) axis) 
                               (DV.append (DV.fromList $ P.map snd xs) vec)

addPntsR ::
  (DV.Storage vec b, DV.Storage vec a, DV.Singleton vec,
   DV.FromList vec) =>
  [(a, b)] -> Signal inst label vec a b -> Signal inst label vec a b
addPntsR xs (Signal axis vec) = Signal (Mono.addRight axis $ P.map fst xs) 
                               (DV.append vec $ DV.fromList $ P.map snd xs)

combine ::  
  (Eq label, Ord a, DV.Storage vec b, DV.Storage vec a,
   DV.Singleton vec) =>
  Caller
  -> Signal inst label vec a b
  -> Signal inst label vec a b
  -> Signal inst label vec a b
combine caller (Signal axis vec) (Signal axis1 vec1) = 
  Signal (Mono.combine (caller |> nc "combine") axis axis1) (DV.append vec vec1)


modifyLabelWith :: 
  (label -> label1) ->  
  Signal inst label vec a b ->
  Signal inst label1 vec a b
modifyLabelWith f (Signal axis vec) = (Signal (Mono.modifyLabelWith f axis) vec)


getValueRange :: 
  (Ord b, DV.Storage vec b, DV.Singleton vec) => 
  Signal inst label vec a b -> Value.Range b
getValueRange (Signal _ vec) = Value.getValueRange vec
-}