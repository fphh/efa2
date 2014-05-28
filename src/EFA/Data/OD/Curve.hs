module EFA.Data.OD.Curve where

import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Interpolation as Interpolation
import qualified EFA.Data.Vector as DV

import qualified EFA.Equation.Arithmetic as Arith


import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
  
modul :: ModuleName
modul = ModuleName "OD.Curve"

nc :: FunctionName -> Caller
nc = genCaller modul

data Curve inst label vec a b = Curve {getAxis :: Strict.Axis inst label vec a, 
                                       getData :: vec b}
     
copyPos2Neg :: 
  (DV.Singleton vec, 
   Ord a, 
   Arith.Constant a, 
   Eq a, 
   DV.Storage vec a, 
   DV.Walker vec, 
   Arith.Sum a, 
   DV.Storage vec b, 
   DV.Reverse vec) =>
  Caller ->
  Maybe b ->
  Curve inst label vec a b -> 
  Curve inst label vec a b
copyPos2Neg caller zeroVal (Curve (Strict.Axis label typ av) dv) = case (DV.any (< Arith.zero) av, DV.any (== Arith.zero) av, zeroVal) of 
  (False,False,Nothing) -> Curve (Strict.Axis label typ (append nav av)) $ append ndv dv 
  (False,False,Just x) -> Curve (Strict.Axis label typ $ appendInsertingZeroVal nav Arith.zero av) 
                          $ appendInsertingZeroVal ndv x dv
  (False,True,Nothing) -> Curve (Strict.Axis label typ $ appendWithZeroVal nav av) $ appendWithZeroVal ndv dv 
  (False,True,Just x) -> merror (caller |> nc "copyPos2Neg") modul "copyPos2Neg" "ZeroPoint already exists"  
  (True,_,_) -> merror caller modul "copyPos2Neg" "Negative Data already exists"
  where 
    nav = DV.reverse $ DV.map Arith.negate av
    ndv = DV.reverse dv
    append x y = DV.append x y    
    appendInsertingZeroVal x y z = DV.append x $ DV.append (DV.singleton y) z
    appendWithZeroVal x y = DV.append (DV.init x) y 


lookupUnsafe :: (DV.LookupUnsafe vec b) => Curve inst label vec a b -> Strict.Idx -> b
lookupUnsafe curve (Strict.Idx idx) = DV.lookupUnsafe (getData curve) idx


interpolate :: 
  (Eq a, Show a, Arith.Product a, 
   Ord a,
   DV.Storage vec a,
   DV.LookupUnsafe vec a,
   DV.Length vec,
   DV.Find vec) =>
  String -> Interpolation.Method a -> Interpolation.ExtrapMethod a -> 
  Curve inst label vec a a -> a -> (Interpolation.Val a)
interpolate caller inmethod exmethod curve x = Interpolation.dim1 caller inmethod exmethod (x1, x2) (y1, y2) x
  where
    ((idx1,idx2),(x1,x2)) = Strict.getSupportPoints (getAxis curve) x
    y1 = lookupUnsafe curve idx1
    y2 = lookupUnsafe curve idx2
    
    

     