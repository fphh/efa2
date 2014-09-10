
{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.EtaFunctions where

import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.Vector as DV
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified  EFA.Data.OD.Curve as Curve
import EFA.Utility(Caller,
                   merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

modul :: ModuleName
modul = ModuleName "OD.Curve"

nc :: FunctionName -> Caller
nc = genCaller modul

data Conf a = Pair a a | Single a | Duplicate a | DuplicateCombine a

type CurveName = String
data EtaAssignMap node a = EtaAssignMap (Map.Map (TopoIdx.Position node) 
                           (CalculationDirection,Conf ((Interp.Method a, Interp.ExtrapMethod a), ([Curve.ModifyOps a],CurveName))))

newtype FunctionMap node a = FunctionMap {unFunctionMap :: (Map.Map (TopoIdx.Position node) (a -> a))}

data CalculationDirection = DownStream | UpStream

lookupEtaFunction :: (Ord node) => FunctionMap node a -> TopoIdx.Position node -> Maybe (a -> a)
lookupEtaFunction (FunctionMap m) pos = Map.lookup pos m

makeEtaFunctions :: 
  (Arith.Product a,
   DV.Zipper vec,
   Ord node,
   DV.Walker vec,
   DV.Storage vec a,
   DV.Singleton vec,
   DV.Reverse vec,
   DV.FromList vec, 
   Ord a,
   Show a,
   Arith.Constant (Interp.Val a),
   Arith.Constant a,
   DV.LookupUnsafe vec a,
   DV.Length vec,
   DV.Find vec) =>
  Caller ->
  EtaAssignMap node a -> 
  Curve.Map String inst String vec a a ->  
  FunctionMap node (Interp.Val a)
makeEtaFunctions caller (EtaAssignMap assignMap) etaCurves = FunctionMap $ Map.fromList $ concat $ map f $ Map.toList assignMap    
  where f (pos@(TopoIdx.Position n n1),assign) = 
          let
            oppositePos = TopoIdx.Position n1 n
            g (ops,name) =  (Curve.modify ops $ Maybe.fromMaybe (err name) $ Map.lookup name etaCurves)
            h UpStream (ops,name) =  (Curve.modify (ops++[Curve.CalcEtaCrvUp]) $ 
                                      Maybe.fromMaybe (err name) $ Map.lookup name etaCurves)
                                     
            h DownStream (ops,name) =  (Curve.modify (ops++[Curve.CalcEtaCrvDwn]) $ 
                                        Maybe.fromMaybe (err name) $ Map.lookup name etaCurves)
            
            err n = merror caller modul "makeEtaFunctions" ("Curve not found in Map: " ++ show n)
            
          in case assign of 
          (dir, Pair (m,x) (n,y)) ->
            [(pos, etaFunctionWithTwoCurves (caller |> nc "makeEtaFunction") (m,g x) (n,g y)),
             (oppositePos, etaFunctionWithTwoCurves (caller |> nc "makeEtaFunction") (m,h dir x) (n,h dir y))]
          
          (dir, Single (m,x)) -> 
            [(pos,etaFunctionWithOneCurve (caller |> nc "makeEtaFunction")  (m,g x)),
             (oppositePos,etaFunctionWithOneCurve (caller |> nc "makeEtaFunction")  (m,h dir x))]

          (dir, Duplicate (m,x)) -> 
            [(pos,etaFunctionWithTwoCurves (caller |> nc "makeEtaFunction") (m,Curve.recipY $ Curve.flipX $ g x) (m,g x)), 
              (oppositePos,etaFunctionWithTwoCurves (caller |> nc "makeEtaFunction") 
                           (m,Curve.recipY $ Curve.flipX $ h dir x) (m,h dir x))]
            
          (dir, DuplicateCombine (m,x)) -> 
            [(pos,etaFunctionWithOneCurve (caller |> nc "makeEtaFunction") 
                  (m ,Curve.combine (caller |> nc "makeEtaFunction") (Curve.recipY $ Curve.flipX $ g x) (g x))),
             (oppositePos,etaFunctionWithOneCurve (caller |> nc "makeEtaFunction") 
                          (m ,Curve.combine (caller |> nc "makeEtaFunction") (Curve.recipY $ Curve.flipX $ h dir x) (h dir x)))]
             
            
etaFunctionWithTwoCurves ::  
  (Ord a,
   Show label,
   Show a,
   Arith.Constant a,
   DV.Storage vec a,
   DV.LookupUnsafe vec a,
   DV.Length vec,
   DV.Find vec, 
   Arith.Constant (Interp.Val a)) =>
  Caller -> 
  ((Interp.Method a, Interp.ExtrapMethod a), Curve.Curve inst label vec a a) -> 
  ((Interp.Method a, Interp.ExtrapMethod a), Curve.Curve inst label vec a a) -> 
  Interp.Val a ->
  Interp.Val a     
etaFunctionWithTwoCurves caller ((inmethodNeg,exmethodNeg),curveNeg) ((inmethodPos,exmethodPos),curvePos) x = 
  case x>= Arith.zero of 
    True -> Curve.interpolate (caller |> nc "etaFunctionWithTwoCurves") inmethodPos exmethodPos curvePos x
    False -> Curve.interpolate (caller |> nc "etaFunctionWithTwoCurves") inmethodNeg exmethodNeg curveNeg x
      
etaFunctionWithOneCurve ::
  (Ord a,
   Show a,
   Show label,
   Arith.Constant a,
   DV.Storage vec a,
   DV.LookupUnsafe vec a,
   DV.Length vec,DV.Walker vec,DV.Reverse vec,
   DV.Find vec) =>
  Caller -> 
  ((Interp.Method a, Interp.ExtrapMethod a), Curve.Curve inst label vec a a) ->   
  Interp.Val a ->
  Interp.Val a         
etaFunctionWithOneCurve caller ((inmethod,exmethod),curve) x = 
  Curve.interpolate (caller |> nc "etaFunctionWithOneCurve") inmethod exmethod curve x

toCurveMap :: 
  (DV.Walker vec, DV.Storage vec (Interp.Val a),DV.Storage vec a) => 
  Strict.Axis inst label vec a -> FunctionMap node (Interp.Val a) -> Curve.Map (TopoIdx.Position node) inst label vec a (Interp.Val a)
toCurveMap axis (FunctionMap functionMap) = Map.map f functionMap
  where f function = Curve.Curve axis (DV.map function $ DV.map Interp.Inter $ Strict.getVec axis)
        
        
