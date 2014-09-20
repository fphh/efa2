
{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.EtaFunctions where

import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.Vector as DV
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified  EFA.Data.OD.Curve as Curve
import qualified  EFA.Value.Type as Type

import EFA.Utility(Caller,
                   nerror,
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
   Arith.Root a, 
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
            err x = merror caller modul "makeEtaFunctions" ("Curve not found in Map: " ++ show x)
            
          in case assign of 
          (dir, Pair (m,x) (m1,y)) ->
            [(pos, etaFunctionWithTwoCurves (caller |> nc "makeEtaFunction") (m,g x) (m1,g y)),
             (oppositePos, etaFunctionWithTwoCurves (caller |> nc "makeEtaFunction") (rev dir (m,g x)) (rev dir (m1,g y)))]
          
          (dir, Single (m,x)) -> 
            [(pos,etaFunctionWithOneCurve (caller |> nc "makeEtaFunction")  (m,g x)),
             (oppositePos,etaFunctionWithOneCurve (caller |> nc "makeEtaFunction")  (rev dir (m,g x)))]

          (dir, Duplicate (m,x)) -> 
            [(pos,etaFunctionWithTwoCurves (caller |> nc "makeEtaFunction") (m,Curve.recipY $ Curve.flipX $ g x) (m,g x)), 
              (oppositePos,etaFunctionWithTwoCurves (caller |> nc "makeEtaFunction") 
                           (rev dir (m,Curve.recipY $ Curve.flipX $ g x)) (rev dir (m,g x)))]
            
          (dir, DuplicateCombine (m,x)) -> 
            [(pos,etaFunctionWithOneCurve (caller |> nc "makeEtaFunction") 
                  (m ,Curve.combine (caller |> nc "makeEtaFunction") (Curve.recipY $ Curve.flipX $ g x) (g x))),
             (oppositePos,etaFunctionWithOneCurve (caller |> nc "makeEtaFunction") 
                          (rev dir (m ,Curve.combine (caller |> nc "makeEtaFunction") (Curve.recipY $ Curve.flipX $ g x) (g x))))]
             
            
etaFunctionWithTwoCurves ::  
  (Ord a,Arith.Root a,
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
   Show a,Arith.Root a,
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

data Location = PowerIn | PowerOut

etaFunctionWithOneCurveAlt ::
  (Ord a,
   Show a,Arith.Root a,
   Show label,
   Arith.Constant a,
   DV.Storage vec a,
   DV.LookupUnsafe vec a,
   DV.Zipper vec,
   DV.Length vec,DV.Walker vec,DV.Reverse vec,
   DV.Find vec) =>
  Caller -> 
  Location ->
  ((Interp.Method a, Interp.ExtrapMethod a), Curve.Curve inst label vec a a) ->   
  Interp.Val a ->
  Interp.Val a         
etaFunctionWithOneCurveAlt caller loc ((inmethod,exmethod),curve) x = case loc of 
  PowerIn -> y Arith.~/ x
  PowerOut -> x Arith.~/ y
  where
    y = Curve.interpolate newCaller inmethod exmethod pCurve x
    newCaller = caller |> nc "etaFunctionWithOneCurveAlt"
    pCurve = case loc of 
      PowerIn -> Curve.zipDataWithAxis (Arith.~*) curve
      PowerOut -> Curve.zipDataWithAxis (Arith.~/) curve
      
etaFunctionWithOneCurveAltOpp ::
  (Ord a,
   Show a,Arith.Root a,
   DV.Zipper vec,
   DV.Storage vec Bool,
   DV.Singleton vec,
   Show label,
   Arith.Constant a,
   DV.Storage vec a,
   DV.LookupUnsafe vec a,
   DV.Length vec,DV.Walker vec,DV.Reverse vec,
   DV.Find vec) =>
  Caller -> 
  Location ->
  label ->
  ((Interp.Method a, Interp.ExtrapMethod a), Curve.Curve inst label vec a a) ->   
  Interp.Val a ->
  Interp.Val a         
etaFunctionWithOneCurveAltOpp caller loc label ((inmethod,exmethod),curve) x = case loc of 
  PowerIn -> x Arith.~/ y
  PowerOut -> y Arith.~/ x 
  where
    y = Curve.interpolate newCaller inmethod exmethod pCurve x
    newCaller = caller |> nc "etaFunctionWithOneCurveAltOpp"
    pCurve = case loc of 
      PowerIn -> Curve.reverseCurve newCaller label Type.P $ Curve.zipDataWithAxis (Arith.~*) curve
      PowerOut -> Curve.reverseCurve newCaller label Type.P $ Curve.zipDataWithAxis (Arith.~/) curve
      
    
toCurveMap :: 
  (DV.Walker vec, DV.Storage vec (Interp.Val a),DV.Storage vec a) => 
  Strict.Axis inst label vec a -> FunctionMap node (Interp.Val a) -> Curve.Map (TopoIdx.Position node) inst label vec a (Interp.Val a)
toCurveMap axis (FunctionMap functionMap) = Map.map f functionMap
  where f function = Curve.Curve axis (DV.map function $ DV.map Interp.Inter $ Strict.getVec axis)
        
reverseInMethod :: 
  CalculationDirection -> 
  Interp.Method a ->
  Interp.Method a
reverseInMethod UpStream Interp.Linear = Interp.LinEtaUpStream  
reverseInMethod DownStream Interp.Linear = Interp.LinEtaDwnStream  
reverseInMethod _ m = nerror modul "reverseInMethod" $ "method not suppoted: " ++ show m

reverseExMethod :: 
  Show a =>
  CalculationDirection -> 
  Interp.ExtrapMethod a ->
  Interp.ExtrapMethod a
reverseExMethod UpStream Interp.ExtrapLinear = Interp.ExtrapLinEtaUpStream  
reverseExMethod DownStream Interp.ExtrapLinear = Interp.ExtrapLinEtaDwnStream  
reverseExMethod _ Interp.ExtrapNone = Interp.ExtrapNone  
reverseExMethod _ Interp.ExtrapError = Interp.ExtrapError
reverseExMethod _ m = nerror modul "reverseExMethod" $ "method not supported: " ++ show m
  
reverseCurve :: 
  (Arith.Root a,
   Arith.Product a,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec a,
   DV.Singleton vec,
   DV.Reverse vec,
   DV.FromList vec) =>
  CalculationDirection -> 
  Curve.Curve inst label vec a a -> 
  Curve.Curve inst label vec a a
reverseCurve UpStream curve = Curve.modify [Curve.CalcEtaCrvUp] curve
reverseCurve DownStream curve = Curve.modify [Curve.CalcEtaCrvDwn] curve


-- Create Reverse Curve and choose reverse Interpolation Method
rev :: 
  (Show a, 
   Arith.Root a,
   Arith.Product a,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec a,
   DV.Singleton vec,
   DV.Reverse vec,
   DV.FromList vec) =>
  CalculationDirection -> 
  ((Interp.Method a,Interp.ExtrapMethod a), Curve.Curve inst label vec a a) -> 
  ((Interp.Method a,Interp.ExtrapMethod a), Curve.Curve inst label vec a a)
rev dir ((x,y),curve) = ((reverseInMethod dir x,reverseExMethod dir y), reverseCurve dir curve )
