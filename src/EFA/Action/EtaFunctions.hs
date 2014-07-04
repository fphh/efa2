
{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.EtaFunctions where

import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.Vector as DV
--import qualified EFA.Data.Collection as Collection

import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.IO.TableParserTypes as ParseTable

import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified  EFA.Data.OD.Curve as Curve

import EFA.Utility(Caller,
                 --  merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

--import qualified EFA.IO.TableParserTypes as ParseTable
--import qualified EFA.Value.Type as Type

--import qualified Data.List as List
import qualified Data.Map as Map

modul :: ModuleName
modul = ModuleName "OD.Curve"

nc :: FunctionName -> Caller
nc = genCaller modul

data Conf a = Pair a a | Single a | Duplicate a | DuplicateCombine a

-- TODO :: Curve datentypen gerade ziehen
type CurveName = String
{-
data EtaAssignMap node = EtaAssign (Map.Map (TopoIdx.Position node) CurveName)
data Configuration a = InterpolationOpts (Map.Map CurveName (Interp.Method a, Interp.ExtrapMethod a))
data Conditioning a = Conditioning (Map.Map CurveName ([Curve.ModifyOps a]))
-}
data EtaAssignMap node a = EtaAssignMap (Map.Map (TopoIdx.Position node) 
                           (Conf ((Interp.Method a, Interp.ExtrapMethod a), ([Curve.ModifyOps a],CurveName))))

type FunctionMap node a = Map.Map (TopoIdx.Position node) (Interp.Val a -> Interp.Val a)

-- makeEtaAssignMap :: EtaAssign node -> Conditioning a -> Configuration a -> Conditioning a


makeEtaFunctions :: 
  (Arith.Product a,
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
  FunctionMap node a
makeEtaFunctions caller (EtaAssignMap assignMap) etaCurves = Map.mapWithKey f assignMap    
  where f _ assign = 
          let -- TODO:: Insert errror message as Map.lookup        
            g (ops,name) =  (Curve.modify ops $ 
                             (\(Just x) -> x) $ Map.lookup name etaCurves) 
            in case assign of 
          (Pair (m,x) (n,y)) ->  etaFunctionWithTwoCurves (caller |> nc "makeEtaFunction") (m,g x) (n,g y)
          (Single (m,x)) -> etaFunctionWithOneCurve (caller |> nc "makeEtaFunction")  (m,g x)
          (Duplicate (m,x)) -> etaFunctionWithTwoCurves (caller |> nc "makeEtaFunction")(m,Curve.recipY $ Curve.flipX $ g x) (m,g x) 
          (DuplicateCombine (m,x)) -> etaFunctionWithOneCurve (caller |> nc "makeEtaFunction") 
                                      (m ,Curve.combine (caller |> nc "makeEtaFunction") (Curve.recipY $ Curve.flipX $ g x) (g x))
            

-- TODO -- include Range check on curve0 and 1 
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
  Interp.Val  a     
etaFunctionWithTwoCurves caller ((inmethodNeg,exmethodNeg),curveNeg) ((inmethodPos,exmethodPos),curvePos) x = 
  case x>= Arith.zero of 
     False -> Curve.interpolate (caller |> nc "etaFunctionWithTwoCurves") inmethodNeg exmethodNeg curveNeg x
     True -> Curve.interpolate (caller |> nc "etaFunctionWithTwoCurves") inmethodPos exmethodPos curvePos x

etaFunctionWithOneCurve ::
  (Ord a,
   Show a,
   Show label,
   Arith.Constant a,
   DV.Storage vec a,
   DV.LookupUnsafe vec a,
   DV.Length vec,
   DV.Find vec) =>
  Caller -> 
  ((Interp.Method a, Interp.ExtrapMethod a), Curve.Curve inst label vec a a) ->   
  Interp.Val a ->
  Interp.Val a         
etaFunctionWithOneCurve caller ((inmethod,exmethod),curve) x = 
  Curve.interpolate (caller |> nc "etaFunctionWithOneCurve") inmethod exmethod curve x
  
      

toCurveMap :: 
  (DV.Walker vec, DV.Storage vec (Interp.Val a),DV.Storage vec a) => 
  Strict.Axis inst label vec a -> FunctionMap node a -> Curve.Map (TopoIdx.Position node) inst label vec a (Interp.Val a)
toCurveMap axis functionMap = Map.map f functionMap
  where f function = Curve.Curve axis (DV.map function $ DV.map Interp.Inter $ Strict.getVec axis)