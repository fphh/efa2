module Main where

import qualified EFA.Data.OD.Curve as Curve
import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Value.Type as Type

import qualified EFA.Action.EtaFunctions as EtaFunctions
import qualified EFA.Data.Plot.D2.Curve as PlotCurve
import qualified EFA.Data.Plot.D2 as PlotD2

import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Data.Map as Map
import EFA.Utility(Caller,
                   --merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Demo.Curve"

nc :: FunctionName -> Caller
nc = genCaller modul

data Node =
     Up1
   | Down1
   | Up2 
   | Down2  
   | Network
   deriving (Eq, Ord, Enum, Show)

etaAssignMap :: EtaFunctions.EtaAssignMap Node Double
etaAssignMap = EtaFunctions.EtaAssignMap $ Map.fromList $
   (TopoIdx.Position Network Up1 ,
    (EtaFunctions.UpStream, EtaFunctions.Duplicate ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 7 0.45], "eta")))) : 
   (TopoIdx.Position Network Down1 ,
    (EtaFunctions.DownStream, EtaFunctions.Duplicate ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 7 0.45], "eta")))) : 
   (TopoIdx.Position Network Up2 ,
    (EtaFunctions.UpStream, EtaFunctions.Single ((Interp.Linear,Interp.ExtrapNone),([Curve.Offset 0 0.1, Curve.Scale 7 0.45], "eta")))) : 
   (TopoIdx.Position Network Down2 ,
    (EtaFunctions.DownStream, EtaFunctions.Single ((Interp.Linear,Interp.ExtrapNone),([Curve.Offset 0 0.1,Curve.Scale 7 0.45], "eta")))) : 
   []

data Base 

eta :: Curve.Curve Base String [] Double Double 
eta = Curve.Curve {Curve.getAxis = Strict.Axis {Strict.getLabel = "coal", 
                                                    Strict.getType = Type.N, 
                                                    Strict.getVec = [0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}, 
                 Curve.getData = [0.1,0.2,0.3,0.4,0.65,0.7,0.75,0.8,0.9,1.0,0.85]}

curveMap :: Curve.Map String Base String [] Double Double
curveMap = Map.fromList [("eta",eta)]


etaFunctions = EtaFunctions.makeEtaFunctions (nc "main") etaAssignMap curveMap

showAxis ::  Strict.Axis Base String [] Double
showAxis = Strict.Axis "Power" Type.P $ [-12,-11.9 .. -0.1] ++ [0,0.1..12]                      

checkMap = EtaFunctions.toCurveMap showAxis etaFunctions

main = do
  PlotD2.allInOneIO DefaultTerm.cons (PlotD2.labledFrame "Original-Curves") (\_ _  -> id) $ PlotCurve.toPlotDataMap curveMap
  PlotD2.allInOneIO DefaultTerm.cons (PlotD2.labledFrame "Interpolated Functions") (\_  plotData -> 
                                                                                     LineSpec.title $ show $ PlotD2.getId plotData) $ 
    PlotCurve.toPlotDataMap checkMap
  
