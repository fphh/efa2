{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where


--import qualified Modules.Signals as Signals
import qualified Modules.System as System
import Modules.System (Node(..))
--import qualified Modules.Analysis as Analysis
import qualified Modules.Optimisation as Optimisation
--import Modules.Optimisation (sec0,sec1)
--import Modules.Utility as ModUt


--import qualified EFA.Signal.Record as Record
import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Draw as Draw
--import EFA.IO.PLTImport (modelicaPLTImport)
import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Vector as V
--import EFA.Signal.Data (Data(..), Nil, (:>))
--import EFA.Signal.Typ (Typ, F, T, A, Tt)
--import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Utility as Utility
--import EFA.Signal.Sequence (makeSeqFlowTopology)
--import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Graph.Draw as Draw
--import EFA.Graph(lefilter)
--import qualified EFA.Signal.Plot as Plot
--import EFA.Graph.Topology(isStructureEdge)
--import qualified EFA.Example.Index as XIdx
--import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Equation.Environment as Env
-- import qualified EFA.Signal.ConvertTable as Table
--import qualified EFA.Example.Absolute as EqGen
--import qualified EFA.Example.Index as XIdx
--import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import EFA.Equation.Result (Result(..))
--import EFA.Utility.Map (checkedLookup)
--import qualified EFA.Signal.PlotIO as PlotIO

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.IO.TableParser as Table

import qualified System.IO as IO
--import System.Environment (getEnv)
--import System.FilePath ((</>))

--import qualified EFA.Utility.Stream as Stream

-- import Data.Monoid (mconcat, (<>))
--import EFA.Example.Absolute ( (.=), (%=), (=.=) )
--import EFA.Utility.Stream (Stream((:~)))

--import qualified Data.List as L
import qualified Data.Map as M
--import qualified Modules.Analysis as Analysis
--import Data.Tuple.HT (mapSnd)

--import qualified Data.Maybe as Maybe


plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

colours :: [Colors.X11Color]
colours = [ Colors.White,	
            Colors.Gray90,	
            Colors.Gray80,	
            Colors.Gray70 ]
          
-- ################### Vary Operation Demand

restPower :: [Double]
restPower = [0, 0.1 .. 0.9]

localPower :: [Double]
localPower = [0, 0.1 .. 0.8]

varRestPower', varLocalPower' :: [[Double]]
(varRestPower', varLocalPower') = CT.varMat restPower localPower

restPowerScale :: Double
restPowerScale = 1

localPowerScale :: Double
localPowerScale = 1

varRestPower :: Sig.PSignal2 [] [] Double
varRestPower = Sig.fromList2 varRestPower'

varLocalPower :: Sig.PSignal2 [] [] Double
varLocalPower = Sig.fromList2 varLocalPower'




-- ################### Vary Degrees of Freedom

waterPower :: [Double]
waterPower = [0,0.2 .. 1]

gasPower :: [Double]
gasPower = [0, 0.1 .. 1]

powerScaleWater ::  Double
powerScaleWater = 5
  
powerScaleGas ::  Double
powerScaleGas = 1

powerScaleTransformer ::  Double
powerScaleTransformer = 1

powerScaleCoal ::  Double
powerScaleCoal = 1  
  
varWaterPowerSig :: Sig.PTestRow [] Double
varWaterPowerSig = Sig.fromList waterPower

varGasPowerSig :: Sig.PTestRow [] Double
varGasPowerSig = Sig.fromList gasPower

varWaterPower', varGasPower' :: [[Double]]
(varWaterPower', varGasPower') = CT.varMat waterPower gasPower

varWaterPower :: Sig.PSignal2 [] [] Double
varWaterPower = Sig.fromList2 varWaterPower'

varGasPower :: Sig.PSignal2 [] [] Double
varGasPower = Sig.fromList2 varGasPower'

noLegend :: Int -> String
noLegend =  (const "")

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"


                 


main :: IO ()
main = do

   IO.hSetEncoding IO.stdout IO.utf8
    
   tabEta <- Table.read "../simulation/maps/eta.txt"

   let etaWaterCharge :: Double -> Double
       etaWaterCharge = Sig.fromSample . Sig.interp1Lin "etaWaterCharge" (Sig.scale xs powerScaleWater) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (M.lookup "storage" tabEta)
                        
   let etaWaterDischarge :: Double -> Double
       etaWaterDischarge = Sig.fromSample . Sig.interp1Lin "etaWaterDischarge" (Sig.scale xs powerScaleWater) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (M.lookup "storage" tabEta)
   
   let etaGas :: Double -> Double
       etaGas = Sig.fromSample . Sig.interp1Lin "etaGas" (Sig.scale xs powerScaleGas) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (M.lookup "gas" tabEta)
               
   let etaCoal :: Double -> Double
       etaCoal = Sig.fromSample . Sig.interp1Lin "etaCoal" (Sig.scale xs powerScaleCoal) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (M.lookup "coal" tabEta)
   
   let etaTransHL :: Double -> Double
       etaTransHL = Sig.fromSample . Sig.interp1Lin "etaTransHL" (Sig.scale xs powerScaleTransformer) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (M.lookup "transformer" tabEta)

   let 
     
     -- | Search DOF (Degree of Fredom) Space
     searchCharge :: Double -> 
                     Double -> 
                     Sig.UTTestRow2 [] [] 
                     (EqEnv.Complete  
                       Node
                       (EqRec.Absolute (Result Double))
                       (EqRec.Absolute (Result Double)))

     searchCharge rPower lPower = Sig.fromList2 $ 
                                         zipWith (zipWith (Optimisation.solveCharge 
                                                           etaWaterCharge etaCoal etaGas 
                                                           etaTransHL rPower lPower)) 
                                         varWaterPower' varGasPower'
     
    -- Search all Configuration Optimations
     searchDischarge :: Double -> 
                        Double -> 
                        Sig.UTTestRow2 [] []
                        ((EqEnv.Complete  
                          Node
                          (EqRec.Absolute (Result Double))
                          (EqRec.Absolute (Result Double))))
     searchDischarge rPower lPower = Sig.fromList2 $ 
                                            zipWith (zipWith (Optimisation.solveDischarge 
                                                              etaWaterDischarge etaCoal etaGas 
                                                              etaTransHL rPower lPower)) 
                                            varWaterPower' varGasPower'
     
    -- | Map Operation space room
     mapCharge :: Sig.UTTestRow2 [] [] (Sig.UTTestRow2 [] []
                         (EqEnv.Complete  
                           Node
                           (EqRec.Absolute (Result  Double))
                           (EqRec.Absolute (Result  Double))))
     mapCharge = Sig.fromList2 $ zipWith (zipWith searchCharge ) varRestPower' varLocalPower'


     mapDischarge :: Sig.UTTestRow2 [] [] (Sig.UTTestRow2 [] []
                           (EqEnv.Complete  
                             Node
                             (EqRec.Absolute (Result Double))
                             (EqRec.Absolute (Result Double))))
     mapDischarge = Sig.fromList2 $ zipWith (zipWith searchDischarge ) varRestPower' varLocalPower'

 

{-
     etaSysCharge :: Sig.NTestRow2 [] [] Double
     etaSysCharge = eOut Sig../ eIn
       where
         eOut = (eRestCharge0 Sig..+ (Sig.makeDelta $ eRestCharge1 Sig..+ 
                                 (Sig.makeDelta $ eRestLocalCharge0 Sig..+ (Sig.makeDelta eRestLocalCharge1))))
         eIn = (eGasCharge0 Sig..+ (Sig.makeDelta $ eGasCharge1 Sig..+ 
                               (Sig.makeDelta $ eCoalCharge0  Sig..+ (Sig.makeDelta eCoalCharge1))))

     etaSysDischarge :: Sig.NTestRow2 [] [] Double
     etaSysDischarge = eOut Sig../ eIn
      where
        eOut = (eRestDischarge0 Sig..+ (Sig.makeDelta $ eRestDischarge1 Sig..+ 
                                        (Sig.makeDelta $ eRestLocalDischarge0 Sig..+ (Sig.makeDelta eRestLocalDischarge1))))
        eIn = (eGasDischarge0 Sig..+ (Sig.makeDelta $ eGasDischarge1 Sig..+ 
                                  (Sig.makeDelta $ eCoalDischarge0  Sig..+ (Sig.makeDelta eCoalDischarge1))))
    
     etaSysMax::    Sig.NTestRow2 [] [] Double
     etaSysMax = Sig.zipWith max etaSysCharge etaSysDischarge  
                
-}
 

 
   concurrentlyMany_ $ [
     Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNamesOpt System.topologyOpt,
     
     putStrLn ("Number of possible flow states: " ++ show (length System.flowStatesOpt)),
     
     Draw.xterm $ Draw.flowTopologies (take 20 System.flowStatesOpt),
     
     Draw.xterm $ Draw.sequFlowGraph System.seqTopoOpt]

