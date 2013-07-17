{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Main where



import qualified Modules.System as System
import qualified Modules.Optimisation as Optimisation
import qualified Modules.Analysis as Analysis
import Modules.System (Node(..))
import Modules.Optimisation (EnvDouble, sec0,sec1, maxEta, maxOpt,
                            -- calcEtaSys
                            )
import Modules.Utility as ModUt
-- import Modules.Utility(getEtas, getPowerSignals,select)

import qualified EFA.Application.EtaSys as ES
import qualified EFA.Application.Index as XIdx
import qualified EFA.Application.Absolute as EqGen
import EFA.Application.Utility (select)

import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Draw as Draw

import qualified EFA.Equation.Arithmetic as EqArith
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import EFA.Equation.Result (Result(..))

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.PlotIO as PlotIO
import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Data as Data
import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Sequence (makeSeqFlowTopology, addZeroCrossings, genSequ,)
import EFA.Signal.Signal (TC,Scalar)
import EFA.Signal.Data (Data(..), Nil, (:>), getData)
import EFA.Signal.Typ (Typ, F, T, A, Tt)

import EFA.Utility.Async (concurrentlyMany_)
import EFA.Report.Report(report,ROpt(..))

import qualified EFA.IO.TableParser as Table
import qualified EFA.IO.TableParserTypes as TPT

import qualified EFA.Utility.Bifunctor as BF

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Graphics.Gnuplot.Terminal.PostScript as PostScript
import qualified Graphics.Gnuplot.Terminal.PNG as PNG

import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Vector as V
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import Text.Printf (printf)

import Data.Maybe (fromJust)
import Data.Tuple.HT (fst3, snd3, thd3)

import qualified System.IO as IO


-- ################### Plot Stuff

plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

colours :: [Colors.X11Color]
colours = [ Colors.White,	
            Colors.Gray90,	
            Colors.Gray80,	
            Colors.Gray70 ]

{-
<<<<<<< HEAD

restPower :: [Double]
restPower = [1,2 .. 5]

localPower :: [Double]
localPower = [1, 2 .. 6]

sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0

varX', varY' :: [[Double]]
(varX', varY') = Table.varMat restPower localPower

varX :: Sig.PSignal2 [] [] Double
varX = Sig.fromList2 varX'

varY :: Sig.PSignal2 [] [] Double
varY = Sig.fromList2 varY'

-- | Defining the given Variables

lookupEtaWaterCharge ::
  EqGen.Expression node s a v Double ->
  EqGen.Expression node s a v Double
lookupEtaWaterCharge = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin "lookupEtaWaterCharge" xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

lookupEtaWaterDisCharge ::
  EqGen.Expression node s a v Double ->
  EqGen.Expression node s a v Double
lookupEtaWaterDisCharge = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin "lookupEtaWaterDisCharge" xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

lookupEtaCoal ::
  EqGen.Expression node s a v Double ->
  EqGen.Expression node s a v Double
lookupEtaCoal = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin "lookupEtaCoal" xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

lookupEtaGas ::
  EqGen.Expression node s a v Double ->
  EqGen.Expression node s a v Double
lookupEtaGas = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin "lookupEtaGas" xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

lookupEtaTransformerHL ::
  EqGen.Expression node s a v Double ->
  EqGen.Expression node s a v Double
lookupEtaTransformerHL = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin "lookupEtaTransformerHL" xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

-- | Defining the given Variables

commonGiven :: EqGen.EquationSystem System.Node s Double Double
commonGiven =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (XIdx.dTime sec1 .= 1) :
   (XIdx.storage TIdx.initial Water .= 0) :
   (XIdx.energy sec0 Water Network  %= XIdx.energy sec1 Water Network) :
   []

givenOptimiseCharging :: Double -> Double -> Double -> Double -> EqGen.EquationSystem System.Node s Double Double
givenOptimiseCharging pRest pRestLocal pCoal pGas =
   (commonGiven <>) $
   mconcat $

   -- Actual Section 0 Charhing to be varied and optimised
   (XIdx.power sec0 Rest Network .= pRest) :
   (XIdx.power sec0 LocalRest LocalNetwork .= pRestLocal) :
   (XIdx.power sec0 Coal Network .= pCoal) :
   (XIdx.power sec0 Gas LocalNetwork .= pGas) :
   ((EqGen.variable $ XIdx.eta sec0 Network Water) =.=
     lookupEtaWaterCharge (EqGen.variable $ XIdx.power sec0 Network Water)) :
   ((EqGen.variable $ XIdx.eta sec0 Coal Network) =.=
     lookupEtaCoal (EqGen.variable $ XIdx.power sec0 Coal Network)) :
   ((EqGen.variable $ XIdx.eta sec0 Gas LocalNetwork) =.=
     lookupEtaGas (EqGen.variable $ XIdx.power sec0 Gas LocalNetwork)) :
   ((EqGen.variable $ XIdx.eta sec0 Network LocalNetwork) =.=
     lookupEtaTransformerHL (EqGen.variable $ XIdx.power sec0 LocalNetwork Network)) :
    (XIdx.eta sec0 Network Rest .= 1.0) :
   (XIdx.eta sec0 LocalNetwork LocalRest .= 1.0) :

   -- Average Section 1 discharging
   (XIdx.eta sec1 Network Rest .= 1.0) :
   (XIdx.eta sec1 LocalNetwork LocalRest .= 1.0) :
   (XIdx.eta sec1 Network LocalNetwork .= 0.8) :
   (XIdx.eta sec1 Coal Network .= 0.4) :
   (XIdx.eta sec1 Gas LocalNetwork .= 0.4) :
   (XIdx.eta sec1 Water Network .= 0.4) :
   (XIdx.eta sec1 Network Water .= 0.4) :
--   (XIdx.x sec1 Network Coal .= 0.7) :
   (XIdx.x sec1 Network Water .= 0.7) :
--   (XIdx.x sec1 LocalNetwork Gas .= 0) :
   (XIdx.x sec1 LocalNetwork Network .= 1.0) :
   (XIdx.x sec1 Network LocalNetwork .= 0.5) :

   []

givenOptimiseDischarging :: Double -> Double -> Double -> Double -> EqGen.EquationSystem System.Node s Double Double
givenOptimiseDischarging pRest pRestLocal pCoal pGas =
   (commonGiven <>) $
   mconcat $

   -- Actual Section 1 discharging to be varied and optimised
   (XIdx.power sec1 Rest Network .= pRest) :
   (XIdx.power sec1 LocalRest LocalNetwork .= pRestLocal) :
   (XIdx.power sec1 Coal Network .= pCoal) :
   (XIdx.power sec1 Gas LocalNetwork .= pGas) :
   ((EqGen.variable $ XIdx.eta sec1 Water Network) =.=
     lookupEtaWaterDisCharge (EqGen.variable $ XIdx.power sec1 Network Water)) :
   ((EqGen.variable $ XIdx.eta sec1 Coal Network) =.=
     lookupEtaCoal (EqGen.variable $ XIdx.power sec1 Coal Network)) :
   ((EqGen.variable $ XIdx.eta sec1 Gas LocalNetwork) =.=
     lookupEtaGas (EqGen.variable $ XIdx.power sec1 Gas LocalNetwork)) :
   ((EqGen.variable $ XIdx.eta sec1 Network LocalNetwork) =.=
     lookupEtaTransformerHL (EqGen.variable $ XIdx.power sec1 LocalNetwork Network)) :
   (XIdx.eta sec1 Network Rest .= 1.0) :
   (XIdx.eta sec1 LocalNetwork LocalRest .= 1.0) :

   -- Average Section 0 discharging
   (XIdx.eta sec0 Network Rest .= 1.0) :
   (XIdx.eta sec0 LocalNetwork LocalRest .= 1.0) :
   (XIdx.eta sec0 Coal Network .= 0.4) :
   (XIdx.eta sec0 Gas LocalNetwork .= 0.4) :
   (XIdx.eta sec0 Water Network .= 0.4) :
   (XIdx.eta sec0 Network Water .= 0.4) :
   (XIdx.eta sec0 Network LocalNetwork .= 0.8) :
   (XIdx.x sec0 Network Water .= 0.2) :
   (XIdx.x sec0 Coal Network .= 0.5) :
   (XIdx.x sec0 LocalNetwork Network .= 1.0) :
   (XIdx.x sec0 Network LocalNetwork .= 0.5) :

   []

etaSys :: EnvDouble -> Double
etaSys env =
  (lk eRest0 + lk eRest1 + lk eRestLocal0 + lk eRestLocal1) / (lk eGas0 + lk eGas1 + lk eCoal0 + lk eCoal1)
  where
        lk n = case checkedLookup (EqEnv.energyMap $ EqEnv.signal env) n of
                  EqRec.Absolute (Determined x) -> x
                  _ -> error "not determined"
        eCoal0  =  (XIdx.energy sec0 Coal Network)
        eGas0  =  (XIdx.energy sec0 Gas LocalNetwork)
        eRest0  =  (XIdx.energy sec0 Network Rest)
        eRestLocal0  =  (XIdx.energy sec0 LocalNetwork LocalRest)

        eCoal1  =  (XIdx.energy sec1 Coal Network)
        eGas1 =  (XIdx.energy sec1 Gas LocalNetwork)
        eRest1  =  (XIdx.energy sec1 Network Rest)
        eRestLocal1  =  (XIdx.energy sec1 LocalNetwork LocalRest)
           --  | otherwise = error "not found"

select :: [topo] -> [Int] -> SD.SequData topo
select ts = SD.fromList . map (ts !!)

seqTopo :: Flow.RangeGraph Node
seqTopo = Flow.mkSequenceTopology (select System.flowStatesOpt [4,0])

solveCharge :: Double -> Double -> Double -> Double -> Double
solveCharge w x y z = etaSys $ EqGen.solve seqTopo $ givenOptimiseCharging w x y z

solveDischarge :: Double -> Double -> Double -> Double -> Double
solveDischarge w x y z = etaSys $ EqGen.solve seqTopo $ givenOptimiseDischarging w x y z

-- envsCharge =

etaSysCharge, etaSysDischarge :: Sig.NSignal2 [] [] Double
etaSysCharge = Sig.fromList2 $ zipWith (zipWith (solveCharge 10 10)) varX' varY'
etaSysDischarge = Sig.fromList2 $ zipWith (zipWith (solveDischarge 10 10)) varX' varY'
=======
-}

noLegend :: Int -> String
noLegend =  (const "")
-- >>>>>>> philipp_neu

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"

scaleTableEta :: Map String (Double, Double)
scaleTableEta = Map.fromList $
  ("storage",     (1, 0.8)) :
  ("gas",         (1, 0.4)) :
  ("transformer", (3.0, 0.95)) :
  ("coal",        (6, 0.46)) :
  ("local",       (1, 1)) :
  ("rest",        (1, 1)) :
  []

-- ################### Efficiency Curves

etaAssign ::
  TIdx.Section ->
  Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)
etaAssign sec = Map.fromList $
  (XIdx.eta sec Water Network, ( "storage", "storage", myflip)) :
  (XIdx.eta sec Network Water, ( "storage", "storage", noflip)) :

  (XIdx.eta sec Coal Network, ( "coal", "coal", myflip)) :
  (XIdx.eta sec Network Coal, ( "coal", "coal", noflip)) :

  (XIdx.eta sec Gas LocalNetwork, ( "gas", "gas", myflip)) :
  (XIdx.eta sec LocalNetwork Gas, ( "gas", "gas", noflip)) :


  (XIdx.eta sec Network LocalNetwork, ( "transformer", "transformer", myflip)) :
  (XIdx.eta sec LocalNetwork Network, ( "transformer", "transformer", noflip)) :

  (XIdx.eta sec LocalNetwork LocalRest, ( "local", "local", myflip)) :
  (XIdx.eta sec LocalRest LocalNetwork, ( "local", "local", noflip)) :

  (XIdx.eta sec Network Rest, ( "rest", "rest", myflip)) :
  (XIdx.eta sec Rest Network, ( "rest", "rest", noflip)) :

  []

noflip :: TIdx.InSection TIdx.Eta node -> TIdx.InSection TIdx.Power node
noflip (TIdx.InSection sec ( TIdx.Eta (TIdx.StructureEdge n1 n2))) = 
  TIdx.InSection sec (TIdx.Power (TIdx.StructureEdge n1 n2))


myflip :: TIdx.InSection TIdx.Eta node -> TIdx.InSection TIdx.Power node
myflip (TIdx.InSection sec ( TIdx.Eta (TIdx.StructureEdge n1 n2))) = 
  TIdx.InSection sec (TIdx.Power (TIdx.StructureEdge n2 n1))

-- ################### Vary Power Demand


frameOpts ::
  Opts.T (Graph3D.T Double Double Double) ->
  Opts.T (Graph3D.T Double Double Double)
frameOpts =
  Plot.heatmap .
  Plot.xyzrange3d (0.2, 2) (0.3, 3.3) (0, 1) .
  -- Plot.cbrange (0.2, 1) .
  Plot.xyzlabel "Rest Power [W]" "Local Power [W]" "" .
  Plot.paletteGH

restPower :: [Double]
restPower = [0.2, 0.3 .. 2]

localPower :: [Double]
localPower = [0.3, 0.4 .. 3.3]

varRestPower', varLocalPower' :: [[Double]]
(varLocalPower', varRestPower') = CT.varMat localPower restPower 

restPowerScale :: Double
restPowerScale = 1

varRestPower1D :: Sig.PSignal V.Vector Double
varRestPower1D = Sig.fromList restPower

localPowerScale :: Double
localPowerScale = 1.2

varRestPower :: Sig.PSignal2 V.Vector V.Vector Double
varRestPower = Sig.fromList2 varRestPower'

varLocalPower :: Sig.PSignal2 V.Vector V.Vector Double
varLocalPower = Sig.fromList2 varLocalPower'


-- ################### Vary Degrees of Freedom for Optimisation

waterPower :: [Double]
waterPower = [0.2,0.4 .. 0.8]

gasPower :: [Double]
gasPower = [0.2,0.4 .. 1.0]

varWaterPowerSig :: Sig.PTestRow [] Double
varWaterPowerSig = Sig.fromList waterPower

varGasPowerSig :: Sig.PTestRow [] Double
varGasPowerSig = Sig.fromList gasPower

varWaterPower', varGasPower' :: [[Double]]
(varWaterPower', varGasPower') = CT.varMat waterPower gasPower

varWaterPower :: Sig.PSignal2 V.Vector V.Vector Double
varWaterPower = Sig.fromList2 varWaterPower'

varGasPower :: Sig.PSignal2 V.Vector V.Vector Double
varGasPower = Sig.fromList2 varGasPower'

sweep ::(SV.Storage c2 d,
          SV.Storage c1 (c2 d), SV.FromList c2,
          SV.FromList c1) =>
         (a -> b -> d)
         -> [[a]] -> [[b]] -> TC s t (Data (c1 :> (c2 :> Nil)) d)
sweep func xs ys = Sig.fromList2 $ zipWith (zipWith func) xs ys

doubleSweep ::
  Optimisation.SolveFunc Double ->
  Map String (Double -> Double) ->
  [[Double]] ->
  [[Double]] ->
  [[Double]] ->
  [[Double]] ->
  Sig.UTSignal2 V.Vector V.Vector
    (Sig.UTSignal2 V.Vector V.Vector
      (EqEnv.Complete
        Node
        (Result (Data Nil Double))
        (Result (Data Nil Double))))
doubleSweep func etaFunc varOptX varOptY varX varY=
  sweep f (mm varX) (mm varY)
  where f x y =
          sweep (func etaAssign etaFunc x y) xo yo
        xo = mm varOptX
        yo = mm varOptY
        mm = map (map Data)


combineOptimalMaps ::
  Sig.UTSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double
combineOptimalMaps state charge discharge =
  Sig.zipWith f state $ Sig.zip charge discharge
  where f s (c, d) = if s < 0.1 then c else d

envGetData ::
  (Ord node) =>
  EqEnv.Complete node (Result (Data va a)) (Result (Data vv v)) ->
  EqEnv.Complete node (Result (Data.Apply va a)) (Result (Data.Apply vv v))
envGetData =
  EqEnv.completeFMap (fmap getData) (fmap getData)

makePics ::
  (forall s. EqGen.EquationSystem Node s (Data Nil Double) (Data Nil Double)) ->
  TPT.Map Double ->
  TPT.Map Double ->
  Double ->
  ( Sig.UTSignal2 V.Vector V.Vector Double,
    Sig.PSignal2 V.Vector V.Vector Double,
    Sig.PSignal2 V.Vector V.Vector Double,
    Sig.NSignal2 V.Vector V.Vector Double )
makePics eqs tabEta tabPower socDrive = t
  where t = (state, optWater, optGas, etaSysMax)
        state = Sig.map fromIntegral $ Sig.argMax maxETACharge maxETADischarge

        optWater = combineOptimalMaps maxEtaSysState
                     powerWaterChargeOpt powerWaterDischargeOpt
        optGas = combineOptimalMaps maxEtaSysState
                     powerGasChargeOpt powerGasDischargeOpt

        maxEtaSysState :: Sig.UTSignal2 V.Vector V.Vector Double
        maxEtaSysState = Sig.map fromIntegral $
          Sig.argMax maxETACharge maxETADischarge

        etaSysMax :: Sig.NSignal2 V.Vector V.Vector Double
        etaSysMax = Sig.zipWith max maxETACharge maxETADischarge

        powerWaterChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerWaterChargeOpt = Sig.setType $
          Sig.map (ModUt.lookupAbsPower (XIdx.power sec0 Network Water)) envsChargeOpt


        powerWaterDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerWaterDischargeOpt = Sig.setType $
          Sig.map (ModUt.lookupAbsPower (XIdx.power sec1 Network Water)) envsDischargeOpt

        powerGasChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerGasChargeOpt = Sig.setType $
          Sig.map (ModUt.lookupAbsPower (XIdx.power sec0 LocalNetwork Gas))
                  envsChargeOpt


        powerGasDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerGasDischargeOpt = Sig.setType $
          Sig.map (ModUt.lookupAbsPower (XIdx.power sec1 LocalNetwork Gas))
                  envsDischargeOpt

        maxETACharge :: Sig.NSignal2 V.Vector V.Vector Double
        maxETACharge = Sig.setType $ Sig.map fst $
          Sig.map maxOptChargeFunc envsCharge

        maxETADischarge :: Sig.NSignal2 V.Vector V.Vector Double
        maxETADischarge = Sig.setType $ Sig.map fst $
          Sig.map maxOptDischargeFunc envsDischarge

        envsCharge =  Sig.map (Sig.map envGetData) $
          doubleSweep (Optimisation.solveCharge eqs)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

        envsDischarge = Sig.map (Sig.map envGetData) $
          doubleSweep (Optimisation.solveDischarge eqs)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

        maxOptChargeFunc = maxOpt System.seqTopoOpt True socDrive
        maxOptDischargeFunc = maxOpt System.seqTopoOpt False socDrive

        envsChargeOpt = Sig.map snd $ Sig.map maxOptChargeFunc envsCharge

        envsDischargeOpt = Sig.map snd $ Sig.map maxOptDischargeFunc envsDischarge

        etaFunc = CT.makeEtaFunctions2D scaleTableEta tabEta

{-

main :: IO ()
main = do

   IO.hSetEncoding IO.stdout IO.utf8
    
   tabEta <- Table.read "../simulation/maps/eta.txt"
   tabPower <- Table.read "../simulation/maps/power.txt"
   let eqs ::
         (a ~ EqArith.Scalar v, Eq a,
         Eq v, EqArith.Product a, EqArith.Product v, EqArith.Integrate v) =>
         EqGen.EquationSystem Node s a v
       eqs = EqGen.fromGraph True (TD.dirFromSequFlowGraph (snd System.seqTopoOpt))

       plotwaterpng n pic = 
         PlotIO.surfaceWithOpts "Optimal Water Power" 
           --(PNG.cons ("waterpics/optimal_water_power" ++ printf "%02d" n ++ ".png"))
           DefaultTerm.cons
           id 
           frameOpts noLegend varRestPower varLocalPower
           pic


       plotgaspng n pic = 
         PlotIO.surfaceWithOpts "Optimal Gas Power [W]" 
           --(PNG.cons ("gaspics/optimal_gas_power" ++ printf "%02d" n ++ ".png"))
           DefaultTerm.cons
           id 
           frameOpts noLegend varRestPower varLocalPower
           pic


       plotstatepng n pic = 
         PlotIO.surfaceWithOpts "Optimal State" 
           --(PNG.cons ("statepics/state_matrix" ++ printf "%02d" n ++ ".png"))
           DefaultTerm.cons
           id 
           frameOpts noLegend varRestPower varLocalPower
           pic

       plotetasyspng n pic = 
         PlotIO.surfaceWithOpts "System Efficiency [%]" 
           (PNG.cons ("etasys/etasys" ++ printf "%02d" n ++ ".png"))
           id 
           frameOpts noLegend varRestPower varLocalPower
           pic


       plotpng (n, x) =
         let (s, w, g, e) = makePics eqs tabEta tabPower x
         in  concurrentlyMany_ [
               -- plotstatepng n s
               plotwaterpng n w,
               plotgaspng n g ]




       (lst1, lst2) = splitAt 20 $ take 40 $ zip [0::Int, 1 ..] [0, 0.001 ..]

   concurrentlyMany_ [
     mapM_ plotpng [(lst1 ++ lst2) !! 30] ]
     -- mapM_ plotpng lst2 ]


-}

main :: IO ()
main = do

   IO.hSetEncoding IO.stdout IO.utf8

   tabEta <- Table.read "../simulation/maps/eta.txt"
   tabPower <- Table.read "../simulation/maps/power.txt"

   let eqsys ::
         (a ~ EqArith.Scalar v, Eq a, Eq v,
          EqArith.Constant a, EqArith.Product v, EqArith.Integrate v) =>
         EqGen.EquationSystem Node s a v
       eqsys = EqGen.fromGraph True (TD.dirFromSequFlowGraph (snd System.seqTopoOpt))

   -- |Import Efficiency Curves
   let etaFunc = CT.makeEtaFunctions2D scaleTableEta tabEta
       etaWaterCharge:etaWaterDischarge:etaGas:etaCoal:etaTransHL:_ =
         getEtas etaFunc ["storage", "storage", "gas", "coal", "transformer"]

   -- | Import Power Curves
   let (timeWind, powerSignalWind) :
         (timeSolar, powerSignalSolar) :
         (timeHouse, powerSignalHouse) :
         (timeIndustry, powerSignalIndustry) : _
           = getPowerSignals tabPower ["wind", "solar", "house", "industry"]


       powerSignalRest = Sig.scale powerSignalWind restPowerScale
       powerSignalLocal = Sig.offset
                          (Sig.scale  (powerSignalSolar Sig..+
                                      Sig.makeDelta (powerSignalHouse Sig..+
                                                     (Sig.makeDelta powerSignalIndustry)))
                          localPowerScale) 0.5

   let
    -- | Speep optimisation and operation space for charge and discharge case
     envsCharge ::
       Sig.UTSignal2 V.Vector V.Vector
         (Sig.UTSignal2 V.Vector V.Vector EnvDouble)
     envsCharge = Sig.map (Sig.map envGetData) $
       doubleSweep (Optimisation.solveCharge eqsys)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

     envsDischarge ::
       Sig.UTSignal2 V.Vector V.Vector
         (Sig.UTSignal2 V.Vector V.Vector EnvDouble)
     envsDischarge = Sig.map (Sig.map envGetData) $
       doubleSweep (Optimisation.solveDischarge eqsys)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

     -- | Get maximum Efficiency Envelope for charge and discharge
     --maxETACharge :: Sig.NSignal2 V.Vector V.Vector Double
     --maxETACharge = Sig.setType $ Sig.map fst $ Sig.map maxEta envsCharge

     --maxETADischarge :: Sig.NSignal2 V.Vector V.Vector Double
     --maxETADischarge = Sig.setType $ Sig.map fst $ Sig.map maxEta envsDischarge

     socDrive = 0.0
     maxOptChargeFunc = maxOpt System.seqTopoOpt True socDrive
     maxOptDischargeFunc = maxOpt System.seqTopoOpt False socDrive

     maxETACharge :: Sig.NSignal2 V.Vector V.Vector Double
     maxETACharge = Sig.setType $ Sig.map fst $
      Sig.map maxOptChargeFunc envsCharge

     maxETADischarge :: Sig.NSignal2 V.Vector V.Vector Double
     maxETADischarge = Sig.setType $ Sig.map fst $
       Sig.map maxOptDischargeFunc envsDischarge




     -- | Get maximum efficiency for both cases
     etaSysMax :: Sig.NSignal2 V.Vector V.Vector Double
     etaSysMax = Sig.zipWith max maxETACharge maxETADischarge

     maxEtaSysState :: Sig.UTSignal2 V.Vector V.Vector Double
     maxEtaSysState = Sig.map fromIntegral $
       Sig.argMax maxETACharge maxETADischarge


     -- | Get the correspondig optimal envs for both states

     envsChargeOpt :: Sig.UTSignal2 V.Vector V.Vector (Maybe EnvDouble)
     envsChargeOpt = Sig.map snd $ Sig.map maxOptChargeFunc envsCharge

     envsDischargeOpt :: Sig.UTSignal2 V.Vector V.Vector (Maybe EnvDouble)
     envsDischargeOpt = Sig.map snd $ Sig.map maxOptDischargeFunc envsDischarge


     powerGasChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerGasChargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec0 LocalNetwork Gas)) envsChargeOpt

     powerWaterChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerWaterChargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec0 Network Water)) envsChargeOpt

     powerGasDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerGasDischargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec1 LocalNetwork Gas)) envsDischargeOpt

     powerWaterDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerWaterDischargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec1 Network Water)) envsDischargeOpt

     powerTransformerChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerChargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec0 Network LocalNetwork)) envsChargeOpt

     powerTransformerDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerDischargeOpt = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec1 Network LocalNetwork)) envsDischargeOpt

     powerTransformerChargeOptLV :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerChargeOptLV = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec0 LocalNetwork Network)) envsChargeOpt

     powerTransformerDischargeOptLV :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerDischargeOptLV = Sig.setType $
       Sig.map (ModUt.lookupAbsPower (XIdx.power sec1 LocalNetwork Network)) envsDischargeOpt

     ------------------------------------------------------------------------------------------
     -- | Start Simulation Here

     -- | Generate optimal control signals and build power record

     powerSignalWaterOptCharge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalWaterOptCharge"
           varRestPower1D varLocalPower powerWaterChargeOpt x y)
         powerSignalRest powerSignalLocal

     powerSignalGasOptCharge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalGasOptCharge"
           varRestPower1D varLocalPower powerGasChargeOpt x y)
         powerSignalRest powerSignalLocal

     powerSignalWaterOptDischarge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalWaterOptDischarge"
           varRestPower1D varLocalPower powerWaterDischargeOpt x y)
         powerSignalRest powerSignalLocal

     powerSignalGasOptDischarge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalGasOptDischarge"
           varRestPower1D varLocalPower powerGasDischargeOpt x y)
         powerSignalRest powerSignalLocal

     stateSignal =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "stateSignal"
           varRestPower1D varLocalPower maxEtaSysState x y)
         powerSignalRest powerSignalLocal

     powerSignalWater =
       Sig.zipWith (\state (x, y) -> if state==0 then x else -y)
         stateSignal (Sig.zip powerSignalWaterOptCharge powerSignalWaterOptDischarge)

     powerSignalGas =
       Sig.zipWith (\state (x, y) -> if state==0 then x else y)
         stateSignal (Sig.zip powerSignalGasOptCharge powerSignalGasOptDischarge)

     time' :: Sig.TSignal [] Double
     time' = Sig.fromList [0 .. 23]

     rec :: Record.PowerRecord Node [] Double
     rec = addZeroCrossings $
           -- Record.diffTime $
           -- Record.rmap (Sig.changeSignalType . Sig.deltaMap (\x y -> (x+y)/2)) $
           Record.Record time' $
           Map.fromList [(TIdx.PPos (TIdx.StructureEdge Rest Network), powerSignalRest),
                       (TIdx.PPos (TIdx.StructureEdge LocalRest LocalNetwork), powerSignalLocal),
                       (TIdx.PPos (TIdx.StructureEdge Network Water), powerSignalWater),
                       (TIdx.PPos (TIdx.StructureEdge LocalNetwork Gas), powerSignalGas)
                      ]


     recConsumers :: Record.PowerRecord Node [] Double
     recConsumers = addZeroCrossings $
           -- Record.diffTime $
           -- Record.rmap (Sig.changeSignalType . Sig.deltaMap (\x y -> (x+y)/2)) $
           Record.Record time' $
           Map.fromList [(TIdx.PPos (TIdx.StructureEdge Rest Network), powerSignalRest),
                       (TIdx.PPos (TIdx.StructureEdge LocalRest LocalNetwork), powerSignalLocal)
                      ]

{-
   PlotIO.recordList "after zero" DefaultTerm.cons show id
                    [ (Record.Name "ohne zero", rec),
                      (Record.Name "mit zero", addZeroCrossings rec) ]
   let
-}

     time = Record.getTime rec

     -- | Build Sequenceflow graph for simulation
     seqTopoSim = Flow.mkSequenceTopology (select System.flowStatesOpt [4])

     -- | Generate and solve Equation System

     eqs :: EqGen.EquationSystem Node s (Data Nil Double) (Data ([] :> Nil) Double)
     eqs = Optimisation.givenSimulate etaAssign etaFunc $
             SD.SequData [SD.Section (TIdx.Section 0) undefined rec]

     envSim = EqGen.solve seqTopoSim eqs


     -- | extract power record from simulation env to allow subsequent EFA
     powerRecSim = ModUt.envToPowerRecord envSim time (TIdx.Section 0)

     -- | flip signs of power signals at water edge, as edge flips direction between state 0 and 4
     flipwater (TIdx.PPos (TIdx.StructureEdge Network Water)) x = Sig.neg x
     flipwater (TIdx.PPos (TIdx.StructureEdge Water Network)) x = Sig.neg x
     flipwater _ x = x
     powerRecSimCorr = Record.rmapWithKey flipwater powerRecSim

     -- | make efa on simulation results
     rec0 = addZeroCrossings powerRecSimCorr

     sectionFilterTime ::  TC Scalar (Typ A T Tt) (Data Nil Double)
     sectionFilterTime = Sig.toScalar 0

     sectionFilterEnergy ::  TC Scalar (Typ A F Tt) (Data Nil Double)
     sectionFilterEnergy = Sig.toScalar 0

     sequencePowers :: SD.SequData (Record.PowerRecord System.Node [] Double)
     sequencePowers = genSequ rec0
     (sequencePowersFilt, sequenceFlowsFilt) =
         SD.unzip $
         SD.filter (Record.major sectionFilterEnergy sectionFilterTime . snd) $
         fmap (\x -> (x, Record.partIntegrate x)) sequencePowers
   
     (flowStates, adjustedFlows) =
         SD.unzip $
         fmap
         (\state ->
           let flowState = Flow.genFlowState state
           in  (flowState, Flow.adjustSigns System.topologyOpt flowState state))
         sequenceFlowsFilt

     flowTopos = Flow.genSequFlowTops System.topologyOpt flowStates
     sequenceFlowTopologySim = makeSeqFlowTopology flowTopos
     envSimAnalysis = Analysis.external2 sequenceFlowTopologySim adjustedFlows
     envSimAnalysisCumulated = Analysis.external2 sequenceFlowTopologySim
                                 (fmap Record.sumFlowRecord adjustedFlows)

 {-
   concurrentlyMany_ [
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt
                  (fromJust (Sig.getSample2D envsChargeOpt (Sig.SignalIdx 0, Sig.SignalIdx 0))),


     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt
                  (Sig.getSample2D (Sig.getSample2D envsDischarge (Sig.SignalIdx 1, Sig.SignalIdx 1)) (Sig.SignalIdx 1, Sig.SignalIdx 1)) ]

-}
   let optGas = combineOptimalMaps maxEtaSysState powerGasChargeOpt powerGasDischargeOpt
       optWater = combineOptimalMaps maxEtaSysState powerWaterChargeOpt powerWaterDischargeOpt


   concurrentlyMany_ $ [
     --putStrLn ("Storage Balance: " ++ show (ES.balance sequenceFlowTopologySim envSimAnalysisCumulated)),

{-

     Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNamesOpt System.topologyOpt,

     putStrLn ("Number of possible flow states: " ++ show (length System.flowStatesOpt)),
     Draw.xterm $ Draw.flowTopologies (take 20 System.flowStatesOpt),
-}
     Draw.xterm $ Draw.sequFlowGraph System.seqTopoOpt, 
{-
     PlotIO.surfaceWithOpts "Optimal System Efficiency" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower etaSysMax,


     PlotIO.surfaceWithOpts "Optimal State" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower maxEtaSysState,
  
     PlotIO.surfaceWithOpts "Charging Optimal System Efficiency " DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower maxETACharge,
     PlotIO.surfaceWithOpts "Discharging Optimal System Efficiency " DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower maxETADischarge,

     --PlotIO.surface "Discharging Optimal System Efficiency " DefaultTerm.cons id noLegend varRestPower varLocalPower maxETADischarge,



     PlotIO.surfaceWithOpts "Charging Optimal Gas Power" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerGasChargeOpt,
     PlotIO.surfaceWithOpts "Charging Optimal Water Power" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerWaterChargeOpt,

    
--     PlotIO.surface "Discharging" DefaultTerm.cons id noLegend varRestPower varLocalPower maxETADischarge,


     PlotIO.surfaceWithOpts "Discharging Optimal Gas Power " DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerGasDischargeOpt,
     PlotIO.surfaceWithOpts "Discharging Optimal Water Power " DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerWaterDischargeOpt,

     PlotIO.surfaceWithOpts "Optimal Water Power" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower optWater,
     PlotIO.surfaceWithOpts "Optimal Gas Power" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower optGas,

     
     --PlotIO.surface "Transformer Power Charge HV " DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerChargeOpt,
     --PlotIO.surface "Transformer Power DisCharge HV" DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerDischargeOpt,
     
     --PlotIO.surface "Transformer Power Charge LV " DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerChargeOptLV,
     --PlotIO.surface "Transformer Power DisCharge LV" DefaultTerm.cons id noLegend varRestPower varLocalPower powerTransformerDischargeOptLV,

     -- PlotIO.xy "Operation" DefaultTerm.cons id show powerSignalRest powerSignalLocal,

   
     report [] ("RestPower", varRestPower),
     report [] ("LocalPower", varLocalPower),
     report [] ("waterPower", varWaterPower),
     report [] ("gasPower", varGasPower),
     report [RAll] ("powerRecordSim", powerRecSim),
     report [RAll] ("rec0", rec0),
     report [RAll] ("rec", rec),
-}
     PlotIO.record "Calculated Signals" DefaultTerm.cons show id rec,

{-
     PlotIO.record "Local / Rest" DefaultTerm.cons show id recConsumers,

     PlotIO.record "Simulation Result" DefaultTerm.cons show id powerRecSim,
     
     PlotIO.signal "State"  DefaultTerm.cons id stateSignal ,

     PlotIO.signal "Interpolated Signals"  DefaultTerm.cons id [powerSignalWaterOptCharge, 
                                               powerSignalWaterOptDischarge,
                                               powerSignalGasOptCharge, 
                                               powerSignalGasOptDischarge],
-}
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv  sequenceFlowTopologySim envSimAnalysis,
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv  seqTopoSim envSim,

     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv sequenceFlowTopologySim envSimAnalysisCumulated,
 
       return ()
     ]
