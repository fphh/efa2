module Main where

--import qualified Modules.Signals as Signals
import qualified Modules.System as System
import Modules.System (Node(..))


--import qualified EFA.Signal.Record as Record
import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Draw as Draw
--import EFA.IO.PLTImport (modelicaPLTImport)
import qualified EFA.Signal.Signal as Sig
--import EFA.Signal.Data (Data(..), Nil)
--import EFA.Signal.Typ (Typ, F, T, A, Tt)
import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Utility as Utility
--import EFA.Signal.Sequence (makeSeqFlowTopology)
import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Graph.Draw as Draw
--import EFA.Graph(lefilter)
--import qualified EFA.Signal.Plot as Plot
--import EFA.Graph.Topology(isStructureEdge)
--import qualified EFA.Example.Index as XIdx
--import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Equation.Environment as Env
import qualified EFA.Signal.ConvertTable as Table
import qualified EFA.Example.Absolute as EqGen
import qualified EFA.Example.Index as XIdx
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import EFA.Equation.Result (Result(..))
import EFA.Utility.Map (checkedLookup)
import qualified EFA.Signal.PlotIO as PlotIO

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified System.IO as IO
--import System.Environment (getEnv)
--import System.FilePath ((</>))

import qualified EFA.Utility.Stream as Stream

import Data.Monoid (mconcat, (<>))
import EFA.Example.Absolute ( (.=), (%=), (=.=) )
import EFA.Utility.Stream (Stream((:~)))

--import qualified Data.List as L
--import qualified Data.Map as M
--import qualified Modules.Analysis as Analysis
--import Data.Tuple.HT (mapSnd)


plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

colours :: [Colors.X11Color]
colours = [ Colors.White,	
            Colors.Gray90,	
            Colors.Gray80,	
            Colors.Gray70 ]

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
   (XIdx.storage TIdx.Initial Water .= 0) :
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

etaSys ::
  EqEnv.Complete
    System.Node
    (EqRec.Absolute (Result Double))
    (EqRec.Absolute (Result Double)) ->
  Double
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

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"

main :: IO ()
main = do

  IO.hSetEncoding IO.stdout IO.utf8

{-  let select :: [topo] -> [Int] -> SD.SequData topo
      select ts = SD.fromList . map (ts !!)

      seqTopo = Flow.mkSequenceTopology (select System.flowStatesOpt [4,0])
-}
  concurrentlyMany_ $ [
    Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNamesOpt System.topologyOpt,
    putStrLn ("Number of possible flow states: " ++ show (length System.flowStatesOpt)),
    Draw.xterm $ Draw.flowTopologies (take 20 System.flowStatesOpt),
    Draw.xterm $ Draw.sequFlowGraph seqTopo,
    Draw.xterm $ Draw.title "Optimise Charging" $ Draw.sequFlowGraphAbsWithEnv seqTopo $ EqGen.solve seqTopo $ givenOptimiseCharging 1 1 1 1,
    Draw.xterm $ Draw.title "Optimise Discharging" $ Draw.sequFlowGraphAbsWithEnv seqTopo $ EqGen.solve seqTopo $ givenOptimiseDischarging 1 1 1 1,
    PlotIO.surface "Systemwirkungsgrad Entladen" DefaultTerm.cons id (const "") varX varY etaSysCharge,
    PlotIO.surface "Systemwirkungsgrad Laden" DefaultTerm.cons id (const "") varX varY etaSysDischarge,
    PlotIO.surface "Systemwirkungsgrad Laden und Entladen" DefaultTerm.cons id legend varX varY [etaSysCharge, etaSysDischarge]]



