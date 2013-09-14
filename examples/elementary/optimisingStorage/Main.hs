{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Application.Topology.TripodB as Tripod
import EFA.Application.Topology.TripodB (Node, node0, node1, node2, node3)
import EFA.Application.Utility ( seqFlowGraphFromStates, checkDetermined )

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((=.=))

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified EFA.Signal.PlotIO as PlotIO
import qualified EFA.Signal.ConvertTable as Table
import qualified EFA.Signal.Signal as S
-- import qualified EFA.Signal.Data as D

import EFA.Signal.Signal(Test2, (.+), (.-), (./))
import EFA.Signal.Typ (A, P, Tt, F, N, Typ, Y)
import EFA.Signal.Base(Val)

import qualified EFA.Report.Report as Rep

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Stream (Stream((:~)))

import qualified Graphics.Gnuplot.Terminal.WXT as WXT

import qualified Data.Vector.Unboxed as UV

import Control.Category ((.))
import Data.Monoid ((<>))

import Prelude hiding ((.))


plotTerm :: WXT.T
plotTerm = WXT.cons

sec0, sec1 :: Idx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ Idx.Section 0


flowGraph :: SeqFlow.Graph Node (Result a) (Result v)
flowGraph = seqFlowGraphFromStates Tripod.topology [0, 4]


type Expr s a = EqSys.ExpressionIgnore Node s Double Double a
type ExpVar s = Expr s Double

etaf :: (Arith.Sum a, Floating a) => Expr s a -> Expr s a
etaf =
   EqSys.liftF $ \x ->
      let y = x/100
      in  1/((y+sqrt(y*y+4*y))/(2*y))

n01, n12, n13, n31, p10, p21, e31, e21, p31, p13 ::
  Idx.Section -> Expr s Double
n01 sec = EqSys.variable $ XIdx.eta sec node0 node1
n12 sec = EqSys.variable $ XIdx.eta sec node1 node2
n13 sec = EqSys.variable $ XIdx.eta sec node1 node3
n31 sec = EqSys.variable $ XIdx.eta sec node3 node1
p10 sec = EqSys.variable $ XIdx.power sec node1 node0
p21 sec = EqSys.variable $ XIdx.power sec node2 node1
e31 sec = EqSys.variable $ XIdx.energy sec node3 node1
e21 sec = EqSys.variable $ XIdx.energy sec node2 node1

p31 sec = EqSys.variable $ XIdx.power sec node3 node1
p13 sec = EqSys.variable $ XIdx.power sec node1 node3


stoinit :: Expr s Double
stoinit = EqSys.variable $ XIdx.storage Idx.initial node3

ein, eout0, eout1 :: XIdx.Energy Node
ein = XIdx.energy sec0 node0 node1
eout0 = XIdx.energy sec0 node2 node1
eout1 = XIdx.energy sec1 node2 node1

e33 :: Expr s Double
e33 = EqSys.variable $ XIdx.stEnergy XIdx.initSection sec1 node3

time :: Idx.Section -> Expr s Double
time = EqSys.variable . XIdx.dTime


-- maybe move this to Utility module
(^!) :: Num a => a -> Int -> a
(^!) = (^)

f ::
   (Arith.Sum a, Floating a) =>
   Expr s a -> Expr s a -> Expr s a
f =
   EqSys.liftF2 $ \r x ->
      let -- r = 0.9
          ui = 200
      in  x/(x + r*((ui - sqrt(ui^!2 - 4*r*x)) / 2*r)^!2)


-- | Provide time of sec1 and inner resistance of battery
given :: Val ->
         Val ->
         Val ->
         EqSys.EquationSystemIgnore Node s Val Val
given y' p' nParam' =
  (stoinit =.= EqSys.constant 3)

  <> (p21 sec0 =.= p)
  <> (e21 sec0 =.= (1-y)*(t*p))
  <> (e31 sec0 =.= e31 sec1)

  <> (n01 sec0 =.= fn01_p10 (p10 sec0))
  <> (n12 sec0 =.= 1)
  <> (n13 sec0 =.= (fn13_p31 nParam (p31 sec0)))

  <> (e21 sec1 =.= y*(1*p))
  <> (p21 sec1 =.= p)
  <> (n12 sec1 =.= 1)
  <> (n31 sec1 =.= fn31_p13 nParam (p13 sec1))
  where y = EqSys.constant y'
        nParam = EqSys.constant nParam'
        t = EqSys.constant 1
        p = EqSys.constant p'


type AbsoluteResult = Result Val

-- | r is inner Resistance of Battery
solve ::
   Val -> Val -> Val ->
   SeqFlow.Graph Node AbsoluteResult AbsoluteResult
solve nPar y p = EqSys.solve flowGraph (given y p nPar)


-- | Checked Lookup
getSignalVar ::
   (SeqFlow.LookupSignal idx,
    Show a, UV.Unbox a) =>
   [[SeqFlow.Graph Node (Result a) (Result a)]] ->
   Idx.InSection idx Node -> Test2 (Typ A u Tt) a
getSignalVar varEnvs idx =
   S.changeSignalType $ S.fromList2 $
   map (map (checkDetermined "getSignalVar" .
             Var.checkedLookup "getSignalVar" SeqFlow.lookupSignal idx)) $
   varEnvs


getSignalVarEnergy ::
   [[SeqFlow.Graph Node AbsoluteResult AbsoluteResult]] ->
   XIdx.Energy Node -> Test2 (Typ A F Tt) Val
getSignalVarEnergy = getSignalVar

getSignalVarPower ::
   [[SeqFlow.Graph Node AbsoluteResult AbsoluteResult]] ->
   XIdx.Power Node -> Test2 (Typ A P Tt) Val
getSignalVarPower = getSignalVar

getSignalVarEta ::
   [[SeqFlow.Graph Node AbsoluteResult AbsoluteResult]] ->
   XIdx.Eta Node -> Test2 (Typ A N Tt) Val
getSignalVarEta = getSignalVar

-- ##############################
-- | Setting come here

-- | Variation ranges


yrange, rrange, prange :: [Double]

-- | Variation of time which is equal to time share as time is one
yrange = [0.1,0.2 .. 0.9]

-- | Variation of Resistance
rrange = 0.01:[0.1,0.2 .. 2]

-- | Variation of Power Demand as Sink
prange = [0.1,0.2 .. 1] -- 10 bis 100 kW

batteryResistance :: Double
batteryResistance = 0.2

nsto_const :: Fractional a => a
nsto_const = 0.95

-- | Choose Variation here
varX',varY' :: [[Double]]
(varX', varY') = Table.varMat yrange prange

-- | Set the type for Graph Display here -- take UT for Resistance

varX :: S.Test2 (Typ A Y Tt) Double
varX = S.fromList2 varX'

varY :: S.Test2 (Typ A P Tt) Double
varY = S.fromList2 varY'


-- | ## Fuel Converter Curves
{-
-- | Lookup Map ic-engine -- forward - convert to backward
p_ic_fw = S.fromList [0,0.1 .. 1] :: S.Signal (Typ UT UT UT) Double
eta_ic = S.fromList [0,0.7,0.9,0.95,0.97,1,0.97,0.95,0.9,0.85,0.8] :: S.Signal (Typ UT UT UT) Double
feta_ic :: ExpVar s -> ExpVar s
feta_ic (EqSys.ExprWithVars x) = EqSys.constant y
  where (S.TC (D.Data y)) = S.interp1Lin (p_ic_fw.*eta_ic) eta_ic x
-}

-- | ic-engine - relative variable efficiency (max = 1) of an ic engine type fuel converter: n01 as function of p10
fn01_p10_ic :: ExpVar s -> ExpVar s
fn01_p10_ic =
   EqSys.liftF $ \x ->
      let y = x
      in  1/((y+sqrt(y*y+4*y))/(2*y))

-- | fuel cell - relative variable efficiency fuel cell type
fn01_p10_fc :: ExpVar s ->  ExpVar s ->  ExpVar s
fn01_p10_fc =
   EqSys.liftF2 $ \x r ->
      let ui = 1
      in  x/(x + r*((ui - sqrt(ui^!2 - 4*r*x)) / 2*r)^!2)

fn01_p10 ::    ExpVar s -> ExpVar s
fn01_p10 = fn01_p10_ic  -- Choose fuel converter type here
-- fn01_p10 = fn01_p10_fc 0.01

-- | ## Storage Efficiency Curves

-- ## Battery
-- | variable efficiency of energy storage in discharging mode as function of n31 over p13
fn31_p13_ba :: ExpVar s ->  ExpVar s ->  ExpVar s
fn31_p13_ba =
   EqSys.liftF2 $ \ r x ->
      let ui = 1
      in  nsto_const*x/(x + r*((ui - sqrt(ui^!2 - 4*r*x)) / 2*r)^!2)

-- | variable efficiency of energy storage in charging mode as function of n13 over p31
fn13_p31_ba ::  ExpVar s ->  ExpVar s ->  ExpVar s
fn13_p31_ba r x = nsto_const / (1 + (x*r)/(ui^!2))
  where ui = 1

-- ## HyperCaps (Capacitors)
-- |  simply very good constant efficiency
fn31_p13_HC :: ExpVar s ->  ExpVar s ->  ExpVar s
fn31_p13_HC _ _ = 0.99

fn13_p31_HC :: ExpVar s ->  ExpVar s ->  ExpVar s
fn13_p31_HC _ _ = 0.99


-- ## Electric Flywheel
-- | Lookup
-- Power [0,0.1,0.2 ,0.5 ,0.7 ,1  ]
-- Eta   [0,0.9,0.93,0.95,0.93,0.9]

-- |  compareable to ic engine
fn31_p13_EF :: ExpVar s ->  ExpVar s ->  ExpVar s
fn31_p13_EF =
   EqSys.liftF2 $ \_ x ->
      let y = x
      in  1/((y+sqrt(y*y+4*y))/(2*y))

-- |  compareable to ic engine
fn13_p31_EF :: ExpVar s ->  ExpVar s ->  ExpVar s
fn13_p31_EF =
   EqSys.liftF2 $ \_ x ->
      let y=x
      in  1/((y+sqrt(y*y+4*y))/(2*y))

fn31_p13, fn13_p31:: ExpVar s ->  ExpVar s ->  ExpVar s
fn31_p13 = fn13_p31_ba
fn13_p31 = fn13_p31_ba


main :: IO ()
main = do
  let

      varEnvs = zipWith (zipWith (solve batteryResistance)) varX' varY'

      -- get Energies
      eoutVar0 = getSignalVarEnergy varEnvs eout0
      eoutVar1 = getSignalVarEnergy varEnvs eout1
  --Rep.report  [] ("eoutVar1",eoutVar1)

      varEout = eoutVar0 .+ (S.makeDelta eoutVar1)
      einVar = getSignalVarEnergy varEnvs ein

      -- calculate split share and system efficiency
      etaSysVar = (eoutVar0 .+ (S.makeDelta eoutVar1))./einVar
--      varY = S.changeType $ eoutVar1 ./ (eoutVar0 .+ (S.makeDelta eoutVar1)) :: S.Test2 (Typ A X Tt) Double

      -- calculate Losses
      varLossA = varE01 .- varE10
      varLossB = varE10 .- varEout
      varLoss = varE01 .- varEout

      -- Get more Env values
      varN13 = getSignalVarEta varEnvs (XIdx.eta sec0 node1 node3)
      varN31 = getSignalVarEta varEnvs (XIdx.eta sec1 node3 node1)
      varN01 = getSignalVarEta varEnvs (XIdx.eta sec0 node0 node1)

      varP31_0 = getSignalVarPower varEnvs (XIdx.power sec0 node3 node1)
      varP31_1 = getSignalVarPower varEnvs (XIdx.power sec1 node3 node1)


      varP13_0 = getSignalVarPower varEnvs (XIdx.power sec0 node1 node3)
      varP13_1 = getSignalVarPower varEnvs (XIdx.power sec1 node1 node3)

      varP10 = getSignalVarPower varEnvs (XIdx.power sec0 node1 node0)
      varP01 = getSignalVarPower varEnvs (XIdx.power sec0 node0 node1)

      varE10 = getSignalVarEnergy varEnvs (XIdx.energy sec0 node1 node0)
      varE01 = getSignalVarEnergy varEnvs (XIdx.energy sec0 node0 node1)

      varPout0 = getSignalVarPower varEnvs (XIdx.power sec0 node2 node1)
      varPout1 = getSignalVarPower varEnvs (XIdx.power sec1 node2 node1)

      -- create curve of n01 in used power range
      p10Lin = S.concat varP10
      n01Lin = S.concat varN01

      -- fuel converter
      (p10Lin', n01Lin') = S.sortTwo (p10Lin,n01Lin)

      -- discharging
--      (p131Lin', n31Lin') = S.sortTwo (p131Lin,n31Lin)

      -- charging
--      (p310Lin', n13Lin') = S.sortTwo (p310Lin,n13Lin)

-- ##################################
--  Debug Plots


  -- Plots to check the variation

  PlotIO.surface "varX"  plotTerm (\_ -> "") varX varY varX
  Rep.report [] ("varX",varX)

  PlotIO.surface "varY" plotTerm (\_ -> "")  varX varY varY
  Rep.report [] ("vary",varY)

  -- Plot to check consumer behaviour

  PlotIO.surface "Eout" plotTerm (\_ -> "")  varX varY (eoutVar0 .+ (S.makeDelta eoutVar1))
  Rep.report [] ("Eout",(eoutVar0 .+ (S.makeDelta eoutVar1)))

  PlotIO.surface "Pout0" plotTerm (\_ -> "")  varX varY varPout0
  Rep.report [] ("Pout0",varPout0)

  PlotIO.surface "Pout1" plotTerm (\_ -> "")  varX varY varPout1
  Rep.report [] ("Pout1",varPout1)


  -- Plots to check variable efficiency at fuel converter

  PlotIO.surface "N01" plotTerm (\_ -> "")  varX varY varN01


  PlotIO.surface "P10" plotTerm (\_ -> "")  varX varY varP10

  PlotIO.surface "P01" plotTerm (\_ -> "")  varX varY varP01

  PlotIO.xy "N01 - Curve"  plotTerm id (\_ -> "efficiency N01") p10Lin' n01Lin'

  -- Plots to check variable efficiency at storage -- charging
  PlotIO.surface "P13_0 - externe Ladeleistung" plotTerm (\_ -> "") varX varY varP13_0
  Rep.report [] ("varP13_0",varP13_0)

  PlotIO.surface "P31_0 - interne LadeLeistung" plotTerm (\_ -> "") varX varY varP31_0
  Rep.report [] ("varP31_0",varP31_0)

  PlotIO.surface "N13 - Charging" plotTerm (\_ -> "") varP31_0 varY varN13
  PlotIO.xy "N13 - Charging"  plotTerm id (\_ -> "efficiency N13") varP31_0 varN13
  Rep.report  [] ("N13 - Charging",varN13)


  PlotIO.surface "P13_1 - externe Entladeleistung" plotTerm (\_ -> "") varX varY varP13_1
  Rep.report [] ("varP13_1",varP13_1)

  PlotIO.surface "P31_1 - interne EntladeLeistung" plotTerm (\_ -> "") varX varY varP31_1
  Rep.report [] ("varP31_1",varP31_1)

  Rep.report  [] ("N31 - Discharging",varN31)
  PlotIO.surface  "N31 - Discharging" plotTerm (\_ -> "")  varP13_1 varY varN31
  PlotIO.xy "N31 - Discharging" plotTerm id (\_ -> "efficiency N31") varP13_1 varN31


  -- Check Losses

  -- Loss of N01
  PlotIO.surface "LossA" plotTerm (\_ -> "") varX varY varLossA

  -- Loss of the Rest of the system
  PlotIO.surface "LossB" plotTerm (\_ -> "") varX varY varLossB

  -- Total System Loss
  PlotIO.surface "Loss" plotTerm (\_ -> "") varX varY varLoss

  -- System loss in curves over split variation for multiple resistance values
  PlotIO.xy "Loss"  plotTerm id (\_ -> "Loss") varX varLoss

  -- Total System Efficiency
  Rep.report  [] ("EtaSys",etaSysVar)
  PlotIO.surface "EtaSys" plotTerm (\_ -> "") varX varY etaSysVar
  PlotIO.xy "EtaSys" plotTerm id (\_ -> "EtaSys") varX etaSysVar -- System efficiency in curves over split variation for multiple resistance values

-- ##################################



  let envhh = head $ head varEnvs
      envhl = head $ last varEnvs
      envlh = last $ head varEnvs
      envll = last $ last varEnvs


  concurrentlyMany_ $
    map ( Draw.xterm
          . Draw.title "Topology"
          . Draw.sequFlowGraph Draw.optionsDefault )
        [envhh,envhl, envlh, envll]
