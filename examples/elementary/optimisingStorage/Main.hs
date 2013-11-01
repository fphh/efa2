{-# LANGUAGE TypeFamilies #-}
module Main where

import EFA.Example.Topology.Tripod.State (flowGraph, sec0, sec1)
import EFA.Example.Topology.Tripod (Node, node0, node1, node2, node3)

import qualified EFA.Application.Plot as PlotIO
import EFA.Application.Utility (checkDetermined)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((=.=))

import qualified EFA.Flow.SequenceState.Variable as Var
import EFA.Equation.Arithmetic ((^!))
import EFA.Equation.Result (Result)

import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.Signal.Signal as S

import EFA.Signal.Signal(Test2, (.+), (.-), (./))
import EFA.Signal.Typ (A, P, Tt, F, N, Typ, Y)

import qualified EFA.Report.Report as Rep

import EFA.Utility.Async (concurrentlyMany_)

import qualified Graphics.Gnuplot.Terminal.WXT as WXT

import qualified Data.Vector.Unboxed as UV

import Data.Monoid ((<>))


plotTerm :: WXT.T
plotTerm = WXT.cons


type Expr s a = EqSys.ExpressionIgnore Node s Double Double a
type ExpVar s = Expr s Double


n02, n21, n23, n32, p20, p12, e32, e12, p32, p23 ::
  Idx.Section -> Expr s Double
n02 sec = EqSys.variable $ XIdx.eta sec node0 node2
n21 sec = EqSys.variable $ XIdx.eta sec node2 node1
n23 sec = EqSys.variable $ XIdx.eta sec node2 node3
n32 sec = EqSys.variable $ XIdx.eta sec node3 node2
p20 sec = EqSys.variable $ XIdx.power sec node2 node0
p12 sec = EqSys.variable $ XIdx.power sec node1 node2
e32 sec = EqSys.variable $ XIdx.energy sec node3 node2
e12 sec = EqSys.variable $ XIdx.energy sec node1 node2

p32 sec = EqSys.variable $ XIdx.power sec node3 node2
p23 sec = EqSys.variable $ XIdx.power sec node2 node3


stoinit :: Expr s Double
stoinit = EqSys.variable $ XIdx.storage Idx.initial node3

ein, eout0, eout1 :: XIdx.Energy Node
ein = XIdx.energy sec0 node0 node2
eout0 = XIdx.energy sec0 node1 node2
eout1 = XIdx.energy sec1 node1 node2


-- | Provide time of sec1 and inner resistance of battery
given :: Double ->
         Double ->
         Double ->
         EqSys.EquationSystemIgnore Node s Double Double
given y' p' nParam' =
  (stoinit =.= EqSys.constant 3)

  <> (p12 sec0 =.= p)
  <> (e12 sec0 =.= (1-y)*(t*p))
  <> (e32 sec0 =.= e32 sec1)

  <> (n02 sec0 =.= fn02_p20 (p20 sec0))
  <> (n21 sec0 =.= 1)
  <> (n23 sec0 =.= (fn23_p32 nParam (p32 sec0)))

  <> (e12 sec1 =.= y*p)
  <> (p12 sec1 =.= p)
  <> (n21 sec1 =.= 1)
  <> (n32 sec1 =.= fn32_p23 nParam (p23 sec1))
  where y = EqSys.constant y'
        nParam = EqSys.constant nParam'
        t = EqSys.constant 1
        p = EqSys.constant p'


type AbsoluteResult = Result Double

-- | r is inner Resistance of Battery
solve ::
   Double -> Double -> Double ->
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
   XIdx.Energy Node -> Test2 (Typ A F Tt) Double
getSignalVarEnergy = getSignalVar

getSignalVarPower ::
   [[SeqFlow.Graph Node AbsoluteResult AbsoluteResult]] ->
   XIdx.Power Node -> Test2 (Typ A P Tt) Double
getSignalVarPower = getSignalVar

getSignalVarEta ::
   [[SeqFlow.Graph Node AbsoluteResult AbsoluteResult]] ->
   XIdx.Eta Node -> Test2 (Typ A N Tt) Double
getSignalVarEta = getSignalVar


-- * Setting comes here

-- ** Variation ranges

-- | Variation of time which is equal to time share as time is one
yrange :: [Double]
yrange = [0.1,0.2 .. 0.9]

-- | Variation of Power Demand as Sink
prange :: [Double]
prange = [0.1,0.2 .. 1] -- 10 bis 100 kW

batteryResistance :: Double
batteryResistance = 0.2

nsto_const :: Fractional a => a
nsto_const = 0.95

-- | Choose Variation here
varX',varY' :: [[Double]]
(varX', varY') = CT.varMat yrange prange

-- | Set the type for Graph Display here -- take UT for Resistance

varX :: S.Test2 (Typ A Y Tt) Double
varX = S.fromList2 varX'

varY :: S.Test2 (Typ A P Tt) Double
varY = S.fromList2 varY'


-- * Fuel Converter Curves

-- | ic-engine - relative variable efficiency (max = 1) of an ic engine type fuel converter: n02 as function of p20
fn02_p20_ic :: ExpVar s -> ExpVar s
fn02_p20_ic =
   EqSys.liftF $ \x ->
      let y = x
      in  1/((y+sqrt(y*y+4*y))/(2*y))

-- | fuel cell - relative variable efficiency fuel cell type
fn02_p20_fc :: ExpVar s ->  ExpVar s ->  ExpVar s
fn02_p20_fc =
   EqSys.liftF2 $ \x r ->
      let ui = 1
      in  x/(x + r*((ui - sqrt(ui^!2 - 4*r*x)) / 2*r)^!2)

fn02_p20 ::    ExpVar s -> ExpVar s
fn02_p20 = fn02_p20_ic  -- Choose fuel converter type here


-- * Storage Efficiency Curves

-- ## Battery
-- | variable efficiency of energy storage in discharging mode as function of n32 over p23
fn32_p23_ba :: ExpVar s ->  ExpVar s ->  ExpVar s
fn32_p23_ba =
   EqSys.liftF2 $ \ r x ->
      let ui = 1
      in  nsto_const*x/(x + r*((ui - sqrt(ui^!2 - 4*r*x)) / 2*r)^!2)

-- | variable efficiency of energy storage in charging mode as function of n23 over p32
fn23_p32_ba ::  ExpVar s ->  ExpVar s ->  ExpVar s
fn23_p32_ba r x = nsto_const / (1 + (x*r)/(ui^!2))
  where ui = 1

-- ## HyperCaps (Capacitors)
-- |  simply very good constant efficiency
fn32_p23_HC :: ExpVar s ->  ExpVar s ->  ExpVar s
fn32_p23_HC _ _ = 0.99

fn23_p32_HC :: ExpVar s ->  ExpVar s ->  ExpVar s
fn23_p32_HC _ _ = 0.99


-- ## Electric Flywheel
-- * Lookup

-- |  compareable to ic engine
fn32_p23_EF :: ExpVar s ->  ExpVar s ->  ExpVar s
fn32_p23_EF =
   EqSys.liftF2 $ \_ x ->
      let y = x
      in  1/((y+sqrt(y*y+4*y))/(2*y))

-- |  compareable to ic engine
fn23_p32_EF :: ExpVar s ->  ExpVar s ->  ExpVar s
fn23_p32_EF =
   EqSys.liftF2 $ \_ x ->
      let y=x
      in  1/((y+sqrt(y*y+4*y))/(2*y))

fn32_p23, fn23_p32:: ExpVar s ->  ExpVar s ->  ExpVar s
fn32_p23 = fn23_p32_ba
fn23_p32 = fn23_p32_ba


main :: IO ()
main = do
  let

      varEnvs = zipWith (zipWith (solve batteryResistance)) varX' varY'

      -- get Energies
      eoutVar0 = getSignalVarEnergy varEnvs eout0
      eoutVar1 = getSignalVarEnergy varEnvs eout1

      varEout = eoutVar0 .+ S.makeDelta eoutVar1
      einVar = getSignalVarEnergy varEnvs ein

      -- calculate split share and system efficiency
      etaSysVar = (eoutVar0 .+ S.makeDelta eoutVar1)./einVar

      -- calculate Losses
      varLossA = varE02 .- varE20
      varLossB = varE20 .- varEout
      varLoss = varE02 .- varEout

      -- Get more Env values
      varN23 = getSignalVarEta varEnvs (XIdx.eta sec0 node2 node3)
      varN32 = getSignalVarEta varEnvs (XIdx.eta sec1 node3 node2)
      varN02 = getSignalVarEta varEnvs (XIdx.eta sec0 node0 node2)

      varP32_0 = getSignalVarPower varEnvs (XIdx.power sec0 node3 node2)
      varP32_1 = getSignalVarPower varEnvs (XIdx.power sec1 node3 node2)


      varP23_0 = getSignalVarPower varEnvs (XIdx.power sec0 node2 node3)
      varP23_1 = getSignalVarPower varEnvs (XIdx.power sec1 node2 node3)

      varP20 = getSignalVarPower varEnvs (XIdx.power sec0 node2 node0)
      varP02 = getSignalVarPower varEnvs (XIdx.power sec0 node0 node2)

      varE20 = getSignalVarEnergy varEnvs (XIdx.energy sec0 node2 node0)
      varE02 = getSignalVarEnergy varEnvs (XIdx.energy sec0 node0 node2)

      varPout0 = getSignalVarPower varEnvs (XIdx.power sec0 node1 node2)
      varPout1 = getSignalVarPower varEnvs (XIdx.power sec1 node1 node2)

      -- create curve of n02 in used power range
      p20Lin = S.concat varP20
      n02Lin = S.concat varN02

      -- fuel converter
      (p20Lin', n02Lin') = S.sortTwo (p20Lin, n02Lin)


  -- Debug Plots


  -- Plots to check the variation

  PlotIO.surface "varX" plotTerm varX varY varX
  Rep.report [] ("varX",varX)

  PlotIO.surface "varY" plotTerm varX varY varY
  Rep.report [] ("vary",varY)

  -- Plot to check consumer behaviour

  PlotIO.surface "Eout" plotTerm varX varY (eoutVar0 .+ (S.makeDelta eoutVar1))
  Rep.report [] ("Eout",(eoutVar0 .+ (S.makeDelta eoutVar1)))

  PlotIO.surface "Pout0" plotTerm varX varY varPout0
  Rep.report [] ("Pout0",varPout0)

  PlotIO.surface "Pout1" plotTerm varX varY varPout1
  Rep.report [] ("Pout1",varPout1)


  -- Plots to check variable efficiency at fuel converter

  PlotIO.surface "N02" plotTerm varX varY varN02


  PlotIO.surface "P20" plotTerm varX varY varP20

  PlotIO.surface "P02" plotTerm varX varY varP02

  PlotIO.xy "N02 - Curve" plotTerm id p20Lin' $
    PlotIO.label "efficiency N02" n02Lin'

  -- Plots to check variable efficiency at storage -- charging
  PlotIO.surface "P23_0 - externe Ladeleistung" plotTerm varX varY varP23_0
  Rep.report [] ("varP23_0", varP23_0)

  PlotIO.surface "P32_0 - interne LadeLeistung" plotTerm varX varY varP32_0
  Rep.report [] ("varP32_0", varP32_0)

  PlotIO.surface "N23 - Charging" plotTerm varP32_0 varY varN23
  PlotIO.xy "N23 - Charging" plotTerm id varP32_0 $
    PlotIO.label "efficiency N23" varN23
  Rep.report  [] ("N23 - Charging", varN23)


  PlotIO.surface "P23_1 - externe Entladeleistung" plotTerm varX varY varP23_1
  Rep.report [] ("varP23_1", varP23_1)

  PlotIO.surface "P32_1 - interne EntladeLeistung" plotTerm varX varY varP32_1
  Rep.report [] ("varP32_1", varP32_1)

  Rep.report  [] ("N32 - Discharging", varN32)
  PlotIO.surface  "N32 - Discharging" plotTerm varP23_1 varY varN32
  PlotIO.xy "N32 - Discharging" plotTerm id varP23_1 $
    PlotIO.label "efficiency N32" varN32


  -- Check Losses

  -- Loss of N02
  PlotIO.surface "LossA" plotTerm varX varY varLossA

  -- Loss of the Rest of the system
  PlotIO.surface "LossB" plotTerm varX varY varLossB

  -- Total System Loss
  PlotIO.surface "Loss" plotTerm varX varY varLoss

  -- System loss in curves over split variation for multiple resistance values
  PlotIO.xy "Loss" plotTerm id varX $ PlotIO.label "Loss" varLoss

  -- Total System Efficiency
  Rep.report  [] ("EtaSys",etaSysVar)
  PlotIO.surface "EtaSys" plotTerm varX varY etaSysVar
  -- System efficiency in curves over split variation for multiple resistance values
  PlotIO.xy "EtaSys" plotTerm id varX $ PlotIO.label "EtaSys" etaSysVar


  let envhh = head $ head varEnvs
      envhl = head $ last varEnvs
      envlh = last $ head varEnvs
      envll = last $ last varEnvs


  concurrentlyMany_ $
    map ( Draw.xterm
          . Draw.title "Topology"
          . Draw.seqFlowGraph Draw.optionsDefault )
        [envhh,envhl, envlh, envll]
