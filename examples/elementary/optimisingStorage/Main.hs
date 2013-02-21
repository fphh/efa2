

module Main where

import Data.Monoid ((<>))

import Control.Applicative


import EFA.Example.Utility
  ( edgeVar, makeEdges, constructSeqTopo, recAbs )

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Graph.Draw as Draw

import qualified EFA.Utility.Stream as Stream
import EFA.Utility (checkedLookup)
import EFA.Utility.Async

import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Equation.System as EqGen
import EFA.Equation.System ((=.=))
import qualified EFA.Equation.Env as Env
import EFA.Equation.Env(Env, SingleRecord,energyMap,powerMap,etaMap,accessMap)

import qualified EFA.Equation.Result as R

import qualified Data.List.Match as Match

import qualified EFA.Graph as Gr
import qualified EFA.Signal.Signal as S

import EFA.Signal.Signal(UTFSig, FSamp, Test2, PFSamp, (.+), (.-),(./),(.*))
import EFA.Signal.Base(Val)

import qualified EFA.Report.Report as Rep


import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Typ as T

import EFA.Signal.Typ (A, P, Tt, F, N, X, Typ, T, UT)

import Debug.Trace


sec0, sec1 :: Idx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ Idx.Section 0


data Node = N0 | N1 | N2 | N3 deriving (Show, Eq, Ord, Enum)


instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


topoDreibein :: TD.Topology Node
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [ (N0, TD.Source),
               (N1, TD.Crossing),
               (N2, TD.Sink),
               (N3, TD.Storage) ]
        es = [(N0, N1), (N1, N3), (N1, N2)]

seqTopo :: TD.SequFlowGraph Node
seqTopo = constructSeqTopo topoDreibein [0, 4]

-- | Variable efficiency of fuel converter: n01 as function of p10       
etaf :: EqGen.ExprWithVars Node s Double -> EqGen.ExprWithVars Node s Double
etaf x = 1/((y+sqrt(y*y+4*y))/(2*y))
  where y = x/1000

n01, n12, n13, n31, p10, p21, e31 ::
  Idx.Section -> EqGen.ExprWithVars Node s a
n01 sec = edgeVar EqGen.eta sec N0 N1
n12 sec = edgeVar EqGen.eta sec N1 N2
n13 sec = edgeVar EqGen.eta sec N1 N3
n31 sec = edgeVar EqGen.eta sec N3 N1
p10 sec = edgeVar EqGen.power sec N1 N0
p21 sec = edgeVar EqGen.power sec N2 N1
e31 sec = edgeVar EqGen.energy sec N3 N1

p31 sec = edgeVar EqGen.power sec N3 N1
p13 sec = edgeVar EqGen.power sec N1 N3


stoinit :: EqGen.ExprWithVars Node s a
stoinit = EqGen.storage (Idx.SecNode Idx.initSection N3)

ein, eout0, eout1 :: Idx.Energy Node
ein = Idx.Energy recAbs (Idx.SecNode sec0 N0) (Idx.SecNode sec0 N1)
eout0 = Idx.Energy recAbs (Idx.SecNode sec0 N2) (Idx.SecNode sec0 N1)
eout1 = Idx.Energy recAbs (Idx.SecNode sec1 N2) (Idx.SecNode sec1 N1)

e33 :: EqGen.ExprWithVars Node s a
e33 = EqGen.getVar $
  Idx.Energy recAbs (Idx.SecNode (Idx.Section (-1)) N3) (Idx.SecNode sec1 N3)

time :: Idx.Section -> EqGen.ExprWithVars nty s Double
time = EqGen.dtime

-- | variable efficiency of energy storage in discharging mode as function of n31 over p13  
f r x = x/(x + r*((ui - sqrt(ui^2 - 4*r*x)) / 2*r)^2)
  where -- r = 0.9
        ui = 200
{-
g x = ((-x) + r*((200 - sqrt(200*200 - 4*r*(-x))) / 2*r)*((200 - sqrt(200*200 - 4*r*(-x))) / 2*r))/(-x)
  where r = 0.9
-}

-- | variable efficiency of energy storage in charging mode as function of n13 over p31 
g r x = 1 / (1 + (x*r)/(ui^2))
  where -- r = 0.9
        ui = 200

-- | Provide time of sec1 and inner resistance of battery
given :: Double -> Double -> EqGen.EquationSystem Node s Double
given t r =
  (time Idx.initSection =.= 1)
  <> (stoinit =.= 3)

  <> (time sec0 =.= 1 - t')
  <> (p21 sec0 =.= p)
  <> (e31 sec0 =.= e31 sec1)

  <> (n01 sec0 =.= etaf (p10 sec0))
  <> (n12 sec0 =.= 0.9)
  <> (n13 sec0 =.= (g r' (p31 sec0)))

  <> (time sec1 =.= t')
  <> (p21 sec1 =.= p)
  <> (n12 sec1 =.= 0.9)
  <> (n31 sec1 =.= f r' (p13 sec1))
  where t' = EqGen.constToExprSys t
        r' = EqGen.constToExprSys r
        p = 1000

trange, rrange :: [Double]
trange = 0.01:[0.1, 0.2 .. 0.9]
rrange = 0.01:[0.1,0.2 .. 2]     
              

varMat :: [a] -> [b] -> ([[a]], [[b]])
varMat xs ys =
   (Match.replicate ys xs, map (Match.replicate xs) ys)


-- | r is inner Resistance of Battery
solve :: Val -> Val -> Env Node SingleRecord (R.Result Val)
solve t r = EqGen.solve (given t r) seqTopo
  

unpackResult :: R.Result a -> a 
unpackResult (R.Determined x) = x

-- | Safe Lookup Functions
getVarEnergy :: [[Env Node SingleRecord (R.Result Val)]] -> Idx.Energy Node -> Test2 (Typ A F Tt) Val
getVarEnergy varEnvs idx = S.changeSignalType $ S.fromList2 $ map (map f ) varEnvs
  where f ::  Env Node SingleRecord (R.Result Val) -> Val
        f envs = unpackResult $ checkedLookup (energyMap envs) idx
        
-- | Safe Lookup Functions
getVarPower :: [[Env Node Env.SingleRecord (R.Result Val)]] -> Idx.Power Node -> Test2 (Typ A P Tt) Val
getVarPower varEnvs idx = S.changeSignalType $ S.fromList2 $ map (map f ) varEnvs
  where f ::  Env Node SingleRecord (R.Result Val) -> Val
        f envs = unpackResult $ checkedLookup (powerMap envs) idx

-- | Safe Lookup Functions
getVarEta :: [[Env Node Env.SingleRecord (R.Result Val)]] -> Idx.Eta Node -> Test2 (Typ A N Tt) Val
getVarEta varEnvs idx = S.changeSignalType $ S.fromList2 $ map (map f ) varEnvs
  where f ::  Env Node SingleRecord (R.Result Val) -> Val
        f envs = unpackResult $ checkedLookup (etaMap envs) idx
        
        
-- lookUpEnvs :: Env Node Env.SingleRecord (R.Result a) -> (idx node) -> a         



main :: IO ()
main = do
  let (varT, varRr) = varMat trange rrange
      varEnvs = zipWith (zipWith solve) varT varRr

      timeVar = S.fromList2 varT :: S.Test2 (Typ A T Tt) Double
      varR = S.fromList2 varRr :: S.Test2 (Typ A UT Tt) Double
      
      -- get Energies 
      eoutVar0 = getVarEnergy varEnvs eout0 
      eoutVar1 = getVarEnergy varEnvs eout1
      
      varEout = eoutVar0 .+ (S.makeDelta eoutVar1)
      einVar = getVarEnergy varEnvs ein 
      
      -- calculate split share and system efficiency
      etaSysVar = (eoutVar0 .+ (S.makeDelta eoutVar1))./einVar
      varY = S.changeType $ eoutVar1 ./ (eoutVar0 .+ (S.makeDelta eoutVar1)) :: S.Test2 (Typ A X Tt) Double
      
      -- calculate Losses
      varLossA = varE01 .- varE10
      varLossB = varE10 .- varEout
      varLoss = varE01 .- varEout
       
      -- Get more Env values
      varN13 = getVarEta varEnvs (Idx.Eta recAbs (Idx.SecNode sec0 N1) (Idx.SecNode sec0 N3))
      varN31 = getVarEta varEnvs (Idx.Eta recAbs (Idx.SecNode sec1 N3) (Idx.SecNode sec1 N1))
      varN01 = getVarEta varEnvs (Idx.Eta recAbs (Idx.SecNode sec0 N0) (Idx.SecNode sec0 N1))
      
      varE31 = getVarEnergy varEnvs (Idx.Energy recAbs (Idx.SecNode sec1 N3) (Idx.SecNode sec1 N1))
      
      varP31_0 = getVarPower varEnvs (Idx.Power recAbs (Idx.SecNode sec0 N3) (Idx.SecNode sec0 N1))            
      varP31_1 = getVarPower varEnvs (Idx.Power recAbs (Idx.SecNode sec1 N3) (Idx.SecNode sec1 N1))

      
      varP13_0 = getVarPower varEnvs (Idx.Power recAbs (Idx.SecNode sec0 N1) (Idx.SecNode sec0 N3))
      varP13_1 = getVarPower varEnvs (Idx.Power recAbs (Idx.SecNode sec1 N1) (Idx.SecNode sec1 N3))

      varP10 = getVarPower varEnvs (Idx.Power recAbs (Idx.SecNode sec0 N1) (Idx.SecNode sec0 N0))
      varP01 = getVarPower varEnvs (Idx.Power recAbs (Idx.SecNode sec0 N0) (Idx.SecNode sec0 N1))
      
      varE10 = getVarEnergy varEnvs (Idx.Energy recAbs (Idx.SecNode sec0 N1) (Idx.SecNode sec0 N0))
      varE01 = getVarEnergy varEnvs (Idx.Energy recAbs (Idx.SecNode sec0 N0) (Idx.SecNode sec0 N1))
      
      varPout0 = getVarPower varEnvs (Idx.Power recAbs (Idx.SecNode sec0 N2) (Idx.SecNode sec0 N1))
      varPout1 = getVarPower varEnvs (Idx.Power recAbs (Idx.SecNode sec1 N2) (Idx.SecNode sec1 N1))
      
      -- create curve of n01 in used power range
      p10Lin = S.concat varP10
      p01Lin = S.concat varP01
      n01Lin = S.concat varN01
      
      p130Lin = S.concat varP13_0
      p310Lin = S.concat varP31_0
      p131Lin = S.concat varP13_1
      p311Lin = S.concat varP31_1
      n13Lin = S.concat varN13
      n31Lin = S.concat varN31
      
      -- fuel converter
      (p10Lin', n01Lin') = S.sortTwo (p10Lin,n01Lin)    
      
      -- discharging
--      (p131Lin', n31Lin') = S.sortTwo (p131Lin,n31Lin)    
      
      -- charging
--      (p310Lin', n13Lin') = S.sortTwo (p310Lin,n13Lin)    
       
 {- 
  -- Plots to check the variation
  
  Plot.surfaceIO "varY" varY varR varY
  
  Plot.surfaceIO "varY" varY varR varR
  
  -- Plot to check consumer behaviour 
  
  Plot.surfaceIO "Eout" varY varR (eoutVar0 .+ (S.makeDelta eoutVar1))

  Plot.surfaceIO "Pout0" varY varR varPout0
  
  Plot.surfaceIO "Pout1" varY varR varPout1
  
  Rep.report [] ("varPout1",varPout1)    

  Rep.report [] ("varPout1",varPout0) 
  
  -- Plots to check variable efficiency at fuel converter
  
  Plot.surfaceIO "N01" varY varR varN01
  
  Plot.surfaceIO "P10" varY varR varP10

  Plot.surfaceIO "P01" varY varR varP01

  Plot.xyIO "N01 - Curve"  p10Lin' n01Lin'

  
 -} 
      
  -- Plots to check variable efficiency at storage -- charging
  Rep.report [] ("varP13_0",varP13_0)
      
  Rep.report [] ("varP31_0",varP31_0)
  
  Rep.report  [] ("N13 - Charging",varN13)
  
  Plot.surfaceIO "P31_0 - interne LadeLeistung" varY varR varP31_0 
  
  Plot.surfaceIO "P13_0 - externe Ladeleistung" varY varR varP13_0
  
  Plot.xyIO "N13 - Charging"  varP31_0 varN13
  
  Plot.surfaceIO "N13 - Charging" varP31_0 varR varN13
  

  Rep.report [] ("varP13_1",varP13_1)
  
  Rep.report [] ("varP31_1",varP31_1)
  
  Plot.surfaceIO "P13_1 - externe Entladeleistung" varY varR varP13_1
  
  Plot.surfaceIO "P31_1 - interne EntladeLeistung" varY varR varP31_1
  
  Plot.xyIO "N31 - Discharging" varP13_1 varN31 
  
  Rep.report  [] ("N31 - Discharging",varN31)
  
  

  -- Check Losses 

  -- Loss of N01
  Plot.surfaceIO "LossA" varY varR varLossA

  -- Loss of the Rest of the system
  Plot.surfaceIO "LossB" varY varR varLossB
  
  -- Total System Loss
  Plot.surfaceIO "Loss" varY varR varLoss
  
  -- System loss in curves over split variation for multiple resistance values 
  Plot.xyIO "Loss" varY varLoss
  
  -- Total System Efficiency
  Plot.surfaceIO "EtaSys" varY varR etaSysVar
  
  -- System efficiency in curves over split variation for multiple resistance values 
  Plot.xyIO "Loss" varY etaSysVar
  
  let envhh = EqGen.solve (given (head trange) (head rrange)) seqTopo
      envhl = EqGen.solve (given (head trange) (last rrange)) seqTopo
      envlh = EqGen.solve (given (last trange) (head rrange)) seqTopo
      envll = EqGen.solve (given (last trange) (last rrange)) seqTopo


  concurrentlyMany_ $
    map (Draw.sequFlowGraphAbsWithEnv seqTopo) [envhh] -- , envhl, envlh, envll]
