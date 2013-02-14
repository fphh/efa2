

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

import qualified EFA.Equation.Result as R

import qualified Data.List.Match as Match

import qualified EFA.Graph as Gr
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Typ as T


sec0, sec1 :: Idx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ Idx.Section 0


data Nodes = N0 | N1 | N2 | N3 deriving (Show, Eq, Ord, Enum)


instance Node.C Nodes where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


topoDreibein :: TD.Topology Nodes
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [ (N0, TD.Source),
               (N1, TD.Crossing),
               (N2, TD.Sink),
               (N3, TD.Storage) ]
        es = [(N0, N1), (N1, N3), (N1, N2)]

seqTopo :: TD.SequFlowGraph Nodes
seqTopo = constructSeqTopo topoDreibein [0, 4]
      
etaf :: EqGen.ExprWithVars Nodes s Double -> EqGen.ExprWithVars Nodes s Double
etaf x = 1/((x+sqrt(x*x+4*x))/(2*x))


n01, n12, n13, n31, p10, p21, e31 ::
  Idx.Section -> EqGen.ExprWithVars Nodes s a
n01 sec = edgeVar EqGen.eta sec N0 N1
n12 sec = edgeVar EqGen.eta sec N1 N2
n13 sec = edgeVar EqGen.eta sec N1 N3
n31 sec = edgeVar EqGen.eta sec N3 N1
p10 sec = edgeVar EqGen.power sec N1 N0
p21 sec = edgeVar EqGen.power sec N2 N1
e31 sec = edgeVar EqGen.energy sec N3 N1

stoinit :: EqGen.ExprWithVars Nodes s a
stoinit = EqGen.storage (Idx.SecNode Idx.initSection N3)

ein, eout0, eout1 :: Idx.Energy Nodes
ein = Idx.Energy recAbs (Idx.SecNode sec0 N0) (Idx.SecNode sec0 N1)
eout0 = Idx.Energy recAbs (Idx.SecNode sec0 N2) (Idx.SecNode sec0 N1)
eout1 = Idx.Energy recAbs (Idx.SecNode sec1 N2) (Idx.SecNode sec1 N1)

e33 :: EqGen.ExprWithVars Nodes s a
e33 = EqGen.getVar $
  Idx.Energy recAbs (Idx.SecNode (Idx.Section (-1)) N3) (Idx.SecNode sec1 N3)

time :: Idx.Section -> EqGen.ExprWithVars nty s Double
time = EqGen.dtime


given :: Double -> Double -> EqGen.EquationSystem Nodes s Double
given t n =
  (time Idx.initSection =.= 1)
  <> (e33 =.= 1)
  <> (stoinit =.= 3)

  <> (time sec0 =.= 1 - t')
  <> (p21 sec0 =.= 1)
  <> (e31 sec0 =.= t' / n' / 0.9)
  <> (n01 sec0 =.= etaf (p10 sec0))
  <> (n12 sec0 =.= 0.9)
  <> (n13 sec0 =.= n')

  <> (time sec1 =.= t')
  <> (p21 sec1 =.= 1)
  <> (n12 sec1 =.= 0.9)
  <> (n31 sec1 =.= n')

  where t' = EqGen.constToExprSys t
        n' = EqGen.constToExprSys n


trange, nrange :: [Double]
trange = 0.01:[0.1, 0.2 .. 0.9]
nrange = [0.6, 0.65 .. 1]

varMat :: [a] -> [b] -> ([[a]], [[b]])
varMat xs ys =
   (Match.replicate ys xs, map (Match.replicate xs) ys)



solve :: Double -> Double -> Double
solve t n =
  let env = EqGen.solve (given t n) seqTopo
      emap = Env.energyMap env
      f ei eo0 eo1 = (eo0 + eo1) / ei
      R.Determined res =
        f <$> (checkedLookup emap ein)
          <*> (checkedLookup emap eout0)
          <*> (checkedLookup emap eout1)
  in res


main :: IO ()
main = do
  let (varT, varN) = varMat trange nrange
      etaSys = zipWith (zipWith solve) varT varN

      timeVar, effVar, etaSysVar :: S.Test2 (T.Typ T.A T.Y T.Tt) Double
      timeVar = S.fromList2 varT
      effVar = S.fromList2 varN
      etaSysVar = S.fromList2 etaSys

  Plot.surfPlot "EtaSys" timeVar effVar etaSysVar

  let envhh = EqGen.solve (given (head trange) (head nrange)) seqTopo
      envhl = EqGen.solve (given (head trange) (last nrange)) seqTopo
      envlh = EqGen.solve (given (last trange) (head nrange)) seqTopo
      envll = EqGen.solve (given (last trange) (last nrange)) seqTopo


  concurrentlyMany_ $
    map (Draw.sequFlowGraphAbsWithEnv seqTopo) [envhh, envhl, envlh, envll]
