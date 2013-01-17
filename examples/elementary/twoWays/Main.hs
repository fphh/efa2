
module Main where

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.System as EqGen
import EFA.Equation.System ((=.=))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility (checkedLookup)

import EFA.Example.Utility (makeEdges, constructSeqTopo, edgeVar, (.=), recAbs)
import EFA.Graph (mkGraph)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import Control.Applicative (liftA2)

import Data.Foldable (foldMap)
import Data.Monoid ((<>))


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0

c1, c2, c3, c4, sink, source :: Idx.Node
c1 :~ (c2 :~ (c3 :~ (c4 :~ (sink :~ (source :~ _)))))
  = Stream.enumFrom $ Idx.Node 0

topology :: TD.Topology
topology = mkGraph nodes (makeEdges edges)
  where nodes
          = [(c1, TD.Crossing), (c2, TD.Crossing), (c3, TD.Crossing),
             (c4, TD.Crossing), (sink, TD.AlwaysSink),
             (source, TD.AlwaysSource)]
        edges
          = [(source, c1), (c1, c2), (c2, c3), (c3, sink), (c1, c4),
             (c4, c3)]

seqTopo :: TD.SequFlowGraph
seqTopo = constructSeqTopo topology [0]

given :: Double -> Double -> EqGen.EquationSystem s Double
given e x =
   foldMap (uncurry (.=)) $
   (EqGen.dtime sec0, 1) :
   (edgeVar EqGen.xfactor sec0 c1 c2, x) :
   (edgeVar EqGen.power sec0 source c1, e) :
   (edgeVar EqGen.eta sec0 source c1, 1) :
   (edgeVar EqGen.eta sec0 c2 c3, 1) :
   (edgeVar EqGen.eta sec0 c4 c3, 1) :
   (edgeVar EqGen.eta sec0 c3 sink, 1) : []

c12, c14 :: EqGen.ExprWithVars s a
c12 = edgeVar EqGen.power sec0 c1 c2
c14 = edgeVar EqGen.power sec0 c1 c4

n12, n14 :: EqGen.ExprWithVars s a
n12 = edgeVar EqGen.eta sec0 c1 c2
n14 = edgeVar EqGen.eta sec0 c1 c4

n1, n2 :: EqGen.ExprWithVars s Double -> EqGen.ExprWithVars s Double
n1 p = -0.012 * (p - 12) * (p - 3) + 0.5
n2 p = -0.021 * (p - 12) * p

etas :: EqGen.EquationSystem s Double
etas =
  (n12 =.= n1 c12)
  <> (n14 =.= n2 c14)


xRange :: [Double]
xRange = 0.01:[ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ] ++ [0.99]

enRange :: [Double]
enRange = 0.01:[1..12]


eout, ein :: Idx.Energy
eout = edgeVar (Idx.Energy recAbs) sec0 sink c3
ein  = edgeVar (Idx.Energy recAbs) sec0 source c1


solve :: Double -> Double -> String
solve e x =
  let emap = Env.energyMap $ EqGen.solve (etas <> given e x) seqTopo
  in  show e ++ " " ++ show x ++ " " ++
      Format.unUnicode (formatValue
         (liftA2 (/) (checkedLookup emap eout) (checkedLookup emap ein)))

main :: IO ()
main =
  putStrLn $ unlines $ map (\e -> unlines $ map (solve e) xRange) enRange
