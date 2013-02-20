
module Main where

import Control.Applicative (liftA2)

import Data.Foldable (foldMap)
import Data.Monoid ((<>))


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


import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph.Topology.Node as Node


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0


data Node = Sink | Source | C Int deriving (Eq, Ord, Show)

instance Enum Node where
         fromEnum Sink = 0
         fromEnum Source = 1
         fromEnum (C x) = x+2

         toEnum 0 = Sink
         toEnum 1 = Source
         toEnum x = C (x-2)

instance Node.C Node where
   display Sink = Format.literal "Sink"
   display Source = Format.literal "Source"
   display (C c) = Format.integer $ fromIntegral c

   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault

c0, c1, c2, c3 :: Node
c0 :~ c1 :~ c2 :~ c3 :~ _ = Stream.enumFrom $ C 0

topo :: TD.Topology Node
topo = mkGraph nodes (makeEdges edges)
  where nodes
          = [(c0, TD.Crossing), (c1, TD.Crossing), (c2, TD.Crossing),
             (c3, TD.Crossing), (Sink, TD.AlwaysSink),
             (Source, TD.AlwaysSource)]
        edges
          = [(Source, c0), (c0, c1), (c1, c2), (c2, Sink), (c0, c3),
             (c3, c2)]

seqTopo :: TD.SequFlowGraph Node
seqTopo = constructSeqTopo topo [0]

given :: Double -> Double -> EqGen.EquationSystem Idx.Absolute Node s Double
given e x =
   foldMap (uncurry (.=)) $
   (EqGen.dtime sec0, 1) :
   (edgeVar EqGen.xfactor sec0 c0 c1, x) :
   (edgeVar EqGen.power sec0 Source c0, e) :
   (edgeVar EqGen.eta sec0 Source c0, 1) :
   (edgeVar EqGen.eta sec0 c1 c2, 1) :
   (edgeVar EqGen.eta sec0 c3 c2, 1) :
   (edgeVar EqGen.eta sec0 c2 Sink, 1) : []

c02, c04 :: EqGen.ExprWithVars Idx.Absolute Node s a
c02 = edgeVar EqGen.power sec0 c0 c1
c04 = edgeVar EqGen.power sec0 c0 c3

n12, n14 :: EqGen.ExprWithVars Idx.Absolute Node s a
n12 = edgeVar EqGen.eta sec0 c0 c1
n14 = edgeVar EqGen.eta sec0 c0 c3

n1, n2 ::
   EqGen.ExprWithVars Idx.Absolute Node s Double ->
   EqGen.ExprWithVars Idx.Absolute Node s Double
n1 p = -0.012 * (p - 12) * (p - 3) + 0.5
n2 p = -0.021 * (p - 12) * p

etas :: EqGen.EquationSystem Idx.Absolute Node s Double
etas =
  (n12 =.= n1 c02)
  <> (n14 =.= n2 c04)


xRange :: [Double]
xRange = 0.01:[ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ] ++ [0.99]

enRange :: [Double]
enRange = 0.01:[1..12]


eout, ein :: Idx.Energy Idx.Absolute Node
eout = edgeVar (Idx.Energy recAbs) sec0 Sink c2
ein  = edgeVar (Idx.Energy recAbs) sec0 Source c0


solve :: Double -> Double -> String
solve e x =
  let emap = Env.energyMap $ EqGen.solve (etas <> given e x) seqTopo
  in  show e ++ " " ++ show x ++ " " ++
      Format.unUnicode (formatValue
         (liftA2 (/) (checkedLookup emap eout) (checkedLookup emap ein)))

main :: IO ()
main =
  concurrentlyMany_ [
    Draw.topology topo,
    putStrLn $ unlines $ map (\e -> unlines $ map (solve e) xRange) enRange
  ]
