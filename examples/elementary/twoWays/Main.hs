
module Main where

import qualified EFA.Example.Index as XIdx
import qualified EFA.Example.Absolute as EqGen
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Graph as Gr
import EFA.Equation.System ((=.=))
import EFA.Example.Absolute ((.=))
import EFA.Example.Utility (makeEdges, constructSeqTopo)

import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw

import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Map (checkedLookup)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import EFA.Utility.Async (concurrentlyMany_)

import Control.Applicative (liftA2)

import Data.Monoid (mconcat, (<>))


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
topo = Gr.fromList nodes (makeEdges edges)
  where nodes
          = [(c0, TD.Crossing), (c1, TD.Crossing), (c2, TD.Crossing),
             (c3, TD.Crossing), (Sink, TD.AlwaysSink),
             (Source, TD.AlwaysSource)]
        edges
          = [(Source, c0), (c0, c1), (c1, c2), (c2, Sink), (c0, c3),
             (c3, c2)]

seqTopo :: Flow.RangeGraph Node
seqTopo = constructSeqTopo topo [0]

given :: Double -> Double -> EqGen.EquationSystem Node s Double Double
given e x =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (XIdx.x sec0 c0 c1 .= x) :
   (XIdx.power sec0 Source c0 .= e) :
   (XIdx.eta sec0 Source c0 .= 1) :
   (XIdx.eta sec0 c1 c2 .= 1) :
   (XIdx.eta sec0 c3 c2 .= 1) :
   (XIdx.eta sec0 c2 Sink .= 1) : []


type Expr s a v x = EqGen.Expression Node s a v x

c02, c04 :: (Eq v, Arith.Sum v) => Expr s a v v
c02 = EqGen.variable $ XIdx.power sec0 c0 c1
c04 = EqGen.variable $ XIdx.power sec0 c0 c3

n12, n14 :: (Eq v, Arith.Sum v) => Expr s a v v
n12 = EqGen.variable $ XIdx.eta sec0 c0 c1
n14 = EqGen.variable $ XIdx.eta sec0 c0 c3

n1, n2 :: (Fractional x) => Expr s a v x -> Expr s a v x
n1 p = -0.012 * (p - 12) * (p - 3) + 0.5
n2 p = -0.021 * (p - 12) * p

etas :: EqGen.EquationSystem Node s Double Double
etas =
  (n12 =.= n1 c02)
  <> (n14 =.= n2 c04)


xRange :: [Double]
xRange = 0.01:[ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ] ++ [0.99]

enRange :: [Double]
enRange = 0.01:[1..12]


eout, ein :: XIdx.Energy Node
eout = XIdx.energy sec0 Sink c2
ein  = XIdx.energy sec0 Source c0


solve :: Double -> Double -> String
solve e x =
  let emap =
         Env.energyMap $ Env.signal $ EqGen.solve seqTopo (etas <> given e x)
  in  show e ++ " " ++ show x ++ " " ++
      Format.unUnicode (formatValue
         (liftA2 (/)
             (Record.unAbsolute $ checkedLookup "solve" emap eout)
             (Record.unAbsolute $ checkedLookup "solve" emap ein)))

main :: IO ()
main =
  concurrentlyMany_ [
    Draw.xterm $ Draw.topology topo,
    putStrLn $ unlines $ map (\e -> unlines $ map (solve e) xRange) enRange
  ]
