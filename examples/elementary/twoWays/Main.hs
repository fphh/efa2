
module Main where

import EFA.Application.Utility (topologyFromEdges, seqFlowGraphFromStates)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((.=), (=.=))

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Variable as Var
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Async (concurrentlyMany_)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import Control.Applicative (liftA2)

import Data.Monoid (mconcat, (<>))


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0


data Node = Sink | Source | Crossing Int deriving (Eq, Ord, Show)

instance Enum Node where
   fromEnum Sink = 0
   fromEnum Source = 1
   fromEnum (Crossing x) = x+2

   toEnum 0 = Sink
   toEnum 1 = Source
   toEnum x = Crossing (x-2)

instance Node.C Node where
   display Sink = Format.literal "Sink"
   display Source = Format.literal "Source"
   display (Crossing c) = Format.integer $ fromIntegral c

   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault
   typ Sink = Node.AlwaysSink
   typ Source = Node.AlwaysSource
   typ (Crossing _) = Node.Crossing

c0, c1, c2, c3 :: Node
c0 :~ c1 :~ c2 :~ c3 :~ _ = fmap Crossing $ Stream.enumFrom 0

topo :: Topo.Topology Node
topo =
   topologyFromEdges
      [(Source, c0), (c0, c1), (c1, c2), (c2, Sink),
       (c0, c3), (c3, c2)]

flowGraph :: SeqFlow.Graph Node (Result a) (Result v)
flowGraph = seqFlowGraphFromStates topo [0]

given :: Double -> Double -> EqSys.EquationSystemIgnore Node s Double Double
given e x =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (XIdx.x sec0 c0 c1 .= x) :
   (XIdx.power sec0 Source c0 .= e) :
   (XIdx.eta sec0 Source c0 .= 1) :
   (XIdx.eta sec0 c1 c2 .= 1) :
   (XIdx.eta sec0 c3 c2 .= 1) :
   (XIdx.eta sec0 c2 Sink .= 1) : []


type Expr s a v x = EqSys.ExpressionIgnore Node s a v x

c02, c04 :: (Eq v, Arith.Sum v) => Expr s a v v
c02 = EqSys.variable $ XIdx.power sec0 c0 c1
c04 = EqSys.variable $ XIdx.power sec0 c0 c3

n12, n14 :: (Eq v, Arith.Sum v) => Expr s a v v
n12 = EqSys.variable $ XIdx.eta sec0 c0 c1
n14 = EqSys.variable $ XIdx.eta sec0 c0 c3

n1, n2 :: (Fractional x) => Expr s a v x -> Expr s a v x
n1 p = -0.012 * (p - 12) * (p - 3) + 0.5
n2 p = -0.021 * (p - 12) * p

etas :: EqSys.EquationSystemIgnore Node s Double Double
etas =
   (n12 =.= n1 c02) <>
   (n14 =.= n2 c04)


xRange :: [Double]
xRange = 0.01:[ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ] ++ [0.99]

enRange :: [Double]
enRange = 0.01:[1..12]


eout, ein :: XIdx.Energy Node
eout = XIdx.energy sec0 Sink c2
ein  = XIdx.energy sec0 Source c0


solve :: Double -> Double -> String
solve e x =
   let emap idx =
          Var.checkedLookup "solve" SeqFlow.lookupEnergy idx $
          EqSys.solve flowGraph (etas <> given e x)
   in  show e ++ " " ++ show x ++ " " ++
       Format.unUnicode (formatValue (liftA2 (/) (emap eout) (emap ein)))

main :: IO ()
main =
   concurrentlyMany_ [
      Draw.xterm $ Draw.topology topo,
      putStrLn $ unlines $ map (\e -> unlines $ map (solve e) xRange) enRange
   ]
