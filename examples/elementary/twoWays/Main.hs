
module Main where

import EFA.Application.Utility (topologyFromEdges, quantityTopology)
import qualified EFA.Application.Plot as AppPlot

import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Variable as Var
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Topology.Absolute ((.=), (=.=))

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Result as Result

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Async (concurrentlyMany_)

import qualified EFA.Signal.Signal as Sig

import qualified EFA.Report.Format as Format

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

import Control.Applicative (liftA2)

import Data.Monoid (mconcat, (<>))
import Data.Tuple.HT (fst3, snd3, thd3)


{- |
This ordering warrants that graphviz displays the graph in a top-down fashion.
-}
data Node = Source | Crossing Int | Sink deriving (Eq, Ord, Show)

instance Node.C Node where
   display Sink = Format.literal "Sink"
   display Source = Format.literal "Source"
   display (Crossing c) = Format.integer $ fromIntegral c

   subscript = Node.subscriptDefault

   dotId Sink = "sink"
   dotId Source = "source"
   dotId (Crossing c) = show c

   typ Sink = Node.AlwaysSink
   typ Source = Node.AlwaysSource
   typ (Crossing _) = Node.Crossing

c0, c1, c2, c3 :: Node
c0 :~ c1 :~ c2 :~ c3 :~ _ = fmap Crossing $ Stream.enumFrom 0

topo :: Topo.Topology Node
topo =
   topologyFromEdges
      [(Source, c0),
       (c0, c1), (c1, c3),
       (c0, c2), (c2, c3),
       (c3, Sink)]

given :: Double -> Double -> EqSys.EquationSystemIgnore Node s Double
given e x =
   mconcat $
   (XIdx.dTime .= 1) :
   (XIdx.x c0 c1 .= x) :
   (XIdx.power Source c0 .= e) :
   (XIdx.eta Source c0 .= 1) :
   (XIdx.eta c1 c3 .= 1) :
   (XIdx.eta c2 c3 .= 1) :
   (XIdx.eta c3 Sink .= 1) : []


type Expr s v x = EqSys.ExpressionIgnore Node s v x

c02, c04 :: (Eq v, Arith.Sum v) => Expr s v v
c02 = EqSys.variable $ XIdx.power c0 c1
c04 = EqSys.variable $ XIdx.power c0 c2

n12, n14 :: (Eq v, Arith.Sum v) => Expr s v v
n12 = EqSys.variable $ XIdx.eta c0 c1
n14 = EqSys.variable $ XIdx.eta c0 c2

n1, n2 :: (Fractional x) => Expr s v x -> Expr s v x
n1 p = -0.012 * (p - 12) * (p - 3) + 0.5
n2 p = -0.021 * (p - 12) * p

etas :: EqSys.EquationSystemIgnore Node s Double
etas =
   (n12 =.= n1 c02) <>
   (n14 =.= n2 c04)


xRange :: [Double]
xRange = 0.01:[ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ] ++ [0.99]

enRange :: [Double]
enRange = 0.01:[1..12]


eout, ein :: XIdx.Energy Node
eout = XIdx.energy Sink c3
ein  = XIdx.energy Source c0


solve :: Double -> Double -> (Double, Double, Double)
solve e x =
   let emap idx =
          Var.checkedLookup "solve" FlowTopo.lookupEnergy idx $
          EqSys.solve (quantityTopology topo) (etas <> given e x)
       res = case liftA2 (/) (emap eout) (emap ein) of
                  Result.Determined v -> v
                  Result.Undetermined ->
                    error $ "Undetermined at " ++ show e ++ ", " ++ show x
   in  (e, x, res)

main :: IO ()
main =
   let pts :: Sig.UTSignal2 [] [] (Double, Double, Double)
       pts = Sig.fromList2 $ map (\e -> map (solve e) xRange) enRange


       xs, ys :: Sig.PSignal2 [] [] Double
       xs = Sig.changeType $ Sig.map fst3 pts
       ys = Sig.changeType $ Sig.map snd3 pts

       zs :: Sig.NSignal2 [] [] Double
       zs = Sig.changeType $ Sig.map thd3 pts


   in concurrentlyMany_ $
      (Draw.xterm $ Draw.topology topo) :
      AppPlot.surface "EtaSys" DefaultTerm.cons xs ys zs :
      []
