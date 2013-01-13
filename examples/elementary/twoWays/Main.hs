{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Main where

import Data.Foldable (foldMap)
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

import Data.Maybe



import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.TopologyData as TD
import qualified EFA2.Utils.Stream as Stream
import EFA2.Utils.Stream (Stream((:~)))
import EFA2.Topology.EfaGraph (mkGraph)
import EFA2.Example.Utility (makeNodes, makeEdges)



import EFA2.Interpreter.Env

import qualified EFA2.StateAnalysis.StateAnalysis as StateAnalysis
import EFA2.Topology.Draw

import EFA2.Example.Utility (constructSeqTopo, edgeVar, (.=), recAbs)
import qualified EFA2.Topology.EquationGenerator as EqGen
import EFA2.Topology.EquationGenerator ((=.=))

import UniqueLogic.ST.Expression
import qualified UniqueLogic.ST.System as Sys

import qualified EFA2.Interpreter.Env as Env
import qualified Data.Accessor.Basic as Accessor
import Control.Monad.Trans.State (StateT, runStateT, gets, modify, get)
import Control.Monad.ST (ST, runST)

import Debug.Trace


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

c12, c21, c14, c41 :: EqGen.ExprWithVars s a
c12 = edgeVar EqGen.power sec0 c1 c2
c21 = edgeVar EqGen.power sec0 c2 c1
c14 = edgeVar EqGen.power sec0 c1 c4
c41 = edgeVar EqGen.power sec0 c4 c1


{-
--gnuplot> set xrange [0:10]
--gnuplot> plot 0.8*(1 - 1/(x+1)), -0.03*(x-10)*x
n1 p = 0.8 * (1 - 1/(p + 1))
n2 p = -0.03 * (p - 10) * c14
-}

--gnuplot> set xrange [0:12]
--gnuplot> plot -0.012*(x-12)*(x-3)+0.5, -0.021*(x-12)*x

n1, n2, n3 :: EqGen.ExprWithVars s Double -> EqGen.ExprWithVars s Double
n1 p = -0.012 * (p - 12) * (p - 3) + 0.5
n2 p = -0.021 * (p - 12) * p

-- n3 soll einen lookup-Table verwenden 
n3 p = EqGen.ExprWithVars $ do
  case dropWhile ((< p') . fst) table of
       (_, x):_ -> return x
       _ -> return 0
  where p' = undefined p
        table = zip [0..12] (repeat 0.5)

etas :: EqGen.EquationSystem s Double
etas =
  (c21 =.= n2 c12 * c12)
  <> (c41 =.= n2 c14 * c14)

range :: [Double]
range = 0.01:[ 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 ] ++ [0.99]
-- range = take 10 (repeat 0)

enRange :: [Double]
enRange = 0.01:[1..12]

pts :: [(Double, Double)]
pts = enRange >>= \e -> range >>= \x -> return (e, x)

eout, ein :: Idx.Energy
eout = Idx.Energy recAbs (Idx.SecNode sec0 sink) (Idx.SecNode sec0 c3)
ein = Idx.Energy recAbs (Idx.SecNode sec0 source) (Idx.SecNode sec0 c1)

--eout = Idx.Energy recAbs (Idx.SecNode sec0 c4) (Idx.SecNode sec0 c1)
--ein = Idx.Energy recAbs (Idx.SecNode sec0 c1) (Idx.SecNode sec0 c4)

main :: IO ()
main = do
  --let sol = StateAnalysis.advanced topology
  --print (length sol)
  -- drawTopologySimple topo

  let env = map g pts
      g (e, x) = EqGen.solveSystem (etas <> given e x) seqTopo
      res = zip3 pts (concat $ catMaybes $ map (M.lookup eout . energyMap) env)
                     (concat $ catMaybes $ map (M.lookup ein . energyMap) env)
      f ((x, y), zout, zin) = show x ++ " " ++ show y ++ " " ++ show ( zout/zin )
                            -- = show zin ++ " " ++ show zout
      h ((x, _), _, _) ((y, _), _, _) = x == y

  putStrLn $ L.intercalate "\n" (L.intercalate [[]] $ map (map f) $ L.groupBy h res)
  -- mapM_ (drawTopology seqTopo) env
