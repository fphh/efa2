{-# LANGUAGE TypeOperators #-}


module Main where

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.PlotIO as PlotIO
import qualified EFA.Signal.Record as Rec
import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data, (:>), Nil)

import qualified EFA.Application.Index as XIdx
import qualified EFA.Application.Absolute as EqGen
import EFA.Application.Absolute ( (.=), (=.=) )
import EFA.Application.Utility ( makeEdges, constructSeqTopo )

import qualified EFA.Equation.Result as Res
import qualified EFA.Equation.Environment as Env

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import EFA.Utility.Async (concurrentlyMany_)

import qualified Data.Map as Map

import Data.Monoid (mconcat)


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

node0, node1, node2, node3 :: Node.Int
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Node.Int 0



topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.fromList ns (makeEdges es)
  where ns = [ (node0, TD.NoRestriction),
               (node1, TD.Crossing),
               (node2, TD.NoRestriction),
               (node3, TD.NoRestriction) ]
        es = [(node0, node1), (node1, node2), (node1, node3)]



time :: Sig.TSignal []  Double
time = Sig.fromList $ take (length node0Sig') [0..]


node0Sig :: Sig.PSignal [] Double
node0Sig = Sig.fromList node0Sig'


node2Sig :: Sig.PSignal [] Double
node2Sig = Sig.fromList node2Sig'


node0Sig' :: [Double]
node0Sig' = [1, 2, 3,  2,  0, -4, -5, -3, 0, -1, -2, 0, 3, 1,  0, -1, -2, -1]


node2Sig' :: [Double]
node2Sig' = [1, 2, 0, -2, -3, -4, -5, -3, 0,  1,  2, 3, 1, 0, -1, -2, -1,  0]



eta01 :: Double -> Double
eta01 x = if x < 0 then 1/n else n
  where n = 0.5

eta12 :: Double -> Double
eta12 x = if x < 0 then 1/n else n
  where n = 0.4

eta13 :: Double -> Double
eta13 x = if x < 0 then 1/n else n
  where n = 0.7


env :: EqGen.EquationSystem Node.Int s (Data Nil Double) (Data ([] :> Nil) Double)
env =
  mconcat $
  (XIdx.dTime sec0 .= Data.map (const 1) sig0) :
  (XIdx.power sec0 node0 node1 .= sig0) :
  (XIdx.power sec0 node2 node1 .= sig2) :

  ((EqGen.variable $ XIdx.eta sec0 node0 node1) =.=
    EqGen.liftF (Data.map eta01) (EqGen.variable $ XIdx.power sec0 node0 node1)) :

  ((EqGen.variable $ XIdx.eta sec0 node1 node2) =.=
    EqGen.liftF (Data.map eta12) (EqGen.variable $ XIdx.power sec0 node2 node1)) :

  ((EqGen.variable $ XIdx.eta sec0 node1 node3) =.=
    EqGen.liftF (Data.map eta13) (EqGen.variable $ XIdx.power sec0 node1 node3)) :

  []
  where sig0 = Sig.unpack node0Sig
        sig2 = Sig.unpack node2Sig

main :: IO ()
main = do


  let seqTopo = constructSeqTopo topoDreibein [0]

      e = EqGen.solve seqTopo env
      pm = Env.powerMap (Env.signal e)

      rec :: Rec.PowerRecord Node.Int [] Double
      rec = Rec.Record time (Map.mapKeys f $ Map.mapMaybe g pm)

      f (Idx.InPart _ (Idx.Power edge)) = Idx.PPos edge

      g (Res.Determined dat) = Just $ Sig.TC dat
      g _ = Nothing

  concurrentlyMany_ [
    PlotIO.record "Power Signals" DefaultTerm.cons show id rec,
    PlotIO.recordList_extract "Power Signals" DefaultTerm.cons show id [(Rec.Name "bla", rec)]
                              [ Idx.PPos (Idx.StructureEdge node1 node0),
                                Idx.PPos (Idx.StructureEdge node1 node2),
                                Idx.PPos (Idx.StructureEdge node1 node3) ],

    Draw.xterm $ Draw.sequFlowGraph seqTopo ]
