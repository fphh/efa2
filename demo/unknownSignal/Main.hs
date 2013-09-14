{-# LANGUAGE TypeOperators #-}


module Main where

import EFA.Application.Utility ( topologyFromEdges, seqFlowGraphFromStates )

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.PlotIO as PlotIO
import qualified EFA.Signal.Record as Rec
import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data, (:>), Nil)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((.=), (=.=))

import qualified EFA.Equation.Result as Result

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Map (checkedLookup)

import qualified Data.Map as Map

import Data.Foldable (foldMap, fold)
import Data.Monoid (mconcat, (<>))


sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1, node2, node3 :: Node.Int
node0 = Node.intNoRestriction 0
node1 = Node.intCrossing 0
node2 = Node.intNoRestriction 1
node3 = Node.intNoRestriction 2


topoDreibein :: Topo.Topology Node.Int
topoDreibein =
   topologyFromEdges [(node0, node1), (node1, node2), (node1, node3)]



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


given ::
   EqSys.EquationSystemIgnore Node.Int s
      (Data Nil Double) (Data ([] :> Nil) Double)
given =
   mconcat $
   (XIdx.dTime sec0 .= Data.map (const 1) sig0) :
   (XIdx.power sec0 node0 node1 .= sig0) :
   (XIdx.power sec0 node2 node1 .= sig2) :

   ((EqSys.variable $ XIdx.eta sec0 node0 node1) =.=
     EqSys.liftF (Data.map eta01) (EqSys.variable $ XIdx.power sec0 node0 node1)) :

   ((EqSys.variable $ XIdx.eta sec0 node1 node2) =.=
     EqSys.liftF (Data.map eta12) (EqSys.variable $ XIdx.power sec0 node2 node1)) :

   ((EqSys.variable $ XIdx.eta sec0 node1 node3) =.=
     EqSys.liftF (Data.map eta13) (EqSys.variable $ XIdx.power sec0 node1 node3)) :

   []
   where sig0 = Sig.unpack node0Sig
         sig2 = Sig.unpack node2Sig


main :: IO ()
main =
   case seqFlowGraphFromStates topoDreibein [0] of
      flowGraph -> do

         let rec :: Rec.PowerRecord Node.Int [] Double
             rec =
                Rec.Record time $
                Map.mapMaybe (fmap Sig.TC . Result.toMaybe) $
                foldMap fold $
                Map.mapWithKey
                   (SeqFlow.liftEdgeFlow $
                    \e flow ->
                       case Topo.structureEdgeFromDirEdge e of
                          se ->
                             Map.singleton (Idx.PPos se)
                                (SeqFlow.flowEnergyOut flow)
                             <>
                             Map.singleton (Idx.PPos $ Idx.flip se)
                                (SeqFlow.flowEnergyIn flow)) $
                Graph.edgeLabels $ snd $ snd $
                flip (checkedLookup "rec") sec0 $
                SeqFlow.sequence $ EqSys.solve flowGraph given

         concurrentlyMany_ [
            PlotIO.record "Power Signals" DefaultTerm.cons show id rec,
            PlotIO.recordList_extract "Power Signals" DefaultTerm.cons show id
               [(Rec.Name "bla", rec)]
               [ XIdx.ppos node1 node0,
                 XIdx.ppos node1 node2,
                 XIdx.ppos node1 node3 ],

            Draw.xterm $ Draw.sequFlowGraph Draw.optionsDefault flowGraph ]
