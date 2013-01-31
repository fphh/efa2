{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Foldable (foldMap)

import qualified Data.Map as M

import EFA.Example.Utility (makeEdges, (.=))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph as Gr
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.System as EqGen

import EFA.IO.CSVImport (modelicaCSVImport)
-- import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Sequence (makeSequence, makeSeqFlowGraph)
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Record

import qualified EFA.Graph.Draw as Draw

import qualified EFA.Graph.Topology.Node as Node

sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

node0, node1, node2, node3 :: Node.Node
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Node.Node 0


topoDreibein :: TD.Topology Node.Node
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Crossing),
              (node2, TD.Sink),
              (node3, TD.Storage)]
        es = [(node0, node1), (node1, node2), (node1, node3)]


given :: EqGen.EquationSystem Node.Node s Double
given = foldMap (uncurry (.=)) $
  (EqGen.dtime Idx.initSection, 1) :
  []

main :: IO ()
main = do
  SignalRecord time sigMap <- modelicaCSVImport "modThreeWay_sto.RecB_res.csv"


  let pMap =  M.fromList [
        (PPosIdx node0 node1, sigMap M.! (SigId "powercon1.u")),
        (PPosIdx node1 node0, sigMap M.! (SigId "powercon2.u")),
        (PPosIdx node1 node2, sigMap M.! (SigId "powercon3.u")),
        (PPosIdx node2 node1, sigMap M.! (SigId "powercon4.u")),
        (PPosIdx node1 node3, sigMap M.! (SigId "powercon5.u")),
        (PPosIdx node3 node1, sigMap M.! (SigId "powercon6.u")) ]

      pRec = PowerRecord time (M.map (Sig.fromList . Sig.toList) pMap)

      sequ = makeSequence pRec

      sequTopo = makeSeqFlowGraph topoDreibein sequ

      env = EqGen.solve given sequTopo

  Draw.sequFlowGraphAbsWithEnv sequTopo env
