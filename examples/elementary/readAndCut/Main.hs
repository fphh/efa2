{-# LANGUAGE FlexibleContexts #-}

module Main where

import EFA.Example.Utility (makeEdges)
import EFA.Equation.Absolute ((.=))

import qualified EFA.Equation.Absolute as EqGen

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Signal.Signal as Sig

import EFA.IO.CSVImport (modelicaCSVImport)
-- import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Sequence (makeSequence, makeSeqFlowGraph)
import EFA.Signal.Record
          (SigId(SigId), Record(Record), PPosIdx(PPosIdx), PowerRecord)

import qualified Data.Map as M


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

node0, node1, node2, node3 :: Node.Int
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Node.Int 0


topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Crossing),
              (node2, TD.Sink),
              (node3, TD.Storage)]
        es = [(node0, node1), (node1, node2), (node1, node3)]


given :: EqGen.EquationSystem Node.Int s Double
given =
  (Idx.DTime Idx.initSection .= 1)

main :: IO ()
main = do
  Record time sigMap <- modelicaCSVImport "modThreeWay_sto.RecB_res.csv"


  let pMap =  M.fromList [
        (PPosIdx node0 node1, sigMap M.! (SigId "powercon1.u")),
        (PPosIdx node1 node0, sigMap M.! (SigId "powercon2.u")),
        (PPosIdx node1 node2, sigMap M.! (SigId "powercon3.u")),
        (PPosIdx node2 node1, sigMap M.! (SigId "powercon4.u")),
        (PPosIdx node1 node3, sigMap M.! (SigId "powercon5.u")),
        (PPosIdx node3 node1, sigMap M.! (SigId "powercon6.u")) ]

      pRec = Record time (M.map (Sig.fromList . Sig.toList) pMap) :: PowerRecord Node.Int [] Double

      sequ = makeSequence pRec

      sequTopo = makeSeqFlowGraph topoDreibein sequ

      env = EqGen.solve given sequTopo

  Draw.sequFlowGraphAbsWithEnv sequTopo env
