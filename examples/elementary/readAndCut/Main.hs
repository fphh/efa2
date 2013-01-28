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

import EFA.IO.Import (modelicaCSVImport)
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Sequence (makeSequence, makeSeqFlowGraph)
import qualified EFA.Signal.Signal as Sig

import qualified EFA.Graph.Draw as Draw


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

node0, node1, node2, node3 :: Idx.Node
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Idx.Node 0


topoDreibein :: TD.Topology
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Crossing),
              (node2, TD.Sink),
              (node3, TD.Storage)]
        es = [(node0, node1), (node1, node2), (node1, node3)]


given :: EqGen.EquationSystem s Double
given = foldMap (uncurry (.=)) $
  (EqGen.dtime Idx.initSection, 1) :
  []

main :: IO ()
main = do
  SD.Record time sigMap <- modelicaCSVImport "modThreeWay_sto.RecA_res.csv"


  let pRec :: SD.PowerRecord [] Double
      pRec = SD.PowerRecord (Sig.fromList $ Sig.toList time) 
                            (M.map (Sig.fromList . Sig.toList) pMap)


      pMap =  M.fromList [
        (SD.PPosIdx node0 node1, sigMap M.! (SD.SigId "powercon1.u")),
        (SD.PPosIdx node1 node0, sigMap M.! (SD.SigId "powercon2.u")),
        (SD.PPosIdx node1 node2, sigMap M.! (SD.SigId "powercon3.u")),
        (SD.PPosIdx node2 node1, sigMap M.! (SD.SigId "powercon4.u")),
        (SD.PPosIdx node1 node3, sigMap M.! (SD.SigId "powercon5.u")),
        (SD.PPosIdx node3 node1, sigMap M.! (SD.SigId "powercon6.u")) ]
      --seq :: SequData (PowerRecord [] Double)
      --seq = chopAtZeroCrossingsPowerRecord pRec

      sequ = makeSequence pRec

      sequTopo = makeSeqFlowGraph topoDreibein sequ

      env = EqGen.solve given sequTopo

  -- print pRec
  Draw.sequFlowGraphAbsWithEnv sequTopo env
