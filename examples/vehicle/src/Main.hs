{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Foldable (foldMap)
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified Data.Map as M

import EFA.Example.Utility (makeEdges, (.=))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph as Gr
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.System as EqGen

import EFA.IO.ASCIIImport (modelicaASCIIImport)
import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Plot as PL
import EFA.Signal.Sequence (makeSequence, makeSeqFlowGraph)
import qualified EFA.Signal.Signal as Sig

import qualified EFA.Report.Report as Rep

import EFA.Graph.Draw -- (drawTopology)


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

tank, con_engine, el_cross, battery, node4, node5, node6, node7, node8, node9, node10, node11 :: Idx.Node
tank :~ con_engine :~ el_cross :~ battery :~ node4 :~ node5 :~ node6 :~ node7 :~ node8 :~ node9 :~ node10 :~ node11 :~ _ = Stream.enumFrom $ Idx.Node 0


topo :: TD.Topology
topo = Gr.mkGraph ns (makeEdges es)
  where ns = [(tank, TD.Source),
              (con_engine , TD.Crossing),
              (el_cross, TD.Crossing),
              (battery, TD.Storage),
              (node4, TD.Crossing),  -- electric crossing to vehicle electric system
              (node5, TD.Crossing),  -- connection motor / gearbox
              (node6, TD.Crossing),  -- connection brakes
              (node7, TD.Crossing),  -- connection inertia
              (node8, TD.Sink),      -- driving resistance                  
              (node9, TD.Sink),      -- vehicle electric system
              (node10, TD.Sink),     -- vehicle brakes
              (node11, TD.Storage)]  -- vehicle inertia
             
        es = [(tank, con_engine),  -- ic engine
              (con_engine, el_cross),  -- generator
              (el_cross, battery),  -- battery
              (el_cross, node4),  -- electric connection        
              (node4, node5),  -- motor
              (node5, node6),  -- gearbox 
              (node6, node7),  -- wheels             
              (node7, node8),  -- driving resistance
              (node4, node9),  -- dcdc          
              (node6, node10), -- brakes
              (node7, node11)] -- inertia             

given :: EqGen.EquationSystem s Double
given = foldMap (uncurry (.=)) $
  (EqGen.dtime Idx.initSection, 1) :
  []

main :: IO ()
main = do
  
  -- ## import signals
  rec <- modelicaASCIIImport "Vehicle_res.asc"
  
  -- Get imported Signals 
  let SD.Record time sigMap = rec
      sigIDList = M.toList(sigMap)
      
  -- Plot imported Signals
  mapM_ (\ (x,y) -> PL.sigPlot (show x) y) (take 10 sigIDList)
  mapM_ (\ (x,_) -> putStrLn(show x))  (take 10 sigIDList) -- sigIDList 
  -- PL.rPlot ("Roh",rec)  
  
  -- ## Calculate Power Signals  

{-
  -- ## Populate Power Record
  let pRec :: SD.PowerRecord [] Double
      pRec = SD.PowerRecord (Sig.fromList $ Sig.toList time) 
                             (M.map (Sig.fromList . Sig.toList) pMap)
             
      -- Define Power Record
      pMap =  M.fromList [
         (SD.PPosIdx tank con_engine, sigMap M.! (SD.SigId "sig_1")),
         (SD.PPosIdx con_engine tank, sigMap M.! (SD.SigId "sig_2")),
         (SD.PPosIdx con_engine el_cross, sigMap M.! (SD.SigId "sig_3")),
         (SD.PPosIdx el_cross con_engine, sigMap M.! (SD.SigId "sig_4")),
         (SD.PPosIdx con_engine battery, sigMap M.! (SD.SigId "sig_5")),
         (SD.PPosIdx battery con_engine, sigMap M.! (SD.SigId "sig_6")) ]
  PL.rPlot ("Test",pRec) 
  -- print pRec  
  
  -- ## Analysis
  let sol = StateAnalysis.advanced topo
  
  -- seq = chopAtZeroCrossingsPowerRecord pRec

       -- sequ = makeSequence pRec

       -- sequTopo = makeSeqFlowGraph topo sequ

--      env = EqGen.solve given sequTopo

  putStrLn ("Number of flow states: " ++ show (length sol))
  drawTopologyXs' [head sol]
  -- drawTopology sequTopo env

  Rep.report [Rep.RAll] ("Test",pRec)
-}  