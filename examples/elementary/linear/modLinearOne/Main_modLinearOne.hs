
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Control.Monad.Error

import Debug.Trace

import EFA2.Topology.RandomTopology
import EFA2.Topology.Graph
import EFA2.Topology.GraphData

import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.DirEquation
import EFA2.Interpreter.Env

import EFA2.Solver.DependencyGraph
import EFA2.Interpreter.Arith

import EFA2.Utils.Utils
import EFA2.Signal.Sequence
import EFA2.IO.Import

import EFA2.Display.DrawGraph
import EFA2.Example.SymSig

import EFA2.Signal.Sequence
import EFA2.Topology.Flow
--import EFA2.Topology.Flow


-- define topology 
g' :: Gr NLabel ELabel
g' = mkGraph (makeNodes no) (makeEdges no) 
  where no = [0..1]
        

main :: IO ()
main = do
  rec@(Record time sigMap) <- modelicaCSVImport "./linear_res.csv"
  let 
--    time = [0,10..100]
    pRec = PowerRecord time pMap              
    pMap =  M.fromList [ (PPosIdx 0 1,  sigMap M.! (SigId "eflow1.u")),
                         (PPosIdx 1 0,  sigMap M.! (SigId "eflow2.u"))]


--    pMap = M.fromList [ (PPosIdx 0 1,[0,1,2,2,3,4,5,-5,-3,-3,4])]
--                        (PPosIdx 1 0,[0,1,2,-2,-3,4,5,-5,-3,-3,4])]   
    
    pRec0 = addZeroCrossings pRec        
    (sequ,sqPRec) = genSequ pRec0          
    
    sqFRec = genSequFlow sqPRec
    sqFStRec = genSequFState sqFRec
    
    sqFlowTops = genSequFlowTops g' sqFStRec
      

--  drawAll [drawTopologyX' g']
  
  putStrLn "PowerRecord"
  putStrLn (myShowList $ genXSig pRec)

  putStrLn "PowerRecord + ZeroPoints"
  putStrLn (myShowList $ genXSig pRec0)

  putStrLn "Sequence"
  putStrLn (show sqPRec)

  putStrLn "Sequence Flow"
  putStrLn (show sqFRec)

  putStrLn "Sequence Flow"
  putStrLn (show sqFStRec)
  
  drawSequFlowTops sqFlowTops
  
  
  return ()

