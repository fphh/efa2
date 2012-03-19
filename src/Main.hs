
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import EFA2.Term.Term
import EFA2.Term.EquationOrder

import EFA2.Graph.Graph
import EFA2.Utils.Utils
import EFA2.Graph.SignalGraph
import EFA2.Signal.SignalAnalysis

import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

import EFA2.Example.Dreibein
import EFA2.Example.Linear



main :: IO ()
main = do
--  let input = S.fromList [Energy 4 3, Energy 0 1]
    let  (g, rec, mapping) = linear
--    sequ = genSequ rec     
--    sequRec = genSequRec sequ rec
       
--  writeTopology g
    drawTopologyX (g)
    
--    sectflowVals = calcFlowVals (g, sequRec) 
    
--    drawFlowX (g,rec)
--  writeDependencyGraph g
--  print sigs
--  print (makeEtaEnv g sigs)
--  print sectRecord


