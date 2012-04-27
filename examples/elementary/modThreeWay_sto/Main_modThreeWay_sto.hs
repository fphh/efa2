
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Control.Monad.Error
import Control.Applicative

import Debug.Trace

import EFA2.Topology.Topology

import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.DirEquation
import EFA2.Solver.IsVar
import EFA2.Solver.DependencyGraph

import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.Arith

import EFA2.Utils.Utils
import EFA2.IO.Import

import EFA2.Display.DrawGraph

import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData

import EFA2.Topology.Flow
import EFA2.Topology.TopologyData


-- define topology 
topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0,Source),(1,Crossing),(2,Sink),(3,Storage)]
        edges = [(0,1,ELabel),(1,2,ELabel),(1,3,ELabel)]

main :: IO ()
main = do
  Record time sigMap <- modelicaCSVImport "./modThreeWay_sto.RecA_res.csv"
  
  let pRec = PowerRecord time pMap              
      pMap =  M.fromList [ (PPosIdx 0 1,  sigMap M.! (SigId "powercon1.u")),
                           (PPosIdx 1 0,  sigMap M.! (SigId "powercon2.u")), 
                           (PPosIdx 1 2,  sigMap M.! (SigId "powercon3.u")),
                           (PPosIdx 2 1,  sigMap M.! (SigId "powercon4.u")),                            
                           (PPosIdx 1 3,  sigMap M.! (SigId "powercon5.u")),
                           (PPosIdx 3 1,  sigMap M.! (SigId "powercon6.u"))]                            


      (sqEnvs, sqTopo) = x pRec topo
      sigs = M.unions (map powerMap sqEnvs)
      
      
      ts = envToEqTerms sigs ++ mkEdgeEq sqTopo ++ mkNodeEq sqTopo

      isV = isVarFromEqs ts

      (given, noVariables, givExt, rest) = splitTerms isV ts
      ho = hornOrder isV givExt rest
      dirs = directEquations isV ho
      envs = Envs sigs M.empty M.empty M.empty M.empty M.empty

      gd = map (eqToInTerm envs) (given ++ dirs)

      res :: Envs [Val]
      res = interpretFromScratch gd

  {-
  putStrLn "Sequence"
  putStrLn (myShowList sequ)
  
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
    -}

  putStrLn (showInTerms gd)
  
  drawTopologyX' sqTopo
  
  -- drawSequFlowTops sqFlowTops
  drawTopology sqTopo res
  print res
  
  
  
  return ()

