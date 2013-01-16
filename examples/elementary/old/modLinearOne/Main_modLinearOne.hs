
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Control.Monad.Error
import Control.Applicative

import Debug.Trace

--import EFA2.Topology.RandomTopology
import EFA2.Topology.Topology
-- import EFA2.Topology.GraphData

import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.DirEquation
import EFA2.Solver.IsVar
import EFA2.Solver.DependencyGraph

import EFA2.Equation.Env
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.Arith

import EFA2.Utility
import EFA2.Signal.Sequence
import EFA2.IO.Import

import EFA2.Topology.Draw
import EFA2.Example.SymSig

import EFA2.Signal.Sequence
import EFA2.Topology.Flow
--import EFA2.Topology.Flow


-- import EFA2.Example.LinearOne

-- define topology 
g' :: Gr NLabel ()
g' = mkGraph (makeNodes nodes) (makeEdges edges) 
   where nodes = [(0,Source),(1,Sink)]
         edges = [(0,1)]

main :: IO ()
main = do
  rec@(Record time sigMap) <- modelicaCSVImport "./modLinearOne.RectA_res.csv"
  
  let pRec = PowerRecord time pMap              
      pMap =  M.fromList [ (PPosIdx 0 1,  sigMap M.! (SigId "powercon1.u")),
                           (PPosIdx 1 0,  sigMap M.! (SigId "powercon2.u"))]


  --    pMap = M.fromList [ (PPosIdx 0 1,[0,1,2,2,3,4,5,-5,-3,-3,4])]
  --                        (PPosIdx 1 0,[0,1,2,-2,-3,4,5,-5,-3,-3,4])]   
      
      pRec0 = addZeroCrossings pRec        
      (sequ,sqPRec) = genSequ pRec0          
      
      sqFRec = genSequFlow sqPRec
      sqFStRec = genSequFState sqFRec
      
      sqFlowTops = genSequFlowTops g' sqFStRec
      sqSecTops = genSectionTopology sqFlowTops
      sqTopo = mkSequenceTopology sqSecTops
      
      SequData sqEnvs = fmap (map (\(s, rec) -> fromFlowRecord s (RecIdx 0) rec) . zip (map SecIdx $ listIdx sequ)) sqFRec
      sigs = M.unions (map powerMap sqEnvs)
      
      
      ts = envToEqTerms sigs ++ mkEdgeEq sqTopo ++ mkNodeEq sqTopo
      varset = L.foldl' f S.empty ts
      f acc (v := Given) = S.insert v acc
      f acc _ = acc
      isV = isVarFromEqs varset

      (given, noVariables, givExt, rest) = splitTerms isV ts

      ho = hornOrder isV givExt rest
      dirs = directEquations isV ho
      --envs = Envs sigs M.empty esigs M.empty xsigs M.empty
      envs = Envs sigs M.empty M.empty M.empty M.empty M.empty

      gd = map (eqToInTerm envs) (given ++ dirs)

      res :: Envs [Val]
      res = interpretFromScratch gd

  
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
  
  putStrLn (showInTerms gd)
  
  
  drawTopologyX' sqTopo
  
  -- drawSequFlowTops sqFlowTops
  drawTopology sqTopo res
  print res
  
  
  
  return ()

