
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe


import Debug.Trace

import EFA2.Topology.Topology
import EFA2.Topology.EfaGraph

import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.DirEquation
import EFA2.Solver.IsVar
import EFA2.Solver.DependencyGraph

import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.Arith
import EFA2.Interpreter.InTerm

import EFA2.Utils.Utils
import EFA2.IO.Import

import EFA2.Display.DrawGraph

import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData

import EFA2.Topology.Flow
import EFA2.Topology.TopologyData
import EFA2.Example.Loop
import EFA2.Example.SymSig


import EFA2.Signal.Signal
import EFA2.Signal.Typ
import EFA2.Signal.Data

import EFA2.Display.ReportSequence

-- define topology 

topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage 0)]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel)]

test :: EfaGraph () ()
test = mkGraph nodes edges
  where nodes = [(0, ()), (1, ()), (2, ()), (3, ())]
        edges = [(0, 1, ()), (1, 2, ()), (1, 3, ())]

main :: IO ()
main = do


  let

      --time = [0, 0, 1, 1]

      s01 = [0, 2, 2, 0] :: [Val]
      s10 = [0, 0.8, 0.8, 0]
      s12 = [0.3, 0.3, 0.3, 0.3]
      s21 = [0.2, 0.2, 0.2, 0.2]
      s13 = [0, 0.5, 0.5, 0]
      s31 = [0, 0.25, 0.25, 0]

      --time' = [1, 2, 2]
      s01' = [0, 0]
      s10' = [0, 0]
      s12' = [0.3, 0.3]
      s21' = [0.2, 0.2]
      s13' = [-0.3, -0.3]
      s31' = [-0.6, -0.6]
      n = 3
      --dtime = replicate n [0, 1, 0]
      --time = foldl (+) 0 dtime
      l = fromIntegral $ length $ replicate n (s01 ++ s01') :: Val
      time = [0, 0] ++ (concatMap (replicate 3) [1.0..l]) 

      pMap =  M.fromList [ (PPosIdx 0 1, sfromList $ concat $ replicate n (s01 ++ s01')),
                           (PPosIdx 1 0, sfromList $ concat $ replicate n (s10 ++ s10')), 
                           (PPosIdx 1 2, sfromList $ concat $ replicate n (s12 ++ s12')),
                           (PPosIdx 2 1, sfromList $ concat $ replicate n (s21 ++ s21')),
                           (PPosIdx 1 3, sfromList $ concat $ replicate n (s13 ++ s13')),
                           (PPosIdx 3 1, sfromList $ concat $ replicate n (s31 ++ s31')) ]

      --(sqEnvs, sqTopo) = makeSequence pRec topo
      pRec = (PowerRecord (sfromList time) pMap)
      
      (sequ,sequPwrRecord) = genSequ pRec
      (sqEnvs, sqTopo) = makeSequence  pRec topo 

      --sigs = M.unions (map powerMap sqEnvs)


      --TheGraph sqTopo sigs = loop
      --storage0 = PowerIdx (-1) 0 24 25
      storage0 = PowerIdx (-1) 0 24 25
      --storage0 = PowerIdx (-1) 0 16 17

      (sqEnvs', ts') = makeAllEquations sqTopo sqEnvs
      --ts = envToEqTerms sigs ++ mkEdgeEq sqTopo ++ mkNodeEq sqTopo
      sigs = M.unions (map powerMap sqEnvs')
      ts = [give storage0] ++ ts'

      isV = isVarFromEqs ts

      (given, noVariables, givExt, rest) = splitTerms isV ts
      ho = hornOrder isV givExt rest
      dirs = directEquations isV ho

      -- envs = Envs (M.insert storage0 [3.0] sigs) M.empty M.empty M.empty M.empty M.empty
      f x | x < 0 = -x
      f x = x
  

      
      envs = emptyEnv { powerMap = M.insert storage0 [3.0] (M.map (map f) sigs) }


      gd = map (eqToInTerm envs) (given ++ dirs)

      res :: Envs [Val]
      res = interpretFromScratch gd
      dirg = makeDirTopology sqTopo
      
  -- print $ toTable show pRec
  printTableToScreen show pRec
  --putStrLn (showEqTerms ts)
 -- putStrLn (showInTerms gd)
  --print sigs
  --print res
  --drawTopology sqTopo res
  --print sqTopo

 
--  drawTopologyX' sqTopo
  --print (sqTopo)
  
  --print test
  --print (fst $ matchAny test)
  --print (fst $ matchAny (snd (matchAny test)))
  --print (fst $ matchAny (snd (matchAny (snd (matchAny test)))))
 -- print (fst $ matchAny (snd (matchAny (snd (matchAny (snd (matchAny test)))))))


{-
  drawAll [
    drawTopologyX' sqTopo,
    drawTopology sqTopo res,
    drawTopologyX' dirg ]
-}

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

  --putStrLn (showEqTerms ts)

  --putStrLn (showInTerms gd)
  
  
  -- drawSequFlowTops sqFlowTops
  --print res
