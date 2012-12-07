
module Main where

import qualified Data.Map as M

import EFA2.Topology.EfaGraph (mkGraph)
import EFA2.Topology.Topology (makeAllEquations, makeNodes, makeSimpleEdges)
import EFA2.Topology.TopologyData

import EFA2.Solver.Equation (give, toAbsEquations)
import EFA2.Solver.EquationOrder (order)

import qualified EFA2.Signal.Index as Idx
import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.Arith (Val)

import EFA2.Topology.Draw (drawTopology)

import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Sequence as Seq
import EFA2.Signal.SequenceData (PPosIdx(PPosIdx), PowerRecord(..), SequData(..))
import EFA2.Signal.Signal (PSigL, TSigL)


topo :: Topology
topo = mkGraph (makeNodes nodes) (makeSimpleEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage)]
        edges = [(0, 1), (1, 2), (1, 3)]


mkSig :: Int -> ([Val] -> PSigL)
mkSig n = S.fromList . concat . replicate n


rec :: Idx.Record
rec = Idx.Record 0

secm, sec3 :: Idx.Section
secm = Idx.initSection
sec3 = Idx.Section 3

pPosIdx :: Int -> Int -> PPosIdx
pPosIdx x y = PPosIdx (Idx.Node x) (Idx.Node y)

edgeIdx ::
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.Section -> Int -> Int -> idx
edgeIdx mkIdx sec x y =
   mkIdx rec (Idx.SecNode sec (Idx.Node x)) (Idx.SecNode sec (Idx.Node y))


main :: IO ()
main = do
  
  let s01 = [0, 2, 2, 0]
      s10 = [0, 0.8, 0.8, 0]
      s12 = [0.3, 0.3, 0.3, 0.3]
      s21 = [0.2, 0.2, 0.2, 0.2]
      s13 = [0, 0.5, 0.5, 0]
      s31 = [0, 0.25, 0.25, 0]

      s01' = [0, 0]
      s10' = [0, 0]
      s12' = [0.3, 0.3]
      s21' = [0.2, 0.2]
      s13' = [-0.3, -0.3]
      s31' = [-0.6, -0.6]
      n = 2

      time :: TSigL
      time = S.fromList ([0, 0] ++ take 20 [1..])

      pMap =  M.fromList [ (pPosIdx 0 1, mkSig n (s01 ++ s01')),
                           (pPosIdx 1 0, mkSig n (s10 ++ s10')),
                           (pPosIdx 1 2, mkSig n (s12 ++ s12')),
                           (pPosIdx 2 1, mkSig n (s21 ++ s21')),
                           (pPosIdx 1 3, mkSig n (s13 ++ s13')),
                           (pPosIdx 3 1, mkSig n (s31 ++ s31')) ]

      sqFRec = Seq.makeSequence (PowerRecord time pMap)
      sqTopo = Seq.makeSeqFlowGraph topo sqFRec
      sqEnvs = Seq.makeRecSequence sqFRec

      -- storage0 = Idx.Power secm rec 24 25
      -- storage0 = Idx.Power secm rec 16 17
      storage0 = edgeIdx Idx.Power secm 0 1

      (sqEnvs', ts') =
         makeAllEquations sqTopo (case sqEnvs of SequData l -> map g l) -- { recordNumber = SingleRecord rec })
      g x = x { recordNumber = SingleRecord rec }
      sigs = powerMap sqEnvs'
      ts = [give storage0] ++ ts'

      envs = sqEnvs' { recordNumber = SingleRecord rec,
                       powerMap = M.insert storage0 (S.fromList [3.0]) (M.map (S.map abs) sigs),
                       fetaMap = M.singleton (edgeIdx Idx.FEta sec3 3 1) (S.map (const 0.4)) }


      gd = map (eqToInTerm envs) (toAbsEquations $ order ts)

      --res :: Envs [Val]
      res = interpretFromScratch (SingleRecord rec) 1 gd


  print sqEnvs'
  putStrLn (unlines $ map showInEquation gd)

  --putStrLn (show $ length gd)
  drawTopology sqTopo res
