{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified EFA2.Topology.EfaGraph as Gr

import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Set (Set)

import Text.Printf (printf)

import qualified EFA2.Report.Format as Format
import qualified EFA2.Solver.Equation as Equ
import EFA2.Solver.Equation
          (MkTermC, mkTerm,
           EqTerm, Term(Const), toAbsEquations, mapEqTermEnv)
import EFA2.Solver.EquationOrder (order)

import EFA2.Interpreter.Interpreter
          (eqToInTerm, interpretFromScratch, interpretTerm)
import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith (Val)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (Sc, toScalar, fromScalar)

import EFA2.Topology.Topology (makeAllEquations)
import EFA2.Topology.TopologyData
          (SequFlowGraph, NodeType(..), ELabel, defaultELabel)

import EFA2.Topology.Draw (drawDeltaTopology, drawTopology, drawAll)


symbolic :: SequFlowGraph -> Envs SingleRecord EqTerm
symbolic g = res' { recordNumber = SingleRecord rec1, dxMap = dx1sym, detaMap = deta1sym, fetaMap = eta1sym }
  where ts0 = snd $ makeAllEquations g [envs0sym]
        ts1 = snd $ makeAllEquations g [envs1sym]

        ts0o = order ts0
        ts1o = order ts1
        difftseq = Equ.mkDiffEqTermEquations rec0 ts1o

        ts =
           toAbsEquations $ order $ map Equ.assignToEquation $
           ts0o ++ ts1o ++ difftseq
        res = Equ.interpretEqTermFromScratch ts
        --res' = mapEqTermEnv (setEqTerms (emptyEnv { dxMap = dx1sym })) (res { fetaMap = eta1sym, recordNumber = SingleRecord 1 })
        res' = mapEqTermEnv (Equ.setEqTerms (emptyEnv { dxMap = dx1sym })) res

numeric :: SequFlowGraph -> Envs MixedRecord Sc
numeric g = {- trace ("---------\n" ++ showEqTerms ts ++ "\n------\n") $ -} res
  where (envs0', ts0) = makeAllEquations g [envs0num]
        (envs1', ts1) = makeAllEquations g [envs1num]

        ts0o = order ts0
        ts1o = order ts1
        difftseq = Equ.mkDiffEqTermEquations rec0 ts1o

        envs = envUnion [envs0', envs1']

        ts = toAbsEquations $ ts0o ++ ts1o ++ difftseq
        res = interpretFromScratch (recordNumber envs) 1 (map (eqToInTerm envs) ts)


deltaEnv :: SequFlowGraph -> Envs SingleRecord Sc
deltaEnv g = (res1 `minusEnv` res0) { recordNumber = SingleRecord rec1 }
  where (envs0', ts0) = makeAllEquations g [envs0num]
        (envs1', ts1) = makeAllEquations g [envs1num]

        ts0' = toAbsEquations $ order ts0
        ts1' = toAbsEquations $ order ts1

        res0 = interpretFromScratch (recordNumber envs0') 1 (map (eqToInTerm envs0') ts0')
        res1 = interpretFromScratch (recordNumber envs1') 1 (map (eqToInTerm envs1') ts1')


class MyShow a where
      myshow :: a -> String

instance MyShow Int where
         myshow = show

instance MyShow Val where
         myshow = printf "%.6f"

instance MyShow Sc where
         myshow = show

instance MyShow Idx.DPower where
         myshow (Idx.DPower r f t) =
            Format.unUnicode $
            Format.edgeVar Format.Delta Format.Power r f t

instance MyShow a => MyShow [a] where
         myshow xs = "[ " ++ L.intercalate ", " (map myshow xs) ++ " ]"

instance MyShow a => MyShow (Set a) where
         myshow s = myshow $ Set.toList s

format :: (MyShow a, MyShow b) => [(a, b)] -> String
format xs = L.intercalate "\n" (map f xs)
  where f (x, y) = myshow x ++ " = " ++ myshow y



main :: IO ()
main = do
  let
      sym = symbolic graph
      symSimple = mapEqTermEnv ((:[]) . Equ.simplify) sym
      symSimpleDelta = mapEqTermEnv (concatMap Equ.additiveTerms) symSimple

      [num0, num1] = separateEnvs $ numeric graph
      denv = deltaEnv graph
      control = dpowerMap denv

      symSimpleNum = mapEqTermEnv (map (fromScalar . interpretTerm 1 (envUnion [num0, num1]))) symSimpleDelta


  putStrLn "\n== Control delta environment (later env - former env, computed independently) =="
  putStrLn (format $ M.toList control)

  print (dxMap sym)
  drawAll [
    drawTopology graph num0,
    drawTopology graph num1,
    drawTopology graph symSimple,
    drawDeltaTopology graph denv,
    drawDeltaTopology graph symSimpleDelta,
    drawDeltaTopology graph symSimpleNum ]

  --print (M.size $ fetaMap sym)


sec :: Idx.Section
sec = Idx.Section 0

rec0, rec1 :: Idx.Record
rec0 = Idx.Record 0
rec1 = Idx.Record 1

edgeIdx ::
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.Record -> Int -> Int -> idx
edgeIdx mkIdx rec x y =
   mkIdx rec (Idx.SecNode sec (Idx.Node x)) (Idx.SecNode sec (Idx.Node y))



-- Numeric =====================================================================

dtimes0num :: DTimeMap Sc
dtimes0num = M.fromList [ (Idx.DTime rec0 sec, toScalar 1.0) ]

power0num :: PowerMap Sc
power0num = M.fromList [ (edgeIdx Idx.Power rec0 0 2, toScalar 3.0) ]

eta0num :: FEtaMap Sc
eta0num = M.fromList $
   (edgeIdx Idx.FEta rec0 0 2, S.map $ const 0.8) :
   (edgeIdx Idx.FEta rec0 2 0, S.map $ const 0.8) :
   (edgeIdx Idx.FEta rec0 1 2, S.map $ const 0.8) :
   (edgeIdx Idx.FEta rec0 2 1, S.map $ const 0.8) :
   (edgeIdx Idx.FEta rec0 2 3, S.map $ const 0.8) :
   (edgeIdx Idx.FEta rec0 3 2, S.map $ const 0.82) :
   (edgeIdx Idx.FEta rec0 2 4, S.map $ const 0.8) :
   (edgeIdx Idx.FEta rec0 4 2, S.map $ const 0.8) :
   []

x0num :: XMap Sc
x0num = M.fromList $
   (edgeIdx Idx.X rec0 2 0, toScalar 0.3) :
   (edgeIdx Idx.X rec0 2 3, toScalar 0.4) :
   []


envs0num :: Envs SingleRecord Sc
envs0num = emptyEnv { recordNumber = SingleRecord rec0,
                      powerMap = power0num,
                      dtimeMap = dtimes0num,
                      xMap = x0num,
                      fetaMap = eta0num }

----------------------------------------------------------------------------------

dtimes1num :: DTimeMap Sc
dtimes1num = M.fromList [ (Idx.DTime rec1 sec, toScalar 1.0) ]

power1num :: PowerMap Sc
power1num = M.fromList [ (edgeIdx Idx.Power rec1 0 2, toScalar 4.0) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (edgeIdx Idx.DPower rec1 0 2, toScalar 1.0) ]


eta1num :: FEtaMap Sc
eta1num = M.fromList $
   (edgeIdx Idx.FEta rec1 0 2, S.map $ const 0.85) :
   (edgeIdx Idx.FEta rec1 2 0, S.map $ const 0.85) :
   (edgeIdx Idx.FEta rec1 1 2, S.map $ const 0.85) :
   (edgeIdx Idx.FEta rec1 2 1, S.map $ const 0.85) :
   (edgeIdx Idx.FEta rec1 2 3, S.map $ const 0.85) :
   (edgeIdx Idx.FEta rec1 3 2, S.map $ const 0.85) :
   (edgeIdx Idx.FEta rec1 2 4, S.map $ const 0.85) :
   (edgeIdx Idx.FEta rec1 4 2, S.map $ const 0.85) :
   []

deta1num :: DEtaMap Sc
deta1num = M.fromList $
   (edgeIdx Idx.DEta rec1 0 2, S.map $ const 0.05) :
   (edgeIdx Idx.DEta rec1 2 0, S.map $ const 0.05) :
   (edgeIdx Idx.DEta rec1 1 2, S.map $ const 0.05) :
   (edgeIdx Idx.DEta rec1 2 1, S.map $ const 0.05) :
   (edgeIdx Idx.DEta rec1 2 3, S.map $ const 0.05) :
   (edgeIdx Idx.DEta rec1 3 2, S.map $ const 0.05) :
   (edgeIdx Idx.DEta rec1 2 4, S.map $ const 0.05) :
   (edgeIdx Idx.DEta rec1 4 2, S.map $ const 0.05) :
   []

x1num :: XMap Sc
x1num = M.fromList $
   (edgeIdx Idx.X rec1 2 0, toScalar 0.3) :
   (edgeIdx Idx.X rec1 2 3, toScalar 0.4) :
   []


dx1num :: DXMap Sc
dx1num = M.fromList $
   (edgeIdx Idx.DX rec1 0 2, toScalar 0.0) :
   (edgeIdx Idx.DX rec1 2 0, toScalar 0.0) :
   (edgeIdx Idx.DX rec1 1 2, toScalar 0.0) :
   (edgeIdx Idx.DX rec1 2 1, toScalar 0.0) :
   (edgeIdx Idx.DX rec1 2 3, toScalar 0.0) :
   (edgeIdx Idx.DX rec1 3 2, toScalar 0.0) :
   (edgeIdx Idx.DX rec1 2 4, toScalar 0.0) :
   (edgeIdx Idx.DX rec1 4 2, toScalar 0.0) :
   []

envs1num :: Envs SingleRecord Sc
envs1num = emptyEnv { recordNumber = SingleRecord rec1,
                      powerMap = power1num,
                      dpowerMap = dpower1num,
                      fetaMap = eta1num,
                      detaMap = deta1num,
                      xMap = x1num,
                      dxMap = dx1num,
                      dtimeMap = dtimes1num }


-- ================================================================================

-- Symbolic =====================================================================

selfMap :: (MkTermC a, Ord a) => [a] -> M.Map a EqTerm
selfMap xs = M.fromList $ map (\x -> (x, mkTerm x)) xs


selfEta :: (MkTermC a, Ord a) => [a] -> M.Map a (b -> EqTerm)
selfEta ns = M.fromList $ map (\x -> (x, const $ mkTerm x)) ns

dtimes0sym :: DTimeMap EqTerm
dtimes0sym = selfMap [ Idx.DTime rec0 sec ]


power0sym :: PowerMap EqTerm
power0sym = selfMap [ edgeIdx Idx.Power rec0 0 2 ]

eta0sym :: FEtaMap EqTerm
eta0sym = selfEta $
   edgeIdx Idx.FEta rec0 0 2 : edgeIdx Idx.FEta rec0 2 0 :
   edgeIdx Idx.FEta rec0 1 2 : edgeIdx Idx.FEta rec0 2 1 :
   edgeIdx Idx.FEta rec0 2 3 : edgeIdx Idx.FEta rec0 3 2 :
   edgeIdx Idx.FEta rec0 2 4 : edgeIdx Idx.FEta rec0 4 2 :
   []

x0sym :: XMap EqTerm
x0sym = selfMap [ edgeIdx Idx.X rec0 2 0, edgeIdx Idx.X rec0 2 3 ]


envs0sym :: Envs SingleRecord EqTerm
envs0sym = emptyEnv { recordNumber = SingleRecord rec0,
                      powerMap = power0sym,
                      dtimeMap = dtimes0sym,
                      xMap = x0sym,
                      fetaMap = eta0sym }

----------------------------------------------------------------------------------


dtimes1sym :: DTimeMap EqTerm
dtimes1sym = selfMap [ Idx.DTime rec1 sec ]

power1sym :: PowerMap EqTerm
power1sym = selfMap [ edgeIdx Idx.Power rec1 0 2 ]

dpower1sym :: DPowerMap EqTerm
dpower1sym = selfMap [ edgeIdx Idx.DPower rec1 0 2 ]


eta1sym :: FEtaMap EqTerm
eta1sym = selfEta $
   edgeIdx Idx.FEta rec1 0 2 : edgeIdx Idx.FEta rec1 2 0 :
   edgeIdx Idx.FEta rec1 1 2 : edgeIdx Idx.FEta rec1 2 1 :
   edgeIdx Idx.FEta rec1 2 3 : edgeIdx Idx.FEta rec1 3 2 :
   edgeIdx Idx.FEta rec1 2 4 : edgeIdx Idx.FEta rec1 4 2 :
   []

deta1sym :: DEtaMap EqTerm
deta1sym = selfEta $
   edgeIdx Idx.DEta rec1 0 2 : edgeIdx Idx.DEta rec1 2 0 :
   edgeIdx Idx.DEta rec1 1 2 : edgeIdx Idx.DEta rec1 2 1 :
   edgeIdx Idx.DEta rec1 2 3 : edgeIdx Idx.DEta rec1 3 2 :
   edgeIdx Idx.DEta rec1 2 4 : edgeIdx Idx.DEta rec1 4 2 :
   []

x1sym :: XMap EqTerm
x1sym = selfMap [ edgeIdx Idx.X rec1 2 0, edgeIdx Idx.X rec1 2 3 ]


dx1sym :: DXMap EqTerm
dx1sym = M.union m1 m2
  where m1 =
           M.fromList $
              (edgeIdx Idx.DX rec1 0 2, Const 0.0) :
              (edgeIdx Idx.DX rec1 1 2, Const 0.0) :
              (edgeIdx Idx.DX rec1 3 2, Const 0.0) :
              (edgeIdx Idx.DX rec1 4 2, Const 0.0) :
              []
        m2 =
           M.fromList $
              (edgeIdx Idx.DX rec1 2 0, Const 0.0) :
              (edgeIdx Idx.DX rec1 2 1, Const 0.0) :
              (edgeIdx Idx.DX rec1 2 3, Const 0.0) :
              (edgeIdx Idx.DX rec1 2 4, Const 0.0) :
              []

envs1sym :: Envs SingleRecord EqTerm
envs1sym = emptyEnv { recordNumber = SingleRecord rec1,
                      powerMap = power1sym,
                      dpowerMap = dpower1sym,
                      fetaMap = eta1sym,
                      detaMap = deta1sym,
                      xMap = x1sym,
                      dxMap = dx1sym,
                      dtimeMap = dtimes1sym }

-- ================================================================================

makeNode :: Int -> Idx.SecNode
makeNode = Idx.SecNode sec . Idx.Node

makeNodes :: [(Int, NodeType)] -> [Gr.LNode Idx.SecNode NodeType]
makeNodes ns = map f ns
  where f (n, ty) = (makeNode n, ty)

makeEdges :: [(Int, Int)] -> [Gr.LEdge Idx.SecNode ELabel]
makeEdges es = map f es
  where f (a, b) = (Gr.Edge (makeNode a) (makeNode b), defaultELabel)

graph :: SequFlowGraph
graph = Gr.mkGraph ns es
  where ns = makeNodes [(0, Source), (1, Source), (2, Crossing), (3, Sink), (4, Sink)]
        es = makeEdges [(0, 2), (1, 2), (2, 3), (2, 4)]
