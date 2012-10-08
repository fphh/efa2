{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Graph.Inductive (mkGraph)

import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Set (Set)

import Text.Printf (printf)

import EFA2.Solver.Equation
import EFA2.Solver.EquationOrder (order)

import EFA2.Interpreter.Interpreter
          (eqToInTerm, interpretFromScratch, showInTerm, interpretWithEnv)
import EFA2.Interpreter.InTerm (InTerm)
import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith (Val)

import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (Sc, toScalar, fromScalar)

import EFA2.Topology.Topology (makeAllEquations, makeNodes, makeEdges)
import EFA2.Topology.TopologyData (Topology, NodeType(..), defaultELabel)

import EFA2.Display.DrawGraph (drawDeltaTopology, drawTopology, drawAll)


symbolic :: Topology -> Envs EqTerm
symbolic g = res' { recordNumber = SingleRecord 1, dxMap = dx1sym, detaMap = deta1sym, fetaMap = eta1sym }
  where ts0 = snd $ makeAllEquations g [envs0sym]
        ts1 = snd $ makeAllEquations g [envs1sym]

        ts0o = order ts0
        ts1o = order ts1
        difftseq = mkDiffEqTermEquations 0 ts1o

        ts = toAbsEqTermEquations $ order (ts0o ++ ts1o ++ difftseq)
        res = interpretEqTermFromScratch ts
        --res' = mapEqTermEnv (setEqTerms (emptyEnv { dxMap = dx1sym })) (res { fetaMap = eta1sym, recordNumber = SingleRecord 1 })
        res' = mapEqTermEnv (setEqTerms (emptyEnv { dxMap = dx1sym })) res

numeric :: Topology -> Envs Sc
numeric g = {- trace ("---------\n" ++ showEqTerms ts ++ "\n------\n") $ -} res
  where (envs0', ts0) = makeAllEquations g [envs0num]
        (envs1', ts1) = makeAllEquations g [envs1num]

        ts0o = order ts0
        ts1o = order ts1
        difftseq = mkDiffEqTermEquations 0 ts1o

        envs = envUnion [envs0', envs1']

        ts = toAbsEqTermEquations $ ts0o ++ ts1o ++ difftseq
        res = interpretFromScratch (recordNumber envs) 1 (map (eqToInTerm envs) ts)


deltaEnv :: Topology -> Envs Sc
deltaEnv g = (res1 `minusEnv` res0) { recordNumber = SingleRecord 1 }
  where (envs0', ts0) = makeAllEquations g [envs0num]
        (envs1', ts1) = makeAllEquations g [envs1num]

        ts0' = toAbsEqTermEquations $ order ts0
        ts1' = toAbsEqTermEquations $ order ts1

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

instance MyShow DPowerIdx where
         myshow (DPowerIdx s r f t) = "dP_" ++ show s ++ "." ++ show r ++ "_" ++ show f ++ "." ++ show t

instance (Show a) => MyShow (InTerm a) where
         myshow = showInTerm

instance MyShow EqTerm where
         myshow = showEqTerm

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
      symSimple = mapEqTermEnv ((:[]) . simplify) sym
      symSimpleDelta = mapEqTermEnv (concatMap additiveTerms) symSimple

      [num0, num1] = separateEnvs $ numeric graph
      denv = deltaEnv graph
      control = dpowerMap denv

      symSimpleNum = mapEqTermEnv (map (fromScalar . interpretWithEnv 1 (envUnion [num0, num1]) . eqToInTerm emptyEnv)) symSimpleDelta


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

-- Numeric =====================================================================

dtimes0num :: DTimeMap Sc
dtimes0num = M.fromList [ (DTimeIdx 0 0, toScalar 1.0) ]

power0num :: PowerMap Sc
power0num = M.fromList [ (PowerIdx 0 0 0 2, toScalar 3.0) ]

eta0num :: FEtaMap Sc
eta0num = M.fromList [ (FEtaIdx 0 0 0 2, S.map $ const 0.8),
                       (FEtaIdx 0 0 2 0, S.map $ const 0.8),
                       (FEtaIdx 0 0 1 2, S.map $ const 0.8),
                       (FEtaIdx 0 0 2 1, S.map $ const 0.8),
                       (FEtaIdx 0 0 2 3, S.map $ const 0.8),
                       (FEtaIdx 0 0 3 2, S.map $ const 0.82),
                       (FEtaIdx 0 0 2 4, S.map $ const 0.8),
                       (FEtaIdx 0 0 4 2, S.map $ const 0.8) ]

x0num :: XMap Sc
x0num = M.fromList [ (XIdx 0 0 2 0, toScalar 0.3),
                     (XIdx 0 0 2 3, toScalar 0.4) ]


envs0num :: Envs Sc
envs0num = emptyEnv { recordNumber = SingleRecord 0,
                      powerMap = power0num,
                      dtimeMap = dtimes0num,
                      xMap = x0num,
                      fetaMap = eta0num }

----------------------------------------------------------------------------------

dtimes1num :: DTimeMap Sc
dtimes1num = M.fromList [ (DTimeIdx 0 1, toScalar 1.0) ]

power1num :: PowerMap Sc
power1num = M.fromList [ (PowerIdx 0 1 0 2, toScalar 4.0) ]

dpower1num :: DPowerMap Sc
dpower1num = M.fromList [ (DPowerIdx 0 1 0 2, toScalar 1.0) ]


eta1num :: FEtaMap Sc
eta1num = M.fromList [ (FEtaIdx 0 1 0 2, S.map $ const 0.85),
                       (FEtaIdx 0 1 2 0, S.map $ const 0.85),
                       (FEtaIdx 0 1 1 2, S.map $ const 0.85),
                       (FEtaIdx 0 1 2 1, S.map $ const 0.85),
                       (FEtaIdx 0 1 2 3, S.map $ const 0.85),
                       (FEtaIdx 0 1 3 2, S.map $ const 0.85),
                       (FEtaIdx 0 1 2 4, S.map $ const 0.85),
                       (FEtaIdx 0 1 4 2, S.map $ const 0.85) ]

deta1num :: DEtaMap Sc
deta1num = M.fromList [ (DEtaIdx 0 1 0 2, S.map $ const 0.05),
                        (DEtaIdx 0 1 2 0, S.map $ const 0.05),
                        (DEtaIdx 0 1 1 2, S.map $ const 0.05),
                        (DEtaIdx 0 1 2 1, S.map $ const 0.05),
                        (DEtaIdx 0 1 2 3, S.map $ const 0.05),
                        (DEtaIdx 0 1 3 2, S.map $ const 0.05),
                        (DEtaIdx 0 1 2 4, S.map $ const 0.05),
                        (DEtaIdx 0 1 4 2, S.map $ const 0.05) ]

x1num :: XMap Sc
x1num = M.fromList [ (XIdx 0 1 2 0, toScalar 0.3),
                     (XIdx 0 1 2 3, toScalar 0.4) ]


dx1num :: DXMap Sc
dx1num = M.fromList [ (DXIdx 0 1 0 2, toScalar 0.0),
                      (DXIdx 0 1 2 0, toScalar 0.0),
                      (DXIdx 0 1 1 2, toScalar 0.0),
                      (DXIdx 0 1 2 1, toScalar 0.0),
                      (DXIdx 0 1 2 3, toScalar 0.0),
                      (DXIdx 0 1 3 2, toScalar 0.0),
                      (DXIdx 0 1 2 4, toScalar 0.0),
                      (DXIdx 0 1 4 2, toScalar 0.0) ]

envs1num :: Envs Sc
envs1num = emptyEnv { recordNumber = SingleRecord 1,
                      powerMap = power1num,
                      dpowerMap = dpower1num,
                      fetaMap = eta1num,
                      detaMap = deta1num,
                      xMap = x1num,
                      dxMap = dx1num,
                      dtimeMap = dtimes1num }


-- ================================================================================

-- Symbolic =====================================================================

selfMap :: (MkVarC a, Ord a) => [a] -> M.Map a EqTerm
selfMap xs = M.fromList $ map (\x -> (x, mkVar x)) xs


selfEta :: (MkVarC a, Ord a) => [a] -> M.Map a (b -> EqTerm)
selfEta ns = M.fromList $ map (\x -> (x, const $ mkVar x)) ns

dtimes0sym :: DTimeMap EqTerm
dtimes0sym = selfMap [ DTimeIdx 0 0 ]


power0sym :: PowerMap EqTerm
power0sym = selfMap [ PowerIdx 0 0 0 2 ]

eta0sym :: FEtaMap EqTerm
eta0sym = selfEta [ FEtaIdx 0 0 0 2, FEtaIdx 0 0 2 0,
                    FEtaIdx 0 0 1 2, FEtaIdx 0 0 2 1,
                    FEtaIdx 0 0 2 3, FEtaIdx 0 0 3 2,
                    FEtaIdx 0 0 2 4, FEtaIdx 0 0 4 2 ]

x0sym :: XMap EqTerm
x0sym = selfMap [ XIdx 0 0 2 0, XIdx 0 0 2 3 ]


envs0sym :: Envs EqTerm
envs0sym = emptyEnv { recordNumber = SingleRecord 0,
                      powerMap = power0sym,
                      dtimeMap = dtimes0sym,
                      xMap = x0sym,
                      fetaMap = eta0sym }

----------------------------------------------------------------------------------


dtimes1sym :: DTimeMap EqTerm
dtimes1sym = selfMap [ DTimeIdx 0 1 ]

power1sym :: PowerMap EqTerm
power1sym = selfMap [ PowerIdx 0 1 0 2 ]

dpower1sym :: DPowerMap EqTerm
dpower1sym = selfMap [ DPowerIdx 0 1 0 2 ]


eta1sym :: FEtaMap EqTerm
eta1sym = selfEta [ FEtaIdx 0 1 0 2, FEtaIdx 0 1 2 0,
                    FEtaIdx 0 1 1 2, FEtaIdx 0 1 2 1,
                    FEtaIdx 0 1 2 3, FEtaIdx 0 1 3 2,
                    FEtaIdx 0 1 2 4, FEtaIdx 0 1 4 2 ]

deta1sym :: DEtaMap EqTerm
deta1sym = selfEta [ DEtaIdx 0 1 0 2, DEtaIdx 0 1 2 0,
                     DEtaIdx 0 1 1 2, DEtaIdx 0 1 2 1,
                     DEtaIdx 0 1 2 3, DEtaIdx 0 1 3 2,
                     DEtaIdx 0 1 2 4, DEtaIdx 0 1 4 2 ]

x1sym :: XMap EqTerm
x1sym = selfMap [ XIdx 0 1 2 0, XIdx 0 1 2 3 ]


dx1sym :: DXMap EqTerm
dx1sym = M.union m1 m2
  where m1 = M.fromList [ (DXIdx 0 1 0 2, Const 0.0),
                          (DXIdx 0 1 1 2, Const 0.0),
                          (DXIdx 0 1 3 2, Const 0.0),
                          (DXIdx 0 1 4 2, Const 0.0) ]
        m2 = M.fromList [ (DXIdx 0 1 2 0, Const 0.0),
                          (DXIdx 0 1 2 1, Const 0.0),
                          (DXIdx 0 1 2 3, Const 0.0),
                          (DXIdx 0 1 2 4, Const 0.0) ]

envs1sym :: Envs EqTerm
envs1sym = emptyEnv { recordNumber = SingleRecord 1,
                      powerMap = power1sym,
                      dpowerMap = dpower1sym,
                      fetaMap = eta1sym,
                      detaMap = deta1sym,
                      xMap = x1sym,
                      dxMap = dx1sym,
                      dtimeMap = dtimes1sym }

-- ================================================================================

graph :: Topology
graph = mkGraph ns es
  where ns = makeNodes [(0, Source), (1, Source), (2, Crossing), (3, Sink), (4, Sink)]
        es = makeEdges [(0, 2, defaultELabel), (1, 2, defaultELabel), (2, 3, defaultELabel), (2, 4, defaultELabel)]



