{-# LANGUAGE TypeSynonymInstances #-}

module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import System.IO

import Debug.Trace

import Text.Printf

import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.DirEquation
import EFA2.Solver.DependencyGraph
import EFA2.Solver.IsVar
import EFA2.Solver.EquationOrder

import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.InTerm
import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith

import EFA2.Signal.Signal

import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

--import EFA2.Topology.RandomTopology

import EFA2.Utils.Utils

import EFA2.Display.FileSave
import EFA2.Display.DrawGraph

import EFA2.Example.SymSig
--import EFA2.Example.Loop
--import EFA2.Example.LinearTwo
import EFA2.Example.Dreibein
--import EFA2.Example.Dreibein2
--import EFA2.Example.Dreibein3

symbolic :: Topology -> Envs EqTerm
symbolic g = res
  where

        envs0 = emptyEnv { recordNumber = SingleRecord 0,
                           powerMap = power0eq,
                           dtimeMap = dtimes0eq,
                           xMap = x0eq,
                           fetaMap = eta0eq }

        envs1 = emptyEnv { recordNumber = SingleRecord 1,
                           powerMap = power1eq,
                           dpowerMap = dpower1eq,
                           fetaMap = eta1eq,
                           detaMap = deta1eq,
                           xMap = x1eq,
                           dxMap = dx1eq,
                           dtimeMap = dtimes1eq }

        (envs0', ts0) = makeAllEquations g [envs0]
        (envs1', ts1) = makeAllEquations g [envs1]

        ts0o = order ts0
        ts1o = order ts1
        difftseq = mkDiffEqTermEquations 0 ts1o

        ts = toAbsEqTermEquations $ order (ts0o ++ ts1o ++ difftseq)
        res = interpretEqTermFromScratch ts

numeric :: Topology -> Envs Sc
numeric g = trace ("---------\n" ++ showEqTerms ts1o ++ "\n------\n") res
  where envs0 = emptyEnv { recordNumber = SingleRecord 0,
                           powerMap = power0num,
                           dtimeMap = dtimes0num,
                           xMap = x0num,
                           fetaMap = eta0num }

        envs1 = emptyEnv { recordNumber = SingleRecord 1,
                           powerMap = power1num,
                           dpowerMap = dpower1num,
                           fetaMap = eta1num,
                           detaMap = deta1num,
                           xMap = x1num,
                           dxMap = dx1num,
                           dtimeMap = dtimes1num }

        (envs0', ts0) = makeAllEquations g [envs0]
        (envs1', ts1) = makeAllEquations g [envs1]

        ts0o = order ts0
        ts1o = order ts1
        difftseq = mkDiffEqTermEquations 0 ts1o

        envs = envUnion [envs0', envs1']

        ts = toAbsEqTermEquations $ ts0o ++ ts1o ++ difftseq
        res = interpretFromScratch (recordNumber envs) 1 (map (eqToInTerm envs) ts)

deltaEnv :: Topology -> Envs Sc
deltaEnv g = res1 `minusEnv` res0
  where 
        envs0 = emptyEnv { recordNumber = SingleRecord 0,
                           powerMap = power0num,
                           dtimeMap = dtimes0num,
                           xMap = x0num,
                           fetaMap = eta0num }

        envs1 = emptyEnv { recordNumber = SingleRecord 1,
                           powerMap = power1num,
                           --dpowerMap = dpower1num,
                           --detaMap = deta1num,
                           dtimeMap = dtimes1num,
                           xMap = x1num,
                           fetaMap = eta1num }

        (envs0', ts0) = makeAllEquations g [envs0]
        (envs1', ts1) = makeAllEquations g [envs1]

        ts0' = toAbsEqTermEquations $ order ts0
        ts1' = toAbsEqTermEquations $ order ts1

        res0 = interpretFromScratch (recordNumber envs0') 1 (map (eqToInTerm envs0') ts0')
        res1 = interpretFromScratch (recordNumber envs1') 1 (map (eqToInTerm envs1') ts1')


class MyShow a where
      myshow :: a -> String



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

instance MyShow a => MyShow (S.Set a) where
         myshow s = myshow $ S.toList s

format :: (MyShow a, MyShow b) => [(a, b)] -> String
format xs = L.intercalate "\n" (map f xs)
  where f (x, y) = myshow x ++ " = " ++ myshow y


main :: IO ()
main = do
  let g = graph

      sym = symbolic g
      num = numeric g


      dpnum = dpowerMap num
      dpsym = dpowerMap sym
      dpsymEq = M.map pushMult dpsym

      dpsymIn = M.map (eqToInTerm emptyEnv) dpsym
      dpsyminterp = M.map (interpretWithEnv 1 num) dpsymIn

      detailsSym = M.map additiveTerms dpsymEq

      details :: M.Map DPowerIdx [Val]
      details = M.map (map (fromScalar . interpretWithEnv 1 num . eqToInTerm emptyEnv)) detailsSym
      
      sumdetails = M.map sum details

      control = dpowerMap (deltaEnv g)
      vars = M.map (map (mkVarSet isStaticVar)) detailsSym

  putStrLn "\n== Control delta environment (later env - former env, computed independently) =="
  putStrLn (format $ M.toList control)


  putStrLn "\n== Numeric solution =="
  putStrLn (format $ M.toList dpnum)


  putStrLn "\n== Symbolic solution =="
  putStrLn (format $ M.toList dpsymEq)



  putStrLn "\n== Numeric interpretation of symbolic solution =="
  putStrLn (format $ M.toList dpsyminterp)

  putStrLn "\n== Symbolic additive terms =="
  putStrLn (format $ M.toList detailsSym)

  putStrLn "\n== Numeric additive terms =="
  putStrLn (format $ M.toList details)

  putStrLn "\n== Sums of numeric additive terms =="
  putStrLn (format $ M.toList sumdetails)

  putStrLn "\n== Variables per stack term =="
  putStrLn (format $ M.toList vars)


{-
dP_0.1_0.1 = 0.500000
dP_0.1_1.0 = 0.750000
dP_0.1_1.2 = -0.015000
dP_0.1_1.3 = 0.765000
dP_0.1_2.1 = 0.082500
dP_0.1_3.1 = 0.832500
-}