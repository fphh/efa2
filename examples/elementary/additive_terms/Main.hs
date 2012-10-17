{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Set (Set)

import Debug.Trace (trace)

import Text.Printf (printf)

import EFA2.Solver.Equation
import EFA2.Solver.EquationOrder (order)
import EFA2.Solver.IsVar (maybeStaticVar)

import EFA2.Interpreter.Interpreter
          (eqToInTerm, interpretFromScratch, interpretTerm)
import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith (Val)

import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (Sc)

import EFA2.Topology.Topology (makeAllEquations)
import EFA2.Topology.TopologyData (Topology)

import EFA2.Display.DrawGraph (drawDeltaTopology, drawTopology, drawAll)

import EFA2.Example.Dreibein


symbolic :: Topology -> Envs EqTerm
symbolic g = mapEqTermEnv (setEqTerms (emptyEnv { dxMap = dx1eq })) res
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

        ts0 = snd $ makeAllEquations g [envs0]
        ts1 = snd $ makeAllEquations g [envs1]

        ts0o = order ts0
        ts1o = order ts1
        difftseq = mkDiffEqTermEquations 0 ts1o

        ts =
           toAbsEquations $ order $ map assignToEquation $
           ts0o ++ ts1o ++ difftseq
        res = interpretEqTermFromScratch ts

numeric :: Topology -> Envs Sc
numeric g =  trace ("---------\n" ++ showAssigns ts1o ++ "\n------\n") res
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

        ts = toAbsEquations $ ts0o ++ ts1o ++ difftseq
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

instance MyShow DPowerIdx where
         myshow (DPowerIdx s r f t) = "dP_" ++ show s ++ "." ++ show r ++ "_" ++ show f ++ "." ++ show t

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
  let g = graph

      sym = symbolic g
      num = numeric g


      dpnum = dpowerMap num
      dpsym = dpowerMap sym
      dpsymEq = M.map pushMult dpsym

      dpsymIn = dpsym
      dpsyminterp = M.map (interpretTerm 1 num) dpsymIn

      detailsSym = M.map additiveTerms dpsymEq

      details :: M.Map DPowerIdx [Val]
      details = M.map (map (S.fromScalar . interpretTerm 1 num)) detailsSym

      sumdetails = M.map sum details

      control = dpowerMap (deltaEnv g)
      vars = M.map (length . map (mkVarSet maybeStaticVar)) detailsSym

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

  putStrLn "\n== Additive terms per stack =="
  putStrLn (format $ M.toList vars)

  drawAll [
    drawTopology g ((mapEqTermEnv ((:[]) . simplify) sym) { recordNumber = SingleRecord 0 }),
    drawDeltaTopology g ((mapEqTermEnv additiveTerms sym) { recordNumber = SingleRecord 1 }) ]

