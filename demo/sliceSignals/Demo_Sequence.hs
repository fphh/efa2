{-# LANGUAGE GADTs, 
FlexibleContexts, TypeOperators #-}

module Main where

import qualified Data.Map as M
import qualified Data.Vector as V
import Debug.Trace
import Data.Monoid

import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Solver.Equation
import EFA2.Solver.EquationOrder

import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.Arith

import EFA2.Display.DrawGraph

import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Sequence 
import EFA2.Signal.SequenceData
import EFA2.Signal.Signal (TC(TC), PSigL, UTFSig, Test1, (.++),rtail,rhead,rsingleton,RSig,RSamp1)
import EFA2.Signal.Typ
-- import EFA2.Signal.Vector (Length)
-- import EFA2.Signal.Data (Apply, Data, (:>) , Nil)
import EFA2.Display.Plot


import EFA2.Utils.Utils


 

mkSig :: Int -> ([Val] -> PSigL)
mkSig n = S.fromList . concat . replicate n

time = take 13 [0 ..]

s01 = [0, 2, 2, 0, 0, 0]
s10 = [0, 0.8, 0.8, 0, 0, 0]
s12 = [0.3, 0.3, 0.3, 0.3, 0.3, 0.3]
s21 = [0.2, 0.2, 0.2, 0.2, 0.2, 0.2]
s13 = [0, 0.5, 0.5, -0.3, -0.3, -0.3]
s31 = [0, 0.25, 0.25, 0, -0.6, -0.6]

n = 2

pMap =  M.fromList [ (PPosIdx 0 1, mkSig n s01 .++ (S.fromList [head s01] :: PSigL)),
                     (PPosIdx 1 0, mkSig n s10 .++ (S.fromList [head s10] :: PSigL)),
                     (PPosIdx 1 2, mkSig n s12 .++ (S.fromList [head s12] :: PSigL)),
                     (PPosIdx 2 1, mkSig n s21 .++ (S.fromList [head s21] :: PSigL)),
                     (PPosIdx 1 3, mkSig n s13 .++ (S.fromList [head s13] :: PSigL)),
                     (PPosIdx 3 1, mkSig n s31 .++ (S.fromList [head s31] :: PSigL)) ]


pRec = PowerRecord (S.fromList time) pMap
pRec0 = addZeroCrossings pRec
(sequ,sequRec) = genSequ pRec0

          
main :: IO ()
main = do


  putStrLn (show time)
  putStrLn (show pRec)  
  putStrLn (show pRec0)  
  
  putStrLn (show sequ)
  
  rPlot ("PowerRecord",pRec)
  rPlot ("Sequ",sequRec)
