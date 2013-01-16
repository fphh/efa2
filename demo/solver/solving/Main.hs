
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import System.IO

import Debug.Trace


import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.DirEquation
import EFA2.Solver.DependencyGraph
import EFA2.Solver.IsVar
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.InTerm
import EFA2.Equation.Env
import EFA2.Interpreter.Arith


main :: IO ()
main = do
  let x1 = XIdx 0 0 0 1
      x2 = XIdx 0 0 0 2
      x3 = XIdx 0 0 0 3

      e1 = PowerIdx 0 0 0 1
      e2 = PowerIdx 0 0 0 2
      e3 = PowerIdx 0 0 0 3
      values = M.fromList [(e1, [2]), (e2, [3]), (e3, [7])]

{-
      -- This will not solve, because we have at least two unknown variables per equation.

      ts = [ give e1,
             give e2,
             x1 !+ x2 != (1 :: Val),
             x1 !* e2 != x2 !* e1 ]
-}
      -- This will solve.
{-
      ts = [ give e1,
             give e2,
             x1 != Recip (e1 !+ e2) !* e1,
             x1 !+ x2 != (1 :: Val) ]
-}
      -- This also will work.
      ts = [ give e1,
             give e2,
             give e3,
             x1 != Recip (e1 !+ e2 !+ e3) !* e1,
             x2 != Recip (e1 !+ e2 !+ e3) !* e2,
             x1 !+ x2 !+ x3 != (1.0 :: Val) ]

      isV = isVarFromEqs ts

      (given, nov, givExt, rest) = splitTerms isV ts

      ho = hornOrder isV givExt rest
      dirs = directEquations isV ho
      envs = Envs values M.empty M.empty M.empty M.empty M.empty

      gd = map (eqToInTerm envs) (given ++ dirs)

      res :: Envs [Val]
      res = interpretFromScratch gd

  putStrLn (showEqTerms ts)
  putStrLn "===================="

  putStrLn ("Number of undeq: " ++ show (length ts))
  putStrLn ("Number of given: " ++ show (length given))
  putStrLn ("Number of noVar: " ++ show (length nov))

  putStrLn ("Number of gvExt: " ++ show (length givExt))
  putStrLn ("Number of rest : " ++ show (length rest))

  putStrLn "===================="
  putStrLn ("Number of equations solved: " ++ show (length dirs))
  putStrLn (showInTerms gd)
  putStrLn "===================="
  print res
