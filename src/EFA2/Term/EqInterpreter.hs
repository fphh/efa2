

module EFA2.Term.EqInterpreter where

import qualified Data.Map as M

import EFA2.Graph.Graph
import EFA2.Term.Equation

import EFA2.Signal.SignalData

--interpretLhs :: (Arithmetic sig cont p n p) => PowerEnv (sig cont p) -> EtaEnv (sig cont n) -> XEnv (sig cont n) -> EqTerm -> sig cont p

interpretLhs :: (Arithmetic sig cont dimp dimn dimp) 
             => PowerEnv (sig cont dimp) -> EtaEnv (sig cont dimn) -> XEnv (sig cont dimn) -> EqTerm -> sig cont dimp
interpretLhs penv nenv xenv (Given (Energy x y)) = penv M.! (PowerIdx x y)
--interpretLhs penv nenv xenv (Recip t) = reciprocal (interpretLhs penv nenv xenv t)
interpretLhs penv nenv xenv (F (Energy x y)) = p .* n 
  where p = penv M.! (PowerIdx x y)
        n = nenv M.! (EtaIdx x y)
interpretLhs penv nenv xenv (B (Energy x y)) = p ./ n 
  where p = penv M.! (PowerIdx x y)
        n = nenv M.! (EtaIdx x y)
interpretLhs penv nenv xenv (Add s t) = s' .+ t'
  where s' = interpretLhs penv nenv xenv s
        t' = interpretLhs penv nenv xenv t
interpretLhs penv nenv xenv (Mult s t) = s' .+ t'
  where s' = interpretLhs penv nenv xenv s
        t' = interpretLhs penv nenv xenv t

        

{-
data EqTerm = EqTerm := EqTerm
          | Eta Int Int
          | Energy Int Int
          | X Int Int
          | F EqTerm
          | B EqTerm
          | Given EqTerm
          | Minus EqTerm
          | Recip EqTerm
          | Add EqTerm EqTerm
          | Mult EqTerm EqTerm deriving (Show, Eq, Ord)
-}