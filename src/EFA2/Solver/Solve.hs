{-# LANGUAGE FlexibleInstances #-}


module EFA2.Solver.Solve where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Graph.Inductive

import Control.Monad.Error


--import EFA2.Graph.GraphData
--import EFA2.Graph.DependencyGraph
--import EFA2.Graph.Graph
import EFA2.Signal.Arith

import EFA2.Solver.Equation
import EFA2.Solver.Horn
import EFA2.Solver.DirEquation
import EFA2.Solver.Env
import EFA2.Solver.TermData
import EFA2.Solver.EqInterpreter

{-
data AbsEnv a = AbsEnv (EtaEnv a) (XEnv a)

data DiffEnv a = DiffEnv (DPowerEnv a) (DEtaEnv a) (EtaEnv a) (XEnv a)

emptyLRPowerEnv :: LRPowerEnv a
emptyLRPowerEnv idx = throwError (PowerIdxError idx M.empty)

emptyLRDPowerEnv :: LRDPowerEnv a
emptyLRDPowerEnv idx = throwError (DPowerIdxError idx M.empty)

emptyLREtaEnv :: LREtaEnv a
emptyLREtaEnv idx = throwError (EtaIdxError idx M.empty)

emptyLRDEtaEnv :: LRDEtaEnv a
emptyLRDEtaEnv idx = throwError (DEtaIdxError idx M.empty)

emptyLRXEnv :: LRXEnv a
emptyLRXEnv idx = throwError (XIdxError idx M.empty)


mkGiven :: (Signal a) => [(PowerIdx, [Val])] -> ([EqTerm b], M.Map PowerIdx [a])
mkGiven xs = (give $ map (Energy . fst) xs, M.fromList (map (fmap (map toSignal)) xs))
-}

{-
symbolicAbsEnv :: AbsEnv [InTerm]
symbolicAbsEnv = AbsEnv (mkEnv mkSymEtaEnv) (mkEnv mkSymXEnv)

symbolicDiffEnv :: DiffEnv [InTerm]
symbolicDiffEnv = DiffEnv (mkEnv mkSymDPowerEnv) (mkEnv mkSymDEtaEnv) (mkEnv mkSymEtaEnv) (mkEnv mkSymXEnv)
-}
{-
class Solver env where
      --environment :: (Arith a) => TheGraph [a] -> env [a]
      solve :: (Signal a, Arith a, Show a) => TheGraph [a] -> env [a] -> [(PowerIdx, [Val])] -> M.Map PowerIdx [a]

instance Solver AbsEnv where
         --environment (TheGraph g sigs) = AbsEnv (mkEnv $ mkEtaEnv g sigs) (mkEnv $ mkXEnv g sigs)

         solve (TheGraph g _) (AbsEnv eenv xenv) given = M.union penv' res
           where gvs :: [EqTerm Abs]
                 (gvs, penv') = mkGiven given
                 depg = makeDependencyGraph g gvs
                 ho = hornOrder depg gvs
                 dirEqs = directEquations ho
                 inTs = toInTerms dirEqs
                 res = undefined -- solveInTerms (mkEnv emptyLRDPowerEnv) (mkEnv emptyLRDEtaEnv) eenv xenv penv' inTs
-}
{-
instance Solver DiffEnv where
         environment (TheGraph g sigs) = undefined -- DiffEnv (mkEnv $ mkEtaEnv g sigs) (mkEnv $ mkXEnv g sigs)

         solve (TheGraph g _) (DiffEnv dpenv deenv eenv xenv) given = M.union penv' res
           where gvs :: [EqTerm Diff]
                 (gvs, penv') = mkGiven given
                 depg = makeDependencyGraph g gvs
                 ho = hornOrder depg gvs
                 dirEqs = directEquations ho
                 inTs = toInTerms dirEqs
                 res = solveInTerms dpenv deenv eenv xenv penv' inTs

-}


{-
solveInTerms :: (Arith a) 
                => DPowerEnv [a] -> DEtaEnv [a] -> EtaEnv [a] -> XEnv [a] -> PowerMap [a] -> [InTerm] -> PowerMap [a]
solveInTerms dpenv deenv eenv xenv penv ts = M.fromList $ snd $ L.foldl' f (penv', []) ts
  where penv' = mkPowerEnv penv
        f (pacc, sol) (InEqual (PIdx idx) t) = (newPEnv `composeLREnv` pacc, (idx, val):sol)
          where val = interpret dpenv deenv eenv xenv (mkEnv pacc) t
                newPEnv x | idx == x = return val
                newPEnv idx = throwError (PowerIdxError idx)
-}

{-
solveInTerms :: (Interpreter a, EnvClass a) => M.Map PowerIdx a -> LREtaEnv a -> LRXEnv a -> [InTerm] -> M.Map PowerIdx a
solveInTerms penv eenv xenv ts = M.fromList $ snd $ L.foldl' f (penv', []) ts
  where eenv' = mkEnv eenv
        xenv' = mkEnv xenv
        penv' = mkPowerEnv penv
        f (pacc, sol) (InEqual (PIdx idx) t) = (newPEnv `composeLREnv` pacc, (idx, val):sol)
          where val = interpret (mkEnv pacc) eenv' xenv' t
                newPEnv x | idx == x = return val
                newPEnv idx = throwError (PowerIdxError idx)
-}