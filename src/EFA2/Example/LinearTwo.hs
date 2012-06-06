{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module EFA2.Example.LinearTwo where

import Data.Graph.Inductive
import qualified Data.Map as M

import Control.Monad.Error

import EFA2.Topology.Topology
import EFA2.Topology.TopologyData

import EFA2.Interpreter.InTerm
import EFA2.Interpreter.Env
import EFA2.Example.SymSig
import EFA2.Utils.Utils
import EFA2.Signal.Signal
import EFA2.Signal.Typ
import EFA2.Signal.Base
import EFA2.Signal.Data



type InTermScalar = TC Scalar (Typ UT UT UT) (Data Nil (InTerm Val))


dtimes0ss :: DTimeMap InTermScalar
dtimes0ss = M.fromList [ (DTimeIdx 0 0, toScalar (DTIdx (DTimeIdx 0 0))) ]

sigs0ss :: EnergyMap InTermScalar
sigs0ss = M.fromList [ (EnergyIdx 0 0 0 1, toScalar (EIdx (EnergyIdx 0 0 0 1))) ]

eta0ss :: FEtaMap InTermScalar
eta0ss = M.fromList [ (FEtaIdx 0 0 1 0, smap $ const (FNIdx (FEtaIdx 0 0 1 0))), 
                      (FEtaIdx 0 0 0 1, smap $ const (FNIdx (FEtaIdx 0 0 0 1))),
                      (FEtaIdx 0 0 1 2, smap $ const (FNIdx (FEtaIdx 0 0 1 2))), 
                      (FEtaIdx 0 0 2 1, smap $ const (FNIdx (FEtaIdx 0 0 2 1))) ]

dtimes1ss:: DTimeMap InTermScalar
dtimes1ss = M.fromList [ (DTimeIdx 0 1, toScalar (DTIdx (DTimeIdx 0 1))) ]

sigs1ss :: EnergyMap InTermScalar
sigs1ss = M.fromList [ (EnergyIdx 0 1 0 1, toScalar (EIdx (EnergyIdx 0 1 0 1))) ]

dpower1ss :: DPowerMap InTermScalar
dpower1ss = M.fromList [ (DPowerIdx 0 1 0 1, toScalar (DPIdx (DPowerIdx 0 1 0 1))) ]

eta1ss :: FEtaMap InTermScalar
eta1ss = M.fromList [ (FEtaIdx 0 1 1 0, smap $ const (FNIdx (FEtaIdx 0 1 1 0))), 
                      (FEtaIdx 0 1 0 1, smap $ const (FNIdx (FEtaIdx 0 1 0 1))),
                      (FEtaIdx 0 1 1 2, smap $ const (FNIdx (FEtaIdx 0 1 1 2))), 
                      (FEtaIdx 0 1 2 1, smap $ const (FNIdx (FEtaIdx 0 1 2 1))) ]

deta1ss :: DEtaMap InTermScalar
deta1ss = M.fromList [ (DEtaIdx 0 1 1 0, smap $ const (DNIdx (DEtaIdx 0 1 1 0))), 
                       (DEtaIdx 0 1 0 1, smap $ const (DNIdx (DEtaIdx 0 1 0 1))),
                       (DEtaIdx 0 1 1 2, smap $ const (DNIdx (DEtaIdx 0 1 1 2))), 
                       (DEtaIdx 0 1 2 1, smap $ const (DNIdx (DEtaIdx 0 1 2 1))) ]


linearTwo :: TheGraph InTermScalar
linearTwo = TheGraph g undefined
  where g = mkGraph ns es
        ns = makeNodes [(0, Source), (1, Crossing), (2, Sink)]
        es = makeEdges [(0, 1, defaultELabel), (1, 2, defaultELabel)]
