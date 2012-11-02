module EFA2.Example.Loop (loop, etas, pows, dtimes) where

import qualified EFA2.Signal.Index as Idx
import EFA2.Topology.Topology
          (makeWithDirEdges, makeEdges, makeNodes)
import EFA2.Topology.TopologyData
          (defaultELabel, flowDirection,
           FlowDirection(AgainstDir), NodeType(Crossing))
import EFA2.Topology.EfaGraph (mkGraph)

import EFA2.Interpreter.Arith (Val)
import EFA2.Interpreter.Env
          (DTimeMap, EnergyMap, FEtaMap, PowerMap)
import EFA2.Example.SymSig (TheGraph(TheGraph))
import EFA2.Utils.Utils (pairs)

import qualified Data.Map as M


numOf :: Int
numOf = 3

dtimes :: DTimeMap [Val]
dtimes = M.fromList [(Idx.DTime (Idx.Section 0) (Idx.Record 0), [2, 2, 2])]

sigs :: EnergyMap [Val]
sigs =
   M.mapKeys (uncurry (Idx.Energy (Idx.Section 0) (Idx.Record 0))) $
   M.fromList $
      ((,) 0 1, [2.3, 2.4, 3]) :
      ((,) 1 0, [2, 2.1, 2.2]) :
      ((,) 1 2, replicate numOf 1.8) :
      ((,) 2 1, replicate numOf 1.0) :
      ((,) 2 3, replicate numOf 1.1) :
      ((,) 3 2, replicate numOf 0.5) :
      ((,) 1 4, replicate numOf 0.4) :
      ((,) 4 1, replicate numOf 0.3) :
      ((,) 4 5, replicate numOf 0.1) :
      ((,) 5 4, replicate numOf 0.05) :
      ((,) 4 2, replicate numOf 0.2) :
      ((,) 2 4, replicate numOf 0.1) :
      []

etas :: FEtaMap [Val]
etas =
   M.mapKeys (uncurry (Idx.FEta (Idx.Section 0) (Idx.Record 0))) $
   M.fromList $
      ((,) 0 1, map (\x -> x/(x+1))) :
      ((,) 1 0, map (\x -> x/(x+1))) :
      ((,) 1 2, map (\x -> x/(x+1))) :
      ((,) 2 1, map (\x -> x/(x+1))) :
      ((,) 2 3, map (\x -> x/(x+1))) :
      ((,) 3 2, map (\x -> x/(x+1))) :
      ((,) 1 4, map (\x -> x/(x+1))) :
      ((,) 4 1, map (\x -> x/(x+1))) :
      ((,) 4 5, map (\x -> x/(x+1))) :
      ((,) 5 4, map (\x -> x/(x+1))) :
      ((,) 4 2, map (\x -> x/(x+1))) :
      ((,) 2 4, map (\x -> x/(x+1))) :
      []


pows :: PowerMap [Val]
pows =
   M.mapKeys (uncurry (Idx.Power (Idx.Section 0) (Idx.Record 0))) $
   M.fromList $
      ((,) 0 1, [1, 2, 3]) :
      ((,) 1 0, replicate numOf 2.2) :
      ((,) 1 2, replicate numOf 1.8) :
      ((,) 2 1, replicate numOf 1.0) :
      ((,) 2 3, replicate numOf 1.1) :
      ((,) 3 2, replicate numOf 0.5) :
      ((,) 1 4, replicate numOf 0.4) :
      ((,) 4 1, replicate numOf 0.3) :
      ((,) 4 5, replicate numOf 0.1) :
      ((,) 5 4, replicate numOf 0.05) :
      ((,) 4 2, replicate numOf 0.2) :
      ((,) 2 4, replicate numOf 0.1) :
      []


loop :: TheGraph [Val]
loop = TheGraph g sigs
  where g = mkGraph ns es
        es = makeWithDirEdges (pairs no)
             ++ makeEdges [(2, 4, defaultELabel { flowDirection =  AgainstDir})] -- Why do we have to do this?
             ++ makeWithDirEdges (pairs (1:no2))
        no = [0..3]
        no2 = [4, 5]
        ns = makeNodes $ map f (no ++ no2)
        f x = (x, Crossing)
