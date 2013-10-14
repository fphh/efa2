{-# LANGUAGE TypeOperators #-}
module EFA.Flow.Topology.Record where

import qualified EFA.Flow.Quantity as Quant

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Topology as FlowTopoPlain

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import EFA.Graph.Topology (Topology)
import EFA.Graph (DirEdge(DirEdge), unDirEdge)

import qualified EFA.Signal.Signal as Signal
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Record as Record
import EFA.Signal.Record (Record(Record), FlowRecord)
import EFA.Signal.Signal (fromScalar)
import EFA.Signal.Data (Data, Nil, (:>))
import EFA.Signal.Base (Sign(PSign, NSign, ZSign), BSum, DArith0)

import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Utility.Map as MapU

import Control.Applicative (pure)

import qualified Data.Map as Map


data Flow a = Flow {flowOut, flowIn :: a}

instance Functor Flow where
   fmap f (Flow o i) = Flow (f o) (f i)

type Section node v a =
        FlowTopoPlain.Section
           node Graph.EitherEdge
           (Signal.TSignal v a)
           (Node.Type ())
           (Maybe (Flow (Signal.FFSignal v a)))

flowTopologyFromRecord ::
   (Ord node, Show node,
    Fractional a, Ord a, BSum a, DArith0 a,
    SV.Walker v, SV.Storage v a) =>
   Topology node ->
   FlowRecord node v a ->
   Section node v a
flowTopologyFromRecord topo (Record time fs) =
   FlowTopoPlain.Section time $
   Graph.fromMap (Graph.nodeLabels topo) $
   Map.unionsWith (error "flowTopologyFromRecord: duplicate edges") $
   Map.elems $
   Map.mapWithKey
      (\(DirEdge idx1 idx2) () ->
         let look = MapU.checkedLookup "Flow.flowTopologyFromRecord" fs
             normal   = look $ XIdx.ppos idx1 idx2
             opposite = look $ XIdx.ppos idx2 idx1
         in  case fromScalar $ Signal.sign $ Signal.sum normal of
                PSign ->
                   Map.singleton
                      (Graph.EDirEdge $ DirEdge idx1 idx2)
                      (Just $ Flow {flowOut = normal, flowIn = opposite})
                NSign ->
                   Map.singleton
                      (Graph.EDirEdge $ DirEdge idx2 idx1)
                      (Just $ Flow {flowOut = Signal.neg opposite, flowIn = Signal.neg normal})
                ZSign ->
                   Map.singleton
                      (Graph.EUnDirEdge $ unDirEdge idx1 idx2)
                      Nothing) $
   Graph.edgeLabels topo

fromSection ::
   (BSum a, SV.Zipper v, SV.Walker v, SV.Singleton v, SV.Storage v a,
    Node.C node) =>
   FlowTopoPlain.Section node Graph.EitherEdge
      (Signal.TSignal v a) (FlowTopo.Sums (Result (Data (v :> Nil) a)))
      (Maybe (Flow (Signal.FFSignal v a))) ->
   FlowTopo.Section node (Result (Data (v :> Nil) a))
fromSection (FlowTopoPlain.Section dtime topo) =
   FlowTopoPlain.Section
      (Determined . Signal.unpack . Signal.delta $ dtime)
      (Graph.mapEdge
         (fmap (fullFlow . fmap (Determined . Signal.unpack)))
         topo)


sectionToPowerRecord ::
   (Ord node) =>
   FlowTopo.Section node (Data (v :> Nil) a) ->
   Record.PowerRecord node v a
sectionToPowerRecord (FlowTopoPlain.Section time topo) =
   Record.Record (Signal.TC time) $
   Map.unionsWith (error "envToPowerRecord: duplicate edges") $
   Map.elems $
   Map.mapWithKey
      (\e flow ->
         let se = Topo.structureEdgeFromDirEdge e
         in  Map.fromList $
                (Idx.PPos se, Signal.TC $ Quant.flowPowerOut flow) :
                (Idx.PPos $ Idx.flip se, Signal.TC $ Quant.flowPowerIn flow) :
                []) $
   Graph.edgeLabels $
   Quant.dirFromFlowGraph topo

fullFlow :: Flow (Result a) -> Quant.Flow (Result a)
fullFlow flow =
   (pure Undetermined) {
      Quant.flowEnergyOut = flowOut flow,
      Quant.flowEnergyIn = flowIn flow
   }
