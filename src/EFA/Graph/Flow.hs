{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module EFA.Graph.Flow where


import qualified EFA.Graph as Gr
import EFA.Graph
          (Edge(Edge), mkGraph,
           labNodes, labEdges,
           insNodes, insEdges)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.SequenceData (SequData)
import EFA.Signal.Record
          (Record(Record), FlowState(FlowState), FlowRecord,
           PPosIdx(PPosIdx), flipPos, getSig, rmapWithKey)
import EFA.Graph.Topology
          (Topology, FlowTopology, ClassifiedTopology, SequFlowGraph,
           FlowDirection(Dir, UnDir))

import qualified EFA.Graph as G

import qualified EFA.Signal.Vector as SV
import EFA.Signal.Signal (fromScalar, sigSign, sigSum, neg,TC(..))
import qualified EFA.Signal.Signal as S
import EFA.Signal.Data(Data(..), Nil, (:>))
import EFA.Signal.Base (Sign(PSign, NSign, ZSign),BSum, DArith0)

import qualified Data.Foldable as Fold
import qualified Data.Map as M
import Control.Monad (join)

import EFA.Utility (checkedLookup,checkedLookup2)



data Quality = Clean | Dirty | Wrong deriving (Show,Eq)
data Dir = Pos | Neg | Zero deriving (Show,Eq)

type EdgeFlow = (Dir,Quality)

newtype EdgeStates node = EdgeStates (M.Map (G.Edge node ) EdgeFlow) deriving (Show)

getEdgeState :: (Fractional a,
                 Ord a,
                 Ord node,
                 SV.Walker v,
                 SV.Storage v a,
                 BSum a,
                 Show node,
                 Show (v a),
                 SV.Storage v Sign,
                 SV.Singleton v) =>
                Topology node -> FlowRecord node v a -> EdgeStates node
getEdgeState topo rec = EdgeStates $ M.fromList $ zip edges $ map f edges
  where
    edges = M.keys $ G.edgeLabels topo
    f (G.Edge n1 n2)  = case sigSign $ sigSum $ s1 of
                        (TC (Data (PSign))) -> (Pos,quality)
                        (TC (Data (NSign))) -> (Neg,quality)
                        (TC (Data (ZSign))) -> (Zero,quality)

              where s1 = getSig rec (PPosIdx n1 n2)
                    s2 = getSig rec (PPosIdx n2 n1)
                    quality = edgeFlowQuality s1 s2

edgeFlowQuality :: (Num d,
                    SV.Storage v d,
                    BSum d,
                    S.SigSum s (v :> Nil),
                    Fractional d,
                    Ord d,
                    SV.Walker v,
                    SV.Storage v Sign,
                    SV.Singleton v,
                    S.TailType s)=>
                   TC s typ (Data (v :> Nil) d)->
                   TC s typ (Data (v :> Nil) d)->
                   Quality
edgeFlowQuality s1 s2 = if isConsistant
                              then (if isClean then Clean else Dirty)
                              else Wrong
  where
    isConsistant = sigSign (sigSum s1) == sigSign (sigSum s2)
    isClean = not (S.hasSignChange s1) || (not $  S.hasSignChange s2)


adjustSignsNew :: (SV.Walker v,
                   SV.Storage v a,
                   DArith0 a,
                   Ord node,
                   Show node) =>
                  EdgeStates node ->
                  FlowRecord node v a ->
                  FlowRecord node v a
adjustSignsNew (EdgeStates m) rec = rmapWithKey f rec
  where f key x = case checkedLookup2 "Flow.adjustSignsNew" m (g key) of
          (Neg, _) -> neg x
          (Pos, _) -> x
          (Zero, _) -> x
        g (PPosIdx n1 n2) = G.Edge n1 n2



adjustSigns ::
  (Show (v a), DArith0 a,
  SV.Walker v, SV.Storage v a, Ord node, Show node) =>
  Topology node ->
  FlowState node -> FlowRecord node v a -> FlowRecord node v a
adjustSigns topo (FlowState state) (Record dt flow) =
   Record dt (M.foldrWithKey g M.empty uniquePPos)
      where g ppos NSign acc =
              M.insert ppos (neg (flow `checkedLookup` ppos))
                $ M.insert ppos' (neg (flow `checkedLookup` ppos')) acc
                where ppos' = flipPos ppos
            g ppos _ acc =
              M.insert ppos (flow `checkedLookup` ppos)
                $ M.insert ppos' (flow `checkedLookup` ppos') acc
                where ppos' = flipPos ppos
            uniquePPos = foldl h M.empty (labEdges topo)
              where h acc (Edge idx1 idx2, ()) =
                      M.insert ppos (state `checkedLookup` ppos) acc
                      where ppos = PPosIdx idx1 idx2


-- | Function to calculate flow states for the whole sequence
genSequFState ::
  (SV.Walker v, SV.Storage v a, BSum a, Fractional a, Ord a) =>
  SequData (FlowRecord node v a) -> SequData (FlowState node)
genSequFState sqFRec = fmap genFlowState sqFRec

-- | Function to extract the flow state out of a Flow Record
genFlowState ::
  (SV.Walker v, SV.Storage v a, BSum a, Fractional a, Ord a) =>
  FlowRecord node v a -> FlowState node
genFlowState (Record _time flowMap) =
   FlowState $ M.map (fromScalar . sigSign . sigSum) flowMap

-- | Function to generate Flow Topologies for all Sections
genSequFlowTops ::
  (Ord node, Show node) =>
  Topology node -> SequData (FlowState node) -> SequData (FlowTopology node)
genSequFlowTops topo = fmap (genFlowTopology topo)

-- | Function to generate Flow Topology -- only use one state per signal
genFlowTopology ::
  (Ord node, Show node) =>
  Topology node -> FlowState node -> FlowTopology node
genFlowTopology topo (FlowState fs) =
   mkGraph (labNodes topo) $
   map
      (\(Edge idx1 idx2, ()) ->
         case fs `checkedLookup` (PPosIdx idx1 idx2) of
            PSign -> (Edge idx1 idx2, Dir)
            NSign -> (Edge idx2 idx1, Dir)
            ZSign -> (Edge idx1 idx2, UnDir)) $
   labEdges topo


mkSectionTopology ::
  (Ord node) =>
  Idx.Section -> ClassifiedTopology node -> SequFlowGraph node
mkSectionTopology sid = Gr.ixmap (Idx.afterSecNode sid)


mkStorageEdges ::
   node -> M.Map Idx.Section Topo.StoreDir ->
   [Topo.LEdge node]
mkStorageEdges node stores = do
   let (ins, outs) =
          M.partition (Topo.In ==) $ M.mapKeys Idx.AfterSection stores
   secin <- Idx.initial : M.keys ins
   secout <- M.keys $ snd $ M.split secin outs
   return $
      (Edge (Idx.BndNode secin node) (Idx.BndNode secout node), Dir)

getActiveStoreSequences ::
   (Ord node) =>
   SequData (Topo.ClassifiedTopology node) ->
   M.Map node (M.Map Idx.Section Topo.StoreDir)
getActiveStoreSequences sq =
   Fold.foldl
      (M.unionWith (M.unionWith (error "duplicate section for node")))
      M.empty $
   SD.mapWithSection
      (\s g ->
          fmap (M.singleton s) $
          M.mapMaybe (join . Topo.maybeStorage) $ Gr.nodeLabels g) sq


type RangeGraph node = (M.Map Idx.Section SD.Range, SequFlowGraph node)

mkSequenceTopology ::
   (Ord node) =>
   SequData (FlowTopology node) ->
   RangeGraph node
mkSequenceTopology sd =
   (,) (Fold.fold $ SD.mapWithSectionRange (\s rng _ -> M.singleton s rng) sq) $
   insEdges (Fold.fold $ M.mapWithKey mkStorageEdges tracks) $
   insNodes
      (map (\n -> (Idx.initBndNode n, Topo.Storage (Just Topo.In))) $
       M.keys tracks) $
   Fold.fold $
   SD.mapWithSection mkSectionTopology sq
  where tracks = getActiveStoreSequences sq
        sq = fmap Topo.classifyStorages sd
