{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module EFA.Graph.Flow where

import qualified EFA.Application.Index as XIdx

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr
import EFA.Graph.Topology
          (Topology, FlowTopology, ClassifiedTopology, SequFlowGraph)
import EFA.Graph (DirEdge(DirEdge), labNodes)

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.SequenceData (SequData)
import EFA.Signal.Record
          (Record(Record), FlowState(FlowState), FlowRecord,
           getSig, rmapWithKey)
import EFA.Signal.Signal (fromScalar, sigSign, neg, TC(..))
import EFA.Signal.Data(Data(..), Nil, (:>))
import EFA.Signal.Base (Sign(PSign, NSign, ZSign),BSum, DArith0)

import qualified EFA.Utility.Map as MapU
import EFA.Utility.Map (checkedLookup)

import qualified Data.Foldable as Fold
import qualified Data.Map as Map

import Data.Map (Map)
import Data.Bool.HT (if')



data Quality = Clean | Dirty | Wrong deriving (Show,Eq)
data Dir = Pos | Neg | Zero deriving (Show,Eq)

type EdgeFlow = (Dir,Quality)

newtype EdgeStates node = EdgeStates (Map (DirEdge node ) EdgeFlow) deriving (Show)

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
getEdgeState topo rec = EdgeStates $ MapU.fromSet f $ Gr.edgeSet topo
  where
    f (DirEdge n1 n2) =
          case sigSign $ S.sum s1 of
             TC (Data s) -> (convertSign s, edgeFlowQuality s1 s2)
       where s1 = getSig rec (XIdx.ppos n1 n2)
             s2 = getSig rec (XIdx.ppos n2 n1)
             convertSign s =
                case s of
                   PSign -> Pos
                   NSign -> Neg
                   ZSign -> Zero

edgeFlowQuality :: (Num d,
                    SV.Storage v d,
                    BSum d,
                    Fractional d,
                    Ord d,
                    SV.Walker v,
                    SV.Storage v Sign,
                    SV.Singleton v,
                    S.FoldType s,
                    S.TailType s)=>
                   TC s typ (Data (v :> Nil) d)->
                   TC s typ (Data (v :> Nil) d)->
                   Quality
edgeFlowQuality s1 s2 =
   if' (sigSign (S.sum s1) /= sigSign (S.sum s2)) Wrong $
   if' (S.hasSignChange s1 && S.hasSignChange s2) Dirty $
   Clean


adjustSignsNew :: (SV.Walker v,
                   SV.Storage v a,
                   DArith0 a,
                   Ord node,
                   Show node) =>
                  EdgeStates node ->
                  FlowRecord node v a ->
                  FlowRecord node v a
adjustSignsNew (EdgeStates m) = rmapWithKey f
  where f (Idx.PPos (Idx.StructureEdge n1 n2)) x =
           case checkedLookup "Flow.adjustSignsNew" m $ DirEdge n1 n2 of
              (Neg, _) -> neg x
              (Pos, _) -> x
              (Zero, _) -> x



adjustSigns ::
  (Show (v a), DArith0 a,
  SV.Walker v, SV.Storage v a, Ord node, Show node) =>
  Topology node ->
  FlowState node -> FlowRecord node v a -> FlowRecord node v a
adjustSigns topo (FlowState state) (Record dt flow) =
   Record dt (Map.foldrWithKey g Map.empty uniquePPos)
      where m!k = checkedLookup "EFA.Graph.Flow.adjustSigns" m k
            g ppos NSign acc =
              Map.insert ppos (neg (flow ! ppos))
                $ Map.insert ppos' (neg (flow ! ppos')) acc
                where ppos' = Idx.flip ppos
            g ppos _ acc =
              Map.insert ppos (flow ! ppos)
                $ Map.insert ppos' (flow ! ppos') acc
                where ppos' = Idx.flip ppos
            uniquePPos = foldl h Map.empty (Gr.edges topo)
              where h acc (DirEdge idx1 idx2) =
                      Map.insert ppos (state ! ppos) acc
                      where ppos = XIdx.ppos idx1 idx2

adjustSignsIgnoreUnknownPPos ::
  (Show (v a), DArith0 a,
  SV.Walker v, SV.Storage v a, Ord node, Show node) =>
  Topology node ->
  FlowState node -> FlowRecord node v a -> FlowRecord node v a
adjustSignsIgnoreUnknownPPos topo (FlowState state) (Record dt flow) =
   Record dt (Map.foldrWithKey g Map.empty uniquePPos)
      where f m lkup ppos acc =
              maybe acc (flip (Map.insert ppos) acc) (lkup ppos m)
            g ppos si acc =
              foldr (f flow $ (modify .) . Map.lookup) acc [ppos, Idx.flip ppos]
                where modify = case si of { NSign -> fmap neg; _ -> id }
            uniquePPos = foldr h Map.empty (Gr.edges topo)
              where h (DirEdge idx1 idx2) =
                      f state Map.lookup (XIdx.ppos idx1 idx2)


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
   FlowState $ Map.map (fromScalar . sigSign . S.sum) flowMap

-- | Function to generate Flow Topologies for all Sections
genSequFlowTops ::
  (Ord node, Show node) =>
  Topology node -> SequData (FlowState node) -> SequData (FlowTopology node)
genSequFlowTops topo = fmap (genFlowTopology topo)

genSequFlowTopsIgnoreUnknownPPos ::
  (Ord node, Show node) =>
  Topology node -> SequData (FlowState node) -> SequData (FlowTopology node)
genSequFlowTopsIgnoreUnknownPPos topo =
  fmap (genFlowTopologyIgnoreUnknownPPos topo)



-- | Function to generate Flow Topology -- only use one state per signal
genFlowTopology ::
  (Ord node, Show node) =>
  Topology node -> FlowState node -> FlowTopology node
genFlowTopology topo (FlowState fs) =
   Gr.fromList (labNodes topo) $ map (flip (,) ()) $
   map
      (\(DirEdge idx1 idx2) ->
         case fs ! XIdx.ppos idx1 idx2 of
            PSign -> Gr.EDirEdge $ DirEdge idx1 idx2
            NSign -> Gr.EDirEdge $ DirEdge idx2 idx1
            ZSign -> Gr.EUnDirEdge $ Gr.UnDirEdge idx1 idx2) $
   Gr.edges topo
   where (!) = checkedLookup "Flow.genFlowTopology"

genFlowTopologyIgnoreUnknownPPos ::
  (Ord node, Show node) =>
  Topology node -> FlowState node -> FlowTopology node
genFlowTopologyIgnoreUnknownPPos topo (FlowState fs) =
   Gr.fromList (labNodes topo) $ map (flip (,) ()) $
   map
      (\(DirEdge idx1 idx2) ->
        let deflt = Gr.EUnDirEdge $ Gr.UnDirEdge idx1 idx2
        in  maybe deflt
              (\si -> case si of
                           PSign -> Gr.EDirEdge $ DirEdge idx1 idx2
                           NSign -> Gr.EDirEdge $ DirEdge idx2 idx1
                           ZSign -> deflt) $
              Map.lookup (XIdx.ppos idx1 idx2) fs) $
   Gr.edges topo



sectionFromClassTopo ::
  (Ord node) =>
  Idx.Section -> ClassifiedTopology node -> SequFlowGraph node
sectionFromClassTopo sec =
   Gr.ixmap
      (Idx.PartNode (Idx.augment sec))
      (Topo.FlowEdge . Topo.StructureEdge . Idx.InPart sec)


storageEdges ::
   Map Idx.Section Topo.StoreDir -> [Idx.StorageEdge Idx.Section node]
storageEdges stores = do
   let (ins, outs) = Map.partition (Topo.In ==) stores
   secin <- Idx.Init : map Idx.NoInit (Map.keys ins)
   secout <-
      (++[Idx.Exit]) $ map Idx.NoExit $ Map.keys $
      case secin of
         Idx.Init -> outs
         Idx.NoInit s -> snd $ Map.split s outs
   return $ Idx.StorageEdge secin secout

getStorageSequences ::
   (Ord node) =>
   SequData (Topo.ClassifiedTopology node) ->
   Map node (Map Idx.Section (Maybe Topo.StoreDir))
getStorageSequences =
   Map.unionsWith (Map.unionWith (error "duplicate section for node"))
   .
   Fold.toList
   .
   SD.mapWithSection
      (\s g ->
         fmap (Map.singleton s) $
         Map.mapMaybe Topo.maybeStorage $ Gr.nodeLabels g)

type RangeGraph node = (Map Idx.Section SD.Range, SequFlowGraph node)

type FlowEdge = Topo.FlowEdge Gr.EitherEdge
type AugNode sec = Idx.PartNode (Idx.Augmented sec)

insEdges ::
   (Ord sec, Ord node, Ord (FlowEdge (AugNode sec node))) =>
   Map node [Idx.StorageEdge sec node] ->
   Topo.FlowGraph sec node ->
   Topo.FlowGraph sec node
insEdges =
   Gr.insEdges .
   map (flip (,) () . Topo.FlowEdge . Topo.StorageEdge) . Fold.fold .
   Map.mapWithKey (map . flip Idx.ForNode)

insNodes ::
   (Ord sec, Ord node) =>
   [node] ->
   Topo.FlowGraph sec node ->
   Topo.FlowGraph sec node
insNodes storages =
   Gr.insNodes $
      concatMap
         (\n ->
            [(Idx.initAugNode n, Topo.Storage $ Just Topo.In),
             (Idx.exitAugNode n, Topo.Storage $ Just Topo.Out)])
         storages

{-
Alle Storages sollen in die initiale Sektion,
auch wenn sie nie aktiv sind!
So kann man beim Initialisieren auch Werte zuweisen.
-}
sequenceGraph ::
   (Ord node) =>
   SequData (FlowTopology node) ->
   RangeGraph node
sequenceGraph sd =
   (,) (Fold.fold $ SD.mapWithSectionRange (\s rng _ -> Map.singleton s rng) sq) $
   insEdges
      (fmap storageEdges $
       -- Map.filter (not . Map.null) $   -- required?
       fmap (Map.mapMaybe id) tracks) $
   insNodes (Map.keys tracks) $
   Fold.fold $ SD.mapWithSection sectionFromClassTopo sq
  where sq = fmap Topo.classifyStorages sd
        tracks = getStorageSequences sq
