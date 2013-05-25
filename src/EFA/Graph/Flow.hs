{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module EFA.Graph.Flow where

import qualified EFA.Example.Index as XIdx

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr
import EFA.Graph.Topology
          (Topology, FlowTopology, ClassifiedTopology, SequFlowGraph)
import EFA.Graph (DirEdge(DirEdge), labNodes, insNodes)

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

import qualified Data.Foldable as Fold
import qualified Data.Map as M
import qualified Data.Set as Set

import Control.Monad (join)
import Data.Bool.HT (if')

import qualified EFA.Utility.Map as MapU
import EFA.Utility.Map (checkedLookup,checkedLookupOld)



data Quality = Clean | Dirty | Wrong deriving (Show,Eq)
data Dir = Pos | Neg | Zero deriving (Show,Eq)

type EdgeFlow = (Dir,Quality)

newtype EdgeStates node = EdgeStates (M.Map (DirEdge node ) EdgeFlow) deriving (Show)

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
adjustSignsNew (EdgeStates m) rec = rmapWithKey f rec
  where f key x = case checkedLookup "Flow.adjustSignsNew" m (g key) of
          (Neg, _) -> neg x
          (Pos, _) -> x
          (Zero, _) -> x
        g (Idx.PPos (Idx.StructureEdge n1 n2)) = DirEdge n1 n2



adjustSigns ::
  (Show (v a), DArith0 a,
  SV.Walker v, SV.Storage v a, Ord node, Show node) =>
  Topology node ->
  FlowState node -> FlowRecord node v a -> FlowRecord node v a
adjustSigns topo (FlowState state) (Record dt flow) =
   Record dt (M.foldrWithKey g M.empty uniquePPos)
      where (!) = checkedLookup "Flow.adjustSigns"
            g ppos NSign acc =
              M.insert ppos (neg (flow ! ppos))
                $ M.insert ppos' (neg (flow ! ppos')) acc
                where ppos' = Idx.flip ppos
            g ppos _ acc =
              M.insert ppos (flow ! ppos)
                $ M.insert ppos' (flow ! ppos') acc
                where ppos' = Idx.flip ppos
            uniquePPos = foldl h M.empty (Gr.edges topo)
              where h acc (DirEdge idx1 idx2) =
                      M.insert ppos (state `checkedLookupOld` ppos) acc
                      where ppos = XIdx.ppos idx1 idx2


adjustSignsIgnoreUnknownPPos ::
  (Show (v a), DArith0 a,
  SV.Walker v, SV.Storage v a, Ord node, Show node) =>
  Topology node ->
  FlowState node -> FlowRecord node v a -> FlowRecord node v a
adjustSignsIgnoreUnknownPPos topo (FlowState state) (Record dt flow) =
   Record dt (M.foldrWithKey g M.empty uniquePPos)
      where f m lkup ppos acc =
              maybe acc (flip (M.insert ppos) acc) (lkup ppos m)
            g ppos si acc =
              foldr (f flow $ (modify .) . M.lookup) acc [ppos, Idx.flip ppos]
                where modify = case si of { NSign -> fmap neg; _ -> id }
            uniquePPos = foldr h M.empty (Gr.edges topo)
              where h (DirEdge idx1 idx2) =
                      f state M.lookup (XIdx.ppos idx1 idx2)


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
   FlowState $ M.map (fromScalar . sigSign . S.sum) flowMap

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
              M.lookup (XIdx.ppos idx1 idx2) fs) $
   Gr.edges topo



mkSectionTopology ::
  (Ord node) =>
  Idx.Section -> ClassifiedTopology node -> SequFlowGraph node
mkSectionTopology sec =
   Gr.ixmap
      (Idx.afterSecNode sec)
      (Topo.FlowEdge . Topo.StructureEdge . Idx.InSection sec)


mkStorageEdges ::
   node -> M.Map Idx.Section Topo.StoreDir ->
   [Topo.FlowEdge Gr.EitherEdge (Idx.BndNode node)]
mkStorageEdges node stores = do
   let (ins, outs) =
          M.partition (Topo.In ==) $ M.mapKeys Idx.AfterSection stores
   secin <- Idx.initial : M.keys ins
   secout <- M.keys $ snd $ M.split secin outs
   return $
      (Topo.FlowEdge $ Topo.StorageEdge $
       Idx.ForNode (Idx.StorageEdge secin secout) node)

-- Kann man diese Funktion und die folgende (getStorages) vereinigen?
getActiveStoreSequences ::
   (Ord node, Show node) =>
   SequData (Topo.ClassifiedTopology node) ->
   M.Map node (M.Map Idx.Section Topo.StoreDir)
getActiveStoreSequences =
   Fold.foldl
      (M.unionWith (M.unionWith (error "duplicate section for node")))
      M.empty
   .
   SD.mapWithSection
      (\s g ->
         fmap (M.singleton s) $
         M.mapMaybe (join . Topo.maybeStorage) $ Gr.nodeLabels g)

getStorages ::
  (Ord node, Show node) =>
  SequData (Topo.ClassifiedTopology node) -> [node]
getStorages sd = Set.toList $ Set.fromList $ concatMap h d
  where SD.SequData d = SD.mapWithSection f sd
        f _ g = M.keys $ M.filter Topo.isStorage $ Gr.nodeLabels g
        h (SD.Section _ _ ns) = ns

type RangeGraph node = (M.Map Idx.Section SD.Range, SequFlowGraph node)

insEdges ::
   Ord node =>
   [Topo.FlowEdge Gr.EitherEdge (Idx.BndNode node)] ->
   SequFlowGraph node ->
   SequFlowGraph node
insEdges = Gr.insEdges . map (flip (,) ())

-- Alle Storages sollen in die initiale Sektion,
-- auch wenn sie nie aktive sind!
-- Damit man beim initialisieren auch Werte zuweisen kann.
mkSequenceTopology ::
   (Ord node, Show node) =>
   SequData (FlowTopology node) ->
   RangeGraph node
mkSequenceTopology sd =
   (,) (Fold.fold $ SD.mapWithSectionRange (\s rng _ -> M.singleton s rng) sq) $
   insEdges (Fold.fold $ M.mapWithKey mkStorageEdges tracks) $
   insNodes
      (map (\n -> (Idx.initBndNode n, Topo.Storage (Just Topo.In))) sts) $
   Fold.fold $
   SD.mapWithSection mkSectionTopology sq
  where sq = fmap Topo.classifyStorages sd
        tracks = getActiveStoreSequences sq
        sts = getStorages sq
