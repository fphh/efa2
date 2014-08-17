module EFA.Action.Flow.Topology.State where

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Report.Format as Format
import qualified EFA.Flow.State as State
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.Part.Index as PartIdx
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Equation.Arithmetic as Arith
import qualified Data.Map as Map
import qualified Data.List as List

{-
data Orientation = Dir | UnDir deriving Show

absoluteStateIndex ::
  (Node.C node) =>
  Graph.Graph node Graph.DirEdge nodeLabel1 a1 ->
  Graph.Graph node Graph.EitherEdge nodeLabel a ->
  Idx.State
absoluteStateIndex topo flowTopo =
  let tlabels = map unEitherEDir $ Map.keys $ Graph.edgeLabels topo

      flabels = Map.fromList $ map unEDir $ Map.keys $ Graph.edgeLabels flowTopo

      unEDir (Graph.EDirEdge (Graph.DirEdge f t)) = ((f, t), Dir)
      unEDir (Graph.EUnDirEdge (Graph.UnDirEdge f t)) = ((f, t), UnDir)

      unEitherEDir (Graph.DirEdge f t) = (f, t)

      g k@(f, t) =
        case (Map.lookup k flabels, Map.lookup (t, f) flabels) of
             (Just Dir, _) -> 0
             (Just UnDir, _) -> 1
             (_, Just Dir) -> 2
             (_, Just UnDir) -> 1
             _ -> error $ "EFA.Graph.Topology.flowNumber: edge not found "
                          ++ Format.showRaw (Node.display f :: Format.ASCII)
                          ++ "->"
                          ++ Format.showRaw (Node.display t :: Format.ASCII)

      toTernary xs = Idx.State $ sum $ zipWith (*) xs $ map (3^) [0 :: Int ..]

  in toTernary $ map g tlabels


-- PG: Code from HH calculate flow state from flow direction against topology 
-- Is that the right place for this code ??
-}
data Orientation = Dir | UnDir deriving Show

getAbsoluteStateIndex ::
  (Node.C node, Show node) =>
  Graph.Graph node Graph.DirEdge nodeLabel1 a1 ->
  Graph.Graph node Graph.EitherEdge nodeLabel a ->
  PartIdx.AbsoluteState
getAbsoluteStateIndex topo flowTopo =
  let tlabels = map unEitherEDir $ Map.keys $ Graph.edgeLabels topo

      flabels = Map.fromList $ map unEDir $ Map.keys $ Graph.edgeLabels flowTopo

      unEDir (Graph.EDirEdge (Graph.DirEdge f t)) = ((f, t), Dir)
      unEDir (Graph.EUnDirEdge (Graph.UnDirEdge f t)) = ((f, t), UnDir)

      unEitherEDir (Graph.DirEdge f t) = (f, t)

      -- lookup a topology as (node1,node2) and reverse (node2,node1) and check flow
      g k@(f, t) =
        case (Map.lookup k flabels, Map.lookup (t, f) flabels) of
             (Just Dir, _) -> 0
             (Just UnDir, _) -> 1
             (_, Just Dir) -> 2
             (_, Just UnDir) -> 1
             _ -> error $ "EFA.Graph.Topology.flowNumber - edge not found: " 
                  ++ show f  ++ "->" ++ show t
      
      -- nodes are Ord, topology Edges are always sorted same, single edge states are
      -- converted generating a ternary number       
      toTernary xs = PartIdx.AbsoluteState $ sum $ zipWith (*) xs $ map (3^) [0 :: Int ..]

  in toTernary $ map g tlabels

-- TODO :: Introduce absolute State as Type Variable
absoluteStateFlowGraph ::
  (Show node, Node.C node) =>
  Graph.Graph node Graph.DirEdge nodeLabel1 a1->
  State.Graph node Graph.EitherEdge stateLabel nodeLabel storageLabel flowLabel carryLabel ->
  State.Graph node Graph.EitherEdge stateLabel nodeLabel storageLabel flowLabel carryLabel
absoluteStateFlowGraph topo sfg = sfg {StateQty.states =  states,  
                                       StateQty.storages = storages}
  where                                          
    stateMap = Map.fromList $ 
               map (\(oldState,x) -> (oldState, PartIdx.State $ PartIdx.unAbsoluteState $ 
                                                              getAbsoluteStateIndex topo $ FlowTopo.topology x)) $
           Map.toList $ StateQty.states sfg
    states = Map.fromList $ map (\(oldState,x) -> (stateMap Map.! oldState,x)) $
           Map.toList $ StateQty.states sfg
           
    storages = Map.map f $ StateQty.storages sfg
    
    f stoGraph = stoGraph{Storage.edges= Map.mapKeys g $ Storage.edges stoGraph}
    
    g (StorageIdx.Edge initSec exitSec) = StorageIdx.Edge (h1 initSec) (h2 exitSec)  
      
    h1 PartIdx.Init = PartIdx.Init  
    h1 (PartIdx.NoInit oldState) = (PartIdx.NoInit $ stateMap Map.! oldState)

    h2 PartIdx.Exit = PartIdx.Exit  
    h2 (PartIdx.NoExit oldState) = (PartIdx.NoExit $ stateMap Map.! oldState)


data EdgeConditions node = NoCond | EdgeOrs [EdgeAnds node]
newtype EdgeAnds node = EdgeAnds [EdgeCon node]
data EdgeCon node = Demand (Graph.EitherEdge node) | 
                    Avoid (Graph.EitherEdge node) |                          
                    NoInactive

stateAnalysisAbsolute ::
  (Show node, Node.C node)=>
  Topo.Topology node -> 
  [(Idx.AbsoluteState,Topo.FlowTopology node)]   
stateAnalysisAbsolute topo =  
  List.sortBy (\x y -> compare (fst x) (fst y)) $ map (\x -> (getAbsoluteStateIndex topo x,x)) 
  $ StateAnalysis.advanced topo

filterFlowStates :: (Ord node) =>
  EdgeConditions node ->
  [(Idx.AbsoluteState,Topo.FlowTopology node)] ->
  [(Idx.AbsoluteState,Topo.FlowTopology node)]
filterFlowStates  NoCond flowTopos =  flowTopos
filterFlowStates (EdgeOrs conds) flowTopos =  List.sortBy (\x y -> compare (fst x) (fst y)) $
  concatMap (\x -> filter (edgeCondition x . Graph.edges . snd) flowTopos) conds

edgeCondition :: (Eq node) => EdgeAnds node -> [Graph.EitherEdge node] -> Bool
edgeCondition (EdgeAnds xs) edges = and $ map f xs
  where
        f (Demand e) = any (e==) edges 
        f (Avoid e) = not $ any (e==) edges 
        f NoInactive = not $ any (==True) $ map isInactive edges 
                 where isInactive (Graph.EUnDirEdge _ ) = True
                       isInactive _ = False

addEdgeCondition ::  
  EdgeConditions node ->
  EdgeAnds node ->
  EdgeConditions node
addEdgeCondition NoCond x = EdgeOrs [x]  
addEdgeCondition (EdgeOrs xs) x = EdgeOrs $ xs ++ [x]

-- type FlowTopology node = Graph node Graph.EitherEdge () ()
{-
data
   EitherEdge node =
        EDirEdge (DirEdge node)
      | EUnDirEdge (UnDirEdge node)
   deriving (Eq, Ord, Show)

data DirEdge node = DirEdge node node
   deriving (Eq, Ord, Show)

data UnDirEdge node = UnDirEdge node node
   deriving (Eq, Ord, Show)



-}

{-
edgeCondition :: FlowTopology node -> Bool                           
edgeCondition edges 

stateCondition :: [
-}