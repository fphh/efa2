module EFA.Graph.Draw (
  sequFlowGraph,
  sequFlowGraphWithEnv,
  sequFlowGraphAbsWithEnv, envAbs,
  sequFlowGraphDeltaWithEnv, envDelta,
  Env(..),
  topology,
  flowTopologies,
  ) where

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Format (Format, Unicode(Unicode, unUnicode))

import qualified EFA.Equation.Env as Interp
import EFA.Equation.Variable (MkIdxC, mkIdx)
import EFA.Equation.Env (StorageMap)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr
import EFA.Graph.Topology
          (SequFlowGraph,
           NodeType(Storage),
           EdgeType(OriginalEdge, IntersectionEdge),
           getFlowDirection,
           FlowDirectionField, FlowTopology)
import EFA.Graph (Edge(Edge), labNodes, labEdges)

-- import EFA.Graph.Topology.Node (ShowNode, showNode)
import qualified EFA.Graph.Topology.Node as Node

import Data.GraphViz (
          runGraphvizCanvas,
          GraphvizCanvas(Xlib),
          runGraphvizCommand,
          GraphvizOutput(XDot),
          GraphID(Int),
          GlobalAttributes(GraphAttrs),
          GraphvizCommand(Dot),
          DotEdge(DotEdge),
          DotGraph(DotGraph),
          DotNode(DotNode),
          DotSubGraph(DotSG),
          DotStatements(DotStmts),
          DotSubGraph,
          attrStmts, nodeStmts, edgeStmts, graphStatements,
          directedGraph, strictGraph, subGraphs,
          graphID)
import Data.GraphViz.Attributes.Complete as Viz

import qualified Data.Accessor.Basic as Accessor

import Data.Tuple.HT (mapFst)
import Data.Maybe.HT (toMaybe)

import qualified Data.Text.Lazy as T

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.HT as HTL

import Control.Monad (void)


nodeColour :: Attribute
nodeColour = FillColor [RGB 230 230 240]

originalEdgeColour :: Attribute
originalEdgeColour = Color [RGB 0 0 200]

intersectionEdgeColour :: Attribute
intersectionEdgeColour = Color [RGB 200 0 0]


dotFromSequFlowGraph ::
  (Node.C node) =>
  SequFlowGraph node ->
  Maybe (Idx.Section -> Unicode) ->
  (Topo.LNode node -> Unicode) ->
  (Topo.LEdge node -> [Unicode]) ->
  DotGraph T.Text
dotFromSequFlowGraph g mtshow nshow eshow =
  DotGraph { strictGraph = False,
             directedGraph = True,
             graphID = Just (Int 1),
             graphStatements = stmts }

  where (topoEs, interEs) =
           mapFst (M.fromListWith (++)) $
           HTL.partitionMaybe
              (\e@(Edge (Idx.SecNode sx _) (Idx.SecNode sy _), _) ->
                 toMaybe (sx == sy) (sx, [e])) $
           Gr.labEdges g

        topoNs =
           M.fromListWith (++) $
           map (\nl@(Idx.SecNode s _, _) -> (s, [nl])) $
           Gr.labNodes g

        sg sl ns es =
            DotSG True (Just (Int $ fromEnum sl)) $
            DotStmts
               [GraphAttrs [Label (StrLabel (T.pack str))]]
               []
               (map (dotFromSecNode nshow) ns)
               (map (dotFromSecEdge eshow) es)
          where str =
                   show sl ++ " / " ++
                   case mtshow of
                      Nothing -> ""
                      Just tshow -> "Time " ++ unUnicode (tshow sl)
        stmts =
          DotStmts {
            attrStmts = [],
            subGraphs =
              M.elems $
              M.intersectionWithKey sg topoNs
                 (M.union topoEs (fmap (const []) topoNs)),
            nodeStmts = [],
            edgeStmts = map (dotFromSecEdge eshow) interEs
          }


dotFromSecNode :: 
  (Node.C node) =>
  (Topo.LNode node -> Unicode) -> Topo.LNode node -> DotNode T.Text
dotFromSecNode nshow n@(x, nodeType) =
   DotNode (dotIdentFromSecNode x)
      [ displabel, nodeColour, 
        Style [SItem Filled []], Shape (shape nodeType), color nodeType ]
  where displabel = Label $ StrLabel $ T.pack $ unUnicode $ nshow n
        shape Topo.Crossing = PlainText
        shape Topo.Source = DiamondShape
        shape Topo.Sink = BoxShape
        shape Topo.Storage = Ellipse
        shape _ = BoxShape
        color Topo.Crossing = FillColor [RGB 150 200 240]
        color _ = nodeColour

dotFromSecEdge ::
  (Node.C node) =>
  (Topo.LEdge node -> [Unicode]) -> Topo.LEdge node -> DotEdge T.Text
dotFromSecEdge eshow e =
   DotEdge
      (dotIdentFromSecNode x) (dotIdentFromSecNode y)
      [displabel, Viz.Dir dir, colour, constraint]
  where (Edge x y, dir, order) = orientEdge e
        displabel =
           Label $ StrLabel $ T.pack $
           L.intercalate "\n" $ map unUnicode $ order $ eshow e
        colour =
           case Topo.getEdgeType e of
              IntersectionEdge -> intersectionEdgeColour
              OriginalEdge -> originalEdgeColour
        constraint =
           Constraint $
           case Topo.getEdgeType e of
              IntersectionEdge -> False
              OriginalEdge -> True


dotIdentFromSecNode :: (Node.C node) => Idx.SecNode node -> T.Text
dotIdentFromSecNode (Idx.SecNode (Idx.Section s) n) =
   T.pack $ "s" ++ show s ++ "n" ++ Node.dotId n

dotIdentFromNode :: (Node.C node) => node -> T.Text
dotIdentFromNode n = T.pack $ Node.dotId n


printGraph, printGraphX, _printGraphDot ::
  (Node.C node) =>
   SequFlowGraph node ->
   Maybe (Idx.Section -> Unicode) ->
   (Topo.LNode node -> Unicode) ->
   (Topo.LEdge node -> [Unicode]) ->
   IO ()
printGraph = printGraphX

printGraphX g recTShow nshow eshow =
   runGraphvizCanvas Dot (dotFromSequFlowGraph g recTShow nshow eshow) Xlib

_printGraphDot g recTShow nshow eshow =
   void $
   runGraphvizCommand Dot
      (dotFromSequFlowGraph g recTShow nshow eshow)
      XDot "result/graph.dot"


sequFlowGraph ::
  (Node.C node) =>
  SequFlowGraph node -> IO ()
sequFlowGraph topo =
   printGraph topo Nothing nshow eshow
  where nshow (Idx.SecNode _ n, l) =
           Unicode $ unUnicode (Node.display n) ++ " - " ++ showType l
        eshow _ = []


dotFromTopology ::
  (Node.C node) =>
  Topo.Topology node -> DotGraph T.Text
dotFromTopology g =
  DotGraph {
    strictGraph = False,
    directedGraph = False,
    graphID = Just (Int 1),
    graphStatements =
      DotStmts {
        attrStmts = [],
        subGraphs = [],
        nodeStmts = map dotFromTopoNode $ Gr.labNodes g,
        edgeStmts = map (dotFromTopoEdge . fst) $ Gr.labEdges g
      }
  }

dotFromTopoNode ::
  (Node.C node) =>
  Gr.LNode node Topo.NodeType -> DotNode T.Text
dotFromTopoNode (x, typ) =
   DotNode (dotIdentFromNode x)
      [Label $ StrLabel $ T.pack $
          unUnicode (Node.display x) ++ "\n" ++ showType typ,
       nodeColour, Style [SItem Filled []], Shape BoxShape]

dotFromTopoEdge ::
  (Node.C node) =>
  Gr.Edge node -> DotEdge T.Text
dotFromTopoEdge e =
   case orientUndirEdge e of
      Edge x y ->
         DotEdge
            (dotIdentFromNode x) (dotIdentFromNode y)
            [Viz.Dir Viz.NoDir, originalEdgeColour]



topology :: (Node.C node) => Topo.Topology node -> IO ()
topology topo =
   runGraphvizCanvas Dot (dotFromTopology topo) Xlib


dotFromFlowTopology ::
  (Node.C node) =>
  Int -> FlowTopology node -> DotSubGraph String
dotFromFlowTopology ident topo = DotSG True (Just (Int ident)) stmts
  where stmts = DotStmts attrs [] ns es
        attrs = [GraphAttrs [labelf ident]]
        ns = map mkNode (labNodes topo)
        idf x = show ident ++ "_" ++ Node.dotId x
        labelf x = Label $ StrLabel $ T.pack (show x)
        mkNode x@(n, _) = DotNode (idf n) (nattrs x)
        nattrs x = [labNodef x, nodeColour, Style [SItem Filled []], Shape BoxShape ]
        labNodef (n, l) =
           Label $ StrLabel $ T.pack $
              unUnicode (Node.display n) ++ " - " ++ showType l
        es = map mkEdge (labEdges topo)
        mkEdge el =
           case orientEdge el of
              (Edge x y, d, _) ->
                 DotEdge (idf x) (idf y) [Viz.Dir d]

flowTopologies ::
  (Node.C node) =>
  [FlowTopology node] -> IO ()
flowTopologies ts = runGraphvizCanvas Dot g Xlib
  where g = DotGraph False True Nothing stmts
        stmts = DotStmts attrs subgs [] []
        subgs = zipWith dotFromFlowTopology [0..] ts
        attrs = []


orientEdge ::
   (Ord n, FlowDirectionField el) =>
   (Edge n, el) -> (Edge n, DirType, [s] -> [s])
orientEdge (e@(Edge x y), l) =
   case getFlowDirection l of
      Topo.UnDir ->
         (orientUndirEdge e, NoDir, const [])
      Topo.Dir ->
--         if comparing (\(Idx.SecNode s n) -> n) x y == LT
         if x < y
           then (Edge x y, Forward, id)
           else (Edge y x, Back, reverse)

orientUndirEdge :: Ord n => Edge n -> Edge n
orientUndirEdge (Edge x y) =
   if x < y then Edge x y else Edge y x


showType :: NodeType -> String
showType = show

formatNodeType :: Format output => NodeType -> output
formatNodeType = Format.literal . showType

formatNodeStorage ::
   (Interp.Record recIdx rec, FormatValue a, Format output, Node.C node) =>
   recIdx -> StorageMap node (rec a) -> Topo.LNode node -> output
formatNodeStorage rec st (n@(Idx.SecNode _sec nid), ty) =
   Format.lines $
   Node.display nid :
   Format.words [formatNodeType ty] :
      case ty of
         Storage ->
            [Format.words
               [lookupFormat rec st $ Idx.Storage n]]
         _ -> []


{- |
The 'Env' shall contain only values and functions for display.
It shall not contain values needed for computations.
-}
data Env node output =
   Env {
      formatEnergy, formatMaxEnergy,
      formatX,
      formatEta    :: Idx.SecNode node -> Idx.SecNode node -> output,
      formatTime   :: Idx.Section -> output,
      formatNode   :: Topo.LNode node -> output
   }

lookupFormat ::
   (Ord (idx node), MkIdxC idx, Interp.Record recIdx rec,
    FormatValue a, Format output, Node.C node) =>
   recIdx -> M.Map (idx node) (rec a) -> idx node -> output
lookupFormat recIdx mp k =
   maybe
      (error $ "could not find index " ++
         (Format.unUnicode $ formatValue $ mkIdx k))
      (formatValue . Accessor.get (Interp.accessRecord recIdx)) $
   M.lookup k mp

lookupFormatAssign ::
   (Ord (idx node), Format.EdgeIdx (idx node), MkIdxC idx,
    Format.Record recIdx, Interp.Record recIdx rec,
    FormatValue a, Format output, Node.C node) =>
   recIdx ->
   M.Map (idx node) (rec a) ->
   (Idx.SecNode node -> Idx.SecNode node -> idx node) ->
   (Idx.SecNode node -> Idx.SecNode node -> output)
lookupFormatAssign rec mp makeIdx x y =
   case makeIdx x y of
      idx ->
         Format.assign
            (Format.record rec $ Format.edgeIdent $ Format.edgeVar idx)
            (lookupFormat rec mp idx)

sequFlowGraphWithEnv ::
  (Node.C node) =>
  SequFlowGraph node -> Env node Unicode -> IO ()
sequFlowGraphWithEnv g env =
   printGraph g (Just (formatTime env)) (formatNode env) eshow
  where eshow e@((Edge uid vid), _l) =
           (case Topo.getEdgeType e of
              OriginalEdge -> []
              IntersectionEdge ->
                 formatMaxEnergy env uid vid :
                 Format.empty :
                 [])
           ++
           formatEnergy env uid vid :
           formatX env uid vid :
           formatEta env uid vid :
           formatX env vid uid :
           formatEnergy env vid uid :
           []

sequFlowGraphAbsWithEnv ::
  (FormatValue a, Node.C node) =>
  SequFlowGraph node -> Interp.Env node (Interp.Absolute a) -> IO ()
sequFlowGraphAbsWithEnv topo = sequFlowGraphWithEnv topo . envAbs

sequFlowGraphDeltaWithEnv ::
  (FormatValue a, Node.C node) =>
  SequFlowGraph node -> Interp.Env node (Interp.Delta a) -> IO ()
sequFlowGraphDeltaWithEnv topo = sequFlowGraphWithEnv topo . envDelta


envGen ::
   (FormatValue a, Format output, Format.Record idx,
    Interp.Record idx rec, Node.C node) =>
   idx ->
   Interp.Env node (rec a) -> Env node output
envGen rec (Interp.Env e me _p n dt x _s st) =
   Env
      (lookupFormatAssign rec e Idx.Energy)
      (lookupFormatAssign rec me Idx.MaxEnergy)
      (lookupFormatAssign rec x Idx.X)
      (lookupFormatAssign rec n Idx.Eta)
      (lookupFormat rec dt . Idx.DTime)
      (formatNodeStorage rec st)

envAbs ::
   (FormatValue a, Format output, Node.C node) =>
   Interp.Env node (Interp.Absolute a) -> Env node output
envAbs = envGen Idx.Absolute

envDelta ::
   (FormatValue a, Format output, Node.C node) =>
   Interp.Env node (Interp.Delta a) -> Env node output
envDelta = envGen Idx.Delta
