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
import EFA.Equation.Env (StorageMap, SingleRecord(SingleRecord))

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
  (Node.Show nty, Ord nty) =>
  SequFlowGraph nty ->
  Maybe (Unicode, Idx.Section -> Unicode) ->
  (Topo.LNode nty -> Unicode) ->
  (Topo.LEdge nty -> [Unicode]) ->
  DotGraph T.Text
dotFromSequFlowGraph g recTShow nshow eshow =
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
                   case recTShow of
                      Nothing -> "NoRecord"
                      Just (Unicode n, timef) ->
                         n ++ " / Time " ++ unUnicode (timef sl)
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
  (Node.Show nty) =>
  (Topo.LNode nty -> Unicode) -> Topo.LNode nty -> DotNode T.Text
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
  (Node.Show nty, Ord nty) =>
  (Topo.LEdge nty -> [Unicode]) -> Topo.LEdge nty -> DotEdge T.Text
dotFromSecEdge eshow e =
   DotEdge
      (dotIdentFromSecNode x) (dotIdentFromSecNode y)
      [displabel, Viz.Dir dir, colour]
  where (Edge x y, dir, order) = orientEdge e
        displabel =
           Label $ StrLabel $ T.pack $
           L.intercalate "\n" $ map unUnicode $ order $ eshow e
        colour =
           case Topo.getEdgeType e of
              IntersectionEdge -> intersectionEdgeColour
              OriginalEdge -> originalEdgeColour
        --colour = originalEdgeColour

dotIdentFromSecNode ::
  (Node.Show nty) =>
  Idx.SecNode nty -> T.Text
dotIdentFromSecNode (Idx.SecNode (Idx.Section s) n) =
   T.pack $ "s" ++ show s ++ "n" ++ Node.show n

printGraph, printGraphX, _printGraphDot ::
  (Ord nty, Node.Show nty) =>
   SequFlowGraph nty ->
   Maybe (Unicode, Idx.Section -> Unicode) ->
   (Topo.LNode nty -> Unicode) ->
   (Topo.LEdge nty -> [Unicode]) ->
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
  (Ord nty, Node.Show nty) =>
  SequFlowGraph nty -> IO ()
sequFlowGraph topo =
   printGraph topo Nothing nshow eshow
  where nshow (Idx.SecNode _ n, l) = Unicode $ Node.show n ++ " - " ++ showType l
        eshow _ = []


dotFromTopology ::
  (Ord nty, Node.Show nty) =>
  Topo.Topology nty -> DotGraph T.Text
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
  (Ord nty, Node.Show nty) =>
  Gr.LNode nty Topo.NodeType -> DotNode T.Text
dotFromTopoNode (x, typ) =
   DotNode (dotIdentFromNode x)
      [Label $ StrLabel $ T.pack $ Node.show x ++ "\n" ++ showType typ,
       nodeColour, Style [SItem Filled []], Shape BoxShape]

dotFromTopoEdge ::
  (Ord nty, Node.Show nty) =>
  Gr.Edge nty -> DotEdge T.Text
dotFromTopoEdge e =
   case orientUndirEdge e of
      Edge x y ->
         DotEdge
            (dotIdentFromNode x) (dotIdentFromNode y)
            [Viz.Dir Viz.NoDir, originalEdgeColour]

dotIdentFromNode :: (Ord nty, Node.Show nty) => nty -> T.Text
dotIdentFromNode n = T.pack $ Node.show n



topology :: (Ord nty, Node.Show nty) => Topo.Topology nty -> IO ()
topology topo =
   runGraphvizCanvas Dot (dotFromTopology topo) Xlib


dotFromFlowTopology ::
  (Ord nty, Node.Show nty) =>
  Int -> FlowTopology nty -> DotSubGraph String
dotFromFlowTopology ident topo = DotSG True (Just (Int ident)) stmts
  where stmts = DotStmts attrs [] ns es
        attrs = [GraphAttrs [labelf ident]]
        ns = map mkNode (labNodes topo)
        idf x = show ident ++ "_" ++ show x
        labelf x = Label $ StrLabel $ T.pack (show x)
        mkNode x@(n, _) = DotNode (idf n) (nattrs x)
        nattrs x = [labNodef x, nodeColour, Style [SItem Filled []], Shape BoxShape ]
        labNodef (n, l) = Label $ StrLabel $ T.pack (Node.show n ++ " - " ++ showType l)
        es = map mkEdge (labEdges topo)
        mkEdge el =
           case orientEdge el of
              (Edge x y, d, _) ->
                 DotEdge (idf x) (idf y) [Viz.Dir d]

flowTopologies ::
  (Ord nty, Node.Show nty) =>
  [FlowTopology nty] -> IO ()
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
   (FormatValue a, Format output, Ord nty, Node.Show nty) =>
   Idx.Record -> StorageMap nty a -> Topo.LNode nty -> output
formatNodeStorage rec st (n@(Idx.SecNode _sec nid), ty) =
   Format.lines $
   Format.literal (Node.show nid) :
   Format.words [formatNodeType ty] :
      case ty of
         Storage ->
            [Format.words
               [lookupFormat st $ Idx.Storage rec n]]
         _ -> []


{- |
The 'Env' shall contain only values and functions for display.
It shall not contain values needed for computations.
-}
data Env nty output =
   Env {
      recordNumber :: output,
      formatEnergy, formatMaxEnergy,
      formatX, formatY,
      formatEta    :: Idx.SecNode nty -> Idx.SecNode nty -> output,
      formatTime   :: Idx.Section -> output,
      formatNode   :: Topo.LNode nty -> output
   }

lookupFormat ::
   (Ord (idx nty), MkIdxC idx, FormatValue a, Format output, Node.Show nty) =>
   M.Map (idx nty) a -> idx nty -> output
lookupFormat mp k =
   maybe (error $ "could not find index " ++ show (mkIdx k)) formatValue $
   M.lookup k mp

lookupFormatAssign ::
   (Ord (idx nty), MkIdxC idx, FormatValue a, Format output, Node.Show nty) =>
   M.Map (idx nty) a ->
   (Idx.SecNode nty -> Idx.SecNode nty -> idx nty) ->
   (Idx.SecNode nty -> Idx.SecNode nty -> output)
lookupFormatAssign mp makeIdx x y =
   case makeIdx x y of
      idx ->
         Format.assign (formatValue $ mkIdx idx) (lookupFormat mp idx)

sequFlowGraphWithEnv ::
  (Ord nty, Node.Show nty) =>
  SequFlowGraph nty -> Env nty Unicode -> IO ()
sequFlowGraphWithEnv g env =
   printGraph g (Just (recordNumber env, formatTime env)) (formatNode env) eshow
  where eshow e@((Edge uid vid), _l) =
           (case Topo.getEdgeType e of
              OriginalEdge -> []
              IntersectionEdge ->
                 formatMaxEnergy env uid vid :
                 formatY env uid vid :
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
  (FormatValue a, Ord nty, Node.Show nty) =>
  SequFlowGraph nty -> Interp.Env nty SingleRecord a -> IO ()
sequFlowGraphAbsWithEnv topo = sequFlowGraphWithEnv topo . envAbs

sequFlowGraphDeltaWithEnv ::
  (FormatValue a, Ord nty, Node.Show nty) =>
  SequFlowGraph nty -> Interp.Env nty SingleRecord a -> IO ()
sequFlowGraphDeltaWithEnv topo = sequFlowGraphWithEnv topo . envDelta


envAbs ::
   (FormatValue a, Format output, Ord nty, Node.Show nty) =>
   Interp.Env nty SingleRecord a -> Env nty output
envAbs (Interp.Env (SingleRecord rec) e _de me _dme _p _dp fn _dn dt x _dx y _dy _v st) =
   Env
      (Format.record rec)
      (lookupFormatAssign e (Idx.Energy rec))
      (lookupFormatAssign me (Idx.MaxEnergy rec))
      (lookupFormatAssign x (Idx.X rec))
      (lookupFormatAssign y (Idx.Y rec))
      (lookupFormatAssign fn (Idx.Eta rec))
      (lookupFormat dt . Idx.DTime rec)
      (formatNodeStorage rec st)

envDelta ::
   (FormatValue a, Format output, Ord nty, Node.Show nty) =>
   Interp.Env nty SingleRecord a -> Env nty output
envDelta (Interp.Env (SingleRecord rec) _e de _me dme _p _dp _fn dn dt _x dx _y dy _v st) =
   Env
      (Format.record rec)
      (lookupFormatAssign de (Idx.DEnergy rec))
      (lookupFormatAssign dme (Idx.DMaxEnergy rec))
      (lookupFormatAssign dx (Idx.DX rec))
      (lookupFormatAssign dy (Idx.DY rec))
      (lookupFormatAssign dn (Idx.DEta rec))
      (lookupFormat dt . Idx.DTime rec)
      (formatNodeStorage rec st)
