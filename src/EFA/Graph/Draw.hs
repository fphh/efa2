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
           edgeType,
           getFlowDirection,
           FlowDirectionField, FlowTopology)
import EFA.Graph (Edge(Edge), labNodes, labEdges)

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
   SequFlowGraph ->
   Maybe (Unicode, Idx.Section -> Unicode) ->
   (Topo.LNode -> Unicode) ->
   (Topo.LEdge -> [Unicode]) ->
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


dotFromSecNode:: (Topo.LNode -> Unicode) -> Topo.LNode -> DotNode T.Text
dotFromSecNode nshow n@(x, _) =
   DotNode (dotIdentFromSecNode x)
      [displabel, nodeColour, Style [SItem Filled []], Shape BoxShape ]
  where displabel = Label $ StrLabel $ T.pack $ unUnicode $ nshow n

dotFromSecEdge :: (Topo.LEdge -> [Unicode]) -> Topo.LEdge -> DotEdge T.Text
dotFromSecEdge eshow e@(_, elabel) =
   DotEdge
      (dotIdentFromSecNode x) (dotIdentFromSecNode y)
      [displabel, Viz.Dir dir, colour]
  where (Edge x y, dir, order) = orientEdge e
        displabel =
           Label $ StrLabel $ T.pack $
           L.intercalate "\n" $ map unUnicode $ order $ eshow e
        colour =
           case edgeType elabel of
              IntersectionEdge -> intersectionEdgeColour
              _ -> originalEdgeColour
        --colour = originalEdgeColour

dotIdentFromSecNode :: Idx.SecNode -> T.Text
dotIdentFromSecNode (Idx.SecNode (Idx.Section s) (Idx.Node n)) =
   T.pack $ "s" ++ show s ++ "n" ++ show n

printGraph, printGraphX, _printGraphDot ::
   SequFlowGraph ->
   Maybe (Unicode, Idx.Section -> Unicode) ->
   (Topo.LNode -> Unicode) ->
   (Topo.LEdge -> [Unicode]) ->
   IO ()
printGraph = printGraphX

printGraphX g recTShow nshow eshow =
   runGraphvizCanvas Dot (dotFromSequFlowGraph g recTShow nshow eshow) Xlib

_printGraphDot g recTShow nshow eshow =
   void $
   runGraphvizCommand Dot
      (dotFromSequFlowGraph g recTShow nshow eshow)
      XDot "result/graph.dot"


sequFlowGraph :: SequFlowGraph -> IO ()
sequFlowGraph topo =
   printGraph topo Nothing nshow eshow
  where nshow (Idx.SecNode _ n, l) = Unicode $ show n ++ " - " ++ showNodeType l
        eshow _ = []


dotFromTopology :: Topo.Topology -> DotGraph T.Text
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

dotFromTopoNode:: Gr.LNode Idx.Node Topo.NodeType -> DotNode T.Text
dotFromTopoNode (x, typ) =
   DotNode (dotIdentFromNode x)
      [Label $ StrLabel $ T.pack $ show x ++ "\n" ++ showNodeType typ,
       nodeColour, Style [SItem Filled []], Shape BoxShape]

dotFromTopoEdge :: Gr.Edge Idx.Node -> DotEdge T.Text
dotFromTopoEdge e =
   case orientUndirEdge e of
      Edge x y ->
         DotEdge
            (dotIdentFromNode x) (dotIdentFromNode y)
            [Viz.Dir Viz.NoDir, originalEdgeColour]

dotIdentFromNode :: Idx.Node -> T.Text
dotIdentFromNode (Idx.Node n) = T.pack $ show n



topology :: Topo.Topology -> IO ()
topology topo =
   runGraphvizCanvas Dot (dotFromTopology topo) Xlib


dotFromFlowTopology :: Int -> FlowTopology -> DotSubGraph String
dotFromFlowTopology ident topo = DotSG True (Just (Int ident)) stmts
  where stmts = DotStmts attrs [] ns es
        attrs = [GraphAttrs [labelf ident]]
        ns = map mkNode (labNodes topo)
        idf x = show ident ++ "_" ++ show x
        labelf x = Label $ StrLabel $ T.pack (show x)
        mkNode x@(n, _) = DotNode (idf n) (nattrs x)
        nattrs x = [labNodef x, nodeColour, Style [SItem Filled []], Shape BoxShape ]
        labNodef (n, l) = Label $ StrLabel $ T.pack (show n ++ " - " ++ showNodeType l)
        es = map mkEdge (labEdges topo)
        mkEdge el =
           case orientEdge el of
              (Edge x y, d, _) ->
                 DotEdge (idf x) (idf y) [Viz.Dir d]

flowTopologies :: [FlowTopology] -> IO ()
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


showNodeType :: NodeType -> String
showNodeType = show

formatNodeType :: Format output => NodeType -> output
formatNodeType = Format.literal . showNodeType

formatNodeStorage ::
   (FormatValue a, Format output) =>
   Idx.Record -> StorageMap a -> Topo.LNode -> output
formatNodeStorage rec st (n@(Idx.SecNode _sec nid), ty) =
   Format.lines $
   Format.literal (show nid) :
   Format.words [Format.literal "Type:", formatNodeType ty] :
      case ty of
         Storage ->
            [Format.words
               [Format.literal "Content:",
                lookupFormat st $ Idx.Storage rec n]]
         _ -> []


{- |
The 'Env' shall contain only values and functions for display.
It shall not contain values needed for computations.
-}
data Env output =
   Env {
      recordNumber :: output,
      formatEnergy, formatMaxEnergy,
      formatX, formatY,
      formatEta    :: Idx.SecNode -> Idx.SecNode -> output,
      formatTime   :: Idx.Section -> output,
      formatNode   :: Topo.LNode -> output
   }

lookupFormat ::
   (Ord idx, MkIdxC idx, FormatValue a, Format output) =>
   M.Map idx a -> idx -> output
lookupFormat mp k =
   maybe (error $ "could not find index " ++ show (mkIdx k)) formatValue $
   M.lookup k mp

lookupFormatAssign ::
   (Ord idx, MkIdxC idx, FormatValue a, Format output) =>
   M.Map idx a ->
   (Idx.SecNode -> Idx.SecNode -> idx) ->
   (Idx.SecNode -> Idx.SecNode -> output)
lookupFormatAssign mp makeIdx x y =
   case makeIdx x y of
      idx ->
         Format.assign (formatValue $ mkIdx idx) (lookupFormat mp idx)

sequFlowGraphWithEnv :: SequFlowGraph -> Env Unicode -> IO ()
sequFlowGraphWithEnv g env =
   printGraph g (Just (recordNumber env, formatTime env)) (formatNode env) eshow
  where eshow (e@(Edge uid vid), l) =
           case edgeType l of
              OriginalEdge -> eshowBase e
              IntersectionEdge ->
                 formatMaxEnergy env uid vid :
                 formatY env uid vid :
                 Format.empty :
                 eshowBase e
        eshowBase (Edge uid vid) =
           formatEnergy env uid vid :
           formatX env uid vid :
           formatEta env uid vid :
           formatX env vid uid :
           formatEnergy env vid uid :
           []

sequFlowGraphAbsWithEnv ::
   FormatValue a => SequFlowGraph -> Interp.Env SingleRecord a -> IO ()
sequFlowGraphAbsWithEnv topo = sequFlowGraphWithEnv topo . envAbs

sequFlowGraphDeltaWithEnv ::
   FormatValue a => SequFlowGraph -> Interp.Env SingleRecord a -> IO ()
sequFlowGraphDeltaWithEnv topo = sequFlowGraphWithEnv topo . envDelta


envAbs ::
   (FormatValue a, Format output) =>
   Interp.Env SingleRecord a -> Env output
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
   (FormatValue a, Format output) =>
   Interp.Env SingleRecord a -> Env output
envDelta
      (Interp.Env (SingleRecord rec) _e de _me dme _p _dp _fn dn dt _x dx _y dy _v st) =
   Env
      (Format.record rec)
      (lookupFormatAssign de (Idx.DEnergy rec))
      (lookupFormatAssign dme (Idx.DMaxEnergy rec))
      (lookupFormatAssign dx (Idx.DX rec))
      (lookupFormatAssign dy (Idx.DY rec))
      (lookupFormatAssign dn (Idx.DEta rec))
      (lookupFormat dt . Idx.DTime rec)
      (formatNodeStorage rec st)
