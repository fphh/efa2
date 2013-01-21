module EFA.Graph.Draw where

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Format (Format, Unicode(Unicode, unUnicode))

import qualified EFA.Equation.Env as Interp
import EFA.Equation.Variable (MkIdxC, mkIdx)
import EFA.Equation.Env (StorageMap, SingleRecord(SingleRecord))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import EFA.Graph.Topology
          (SequFlowGraph,
           NodeType(Storage),
           EdgeType(OriginalEdge, IntersectionEdge),
           edgeType,
           isIntersectionEdge,
           getFlowDirection,
           FlowDirectionField, FlowTopology)
import EFA.Graph
          (Graph, Edge(Edge),
           labNodes, labEdges, edgeLabels, delNodes, delEdgeSet)

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

import Data.Eq.HT (equating)

import qualified Data.Text.Lazy as T

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.HT as HTL
import qualified Data.NonEmpty as NonEmpty
import qualified Data.NonEmpty.Mixed as NonEmptyM

import Control.Monad (void)


nodeColour :: Attribute
nodeColour = FillColor [RGB 230 230 240]

clusterColour :: Attribute
clusterColour = FillColor [RGB 250 250 200]

originalEdgeColour :: Attribute
originalEdgeColour = Color [RGB 0 0 200]

intersectionEdgeColour :: Attribute
intersectionEdgeColour = Color [RGB 200 0 0]


mkDotGraph ::
   SequFlowGraph ->
   Maybe (Unicode, Idx.Section -> Unicode) ->
   (Topo.LNode -> Unicode) ->
   (Topo.LEdge -> [Unicode]) ->
   DotGraph T.Text
mkDotGraph g recTShow nshow eshow =
  DotGraph { strictGraph = False,
             directedGraph = True,
             graphID = Just (Int 1),
             graphStatements = stmts }
  where interEs = M.filter isIntersectionEdge $ edgeLabels g
        g' = delEdgeSet (M.keysSet interEs) g
        section n = case fst n of Idx.SecNode s _ -> s
        cs =
           HTL.removeEach $
           NonEmptyM.groupBy (equating section) $
           labNodes g
        sg (ns, ms) = DotSG True (Just (Int $ fromEnum sl)) (DotStmts gattrs [] xs ys)
          where sl = section $ NonEmpty.head ns
                xs = map (mkDotNode nshow) $ NonEmpty.flatten ns
                ys = map (mkDotEdge eshow) $ labEdges $
                     delNodes (map fst (concatMap NonEmpty.flatten ms)) g'
                gattrs = [GraphAttrs [Label (StrLabel (T.pack str))]]
                str =
                   show sl ++ " / " ++
                   case recTShow of
                      Nothing -> "NoRecord"
                      Just (Unicode n, timef) ->
                         n ++ " / Time " ++ unUnicode (timef sl)
        stmts = DotStmts { attrStmts = [],
                           subGraphs = map sg cs,
                           nodeStmts = [],
                           edgeStmts = map (mkDotEdge eshow) $ M.toList interEs }


mkDotNode:: (Topo.LNode -> Unicode) -> Topo.LNode -> DotNode T.Text
mkDotNode nshow n@(x, _) =
   DotNode (dotIdentFromSecNode x)
      [displabel, nodeColour, Style [SItem Filled []], Shape BoxShape ]
  where displabel = Label $ StrLabel $ T.pack $ unUnicode $ nshow n

mkDotEdge :: (Topo.LEdge -> [Unicode]) -> Topo.LEdge -> DotEdge T.Text
mkDotEdge eshow e@(_, elabel) =
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

printGraph, printGraphX, printGraphDot ::
   SequFlowGraph ->
   Maybe (Unicode, Idx.Section -> Unicode) ->
   (Topo.LNode -> Unicode) ->
   (Topo.LEdge -> [Unicode]) ->
   IO ()
printGraph = printGraphX

printGraphX g recTShow nshow eshow =
   runGraphvizCanvas Dot (mkDotGraph g recTShow nshow eshow) Xlib

printGraphDot g recTShow nshow eshow =
   void $
   runGraphvizCommand Dot
      (mkDotGraph g recTShow nshow eshow)
      XDot "result/graph.dot"

drawTopologyX' :: SequFlowGraph -> IO ()
drawTopologyX' topo =
   printGraph topo Nothing (Unicode . show) ((:[]) . Unicode . show)


drawTopologySimple :: SequFlowGraph -> IO ()
drawTopologySimple topo =
   printGraph topo Nothing nshow eshow
  where nshow (Idx.SecNode _ n, l) = Unicode $ show n ++ " - " ++ showNodeType l
        eshow _ = []

dsg :: Int -> FlowTopology -> DotSubGraph String
dsg ident topo = DotSG True (Just (Int ident)) stmts
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

drawTopologyXs' :: [FlowTopology] -> IO ()
drawTopologyXs' ts = runGraphvizCanvas Dot g Xlib
  where g = DotGraph False True Nothing stmts
        stmts = DotStmts attrs subgs [] []
        subgs = zipWith dsg [0..] ts
        attrs = []


orientEdge ::
   (Ord n, FlowDirectionField el) =>
   (Edge n, el) -> (Edge n, DirType, [s] -> [s])
orientEdge (Edge x y, l) =
   case getFlowDirection l of
      Topo.UnDir ->
         (if x < y then Edge x y else Edge y x,
          NoDir, const [])
      Topo.Dir ->
--         if comparing (\(Idx.SecNode s n) -> n) x y == LT
         if x < y
           then (Edge x y, Forward, id)
           else (Edge y x, Back, reverse)


showNodeType :: NodeType -> String
showNodeType = show

formatNodeType :: Format output => NodeType -> output
formatNodeType = Format.literal . showNodeType

formatNode ::
   (FormatValue a, Format output) =>
   Idx.Record -> StorageMap a -> Topo.LNode -> output
formatNode rec st (n@(Idx.SecNode _sec nid), ty) =
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
      recordNumber_ :: output,
      formatEnergy_ :: Idx.SecNode -> Idx.SecNode -> output,
      formatMaxEnergy_ :: Idx.SecNode -> Idx.SecNode -> output,
      formatX_      :: Idx.SecNode -> Idx.SecNode -> output,
      formatY_      :: Idx.SecNode -> Idx.SecNode -> output,
      formatEta_    :: Idx.SecNode -> Idx.SecNode -> output,
      formatTime_ :: Idx.Section -> output,
      formatNode_ :: Topo.LNode -> output
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

draw :: SequFlowGraph -> Env Unicode -> IO ()
draw g
   (Env rec formatEnergy formatMaxEnergy formatX formatY formatEta tshow nshow) =
      printGraph g (Just (rec, tshow)) nshow eshow
  where eshow (Edge uid vid, l) =
           case edgeType l of
              OriginalEdge ->
                 formatEnergy uid vid :
                 formatX uid vid :
                 formatEta uid vid :
                 formatX vid uid :
                 formatEnergy vid uid :
                 []
              IntersectionEdge ->
                 formatMaxEnergy uid vid :
                 formatY uid vid :
                 Format.empty :
                 formatEnergy uid vid :
                 formatX uid vid :
                 formatEta uid vid :
                 formatX vid uid :
                 formatEnergy vid uid :
                 []

drawTopology ::
   FormatValue a => SequFlowGraph -> Interp.Env SingleRecord a -> IO ()
drawTopology topo = draw topo . envAbs

drawDeltaTopology ::
   FormatValue a => SequFlowGraph -> Interp.Env SingleRecord a -> IO ()
drawDeltaTopology topo = draw topo . envDelta


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
      (formatNode rec st)

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
      (formatNode rec st)
