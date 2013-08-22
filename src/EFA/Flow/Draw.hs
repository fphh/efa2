module EFA.Flow.Draw (
  pdf, png, xterm,
  eps, plain, svg,
  fig, dot,
  title, bgcolour,

  cumulatedFlow,

  topologyWithEdgeLabels,
  topology,
  flowTopologies,
  ) where

import qualified EFA.Flow.Cumulated.Quantity as CumFlowQuant

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Format (Unicode(Unicode, unUnicode))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr
import EFA.Graph.Topology (FlowTopology)
import EFA.Graph (DirEdge(DirEdge))

import Data.GraphViz (
          GraphvizCanvas(Xlib), runGraphvizCanvas,
          GraphvizCommand(Dot), runGraphvizCommand,
          GraphvizOutput(..),
          GraphID(Int),
          GlobalAttributes(GraphAttrs),
          DotEdge(DotEdge),
          DotGraph(DotGraph),
          DotNode(DotNode),
          DotSubGraph(DotSG),
          DotStatements(DotStmts),
          attrStmts, nodeStmts, edgeStmts, graphStatements,
          directedGraph, strictGraph, subGraphs,
          graphID)

import Data.GraphViz.Attributes.Complete (
          Attribute(Color, FillColor), Color(RGB),
          )

import qualified Data.GraphViz.Attributes.Complete as Viz
import qualified Data.GraphViz.Attributes.Colors as Colors
import qualified Data.GraphViz.Attributes.Colors.X11 as X11Colors

import qualified Data.Accessor.Basic as Accessor

import qualified Data.Text.Lazy as T

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.List as List

import Data.Map (Map)
import Data.Foldable (Foldable, fold)

import Control.Monad (void)
import Control.Category ((.))

import Prelude hiding (reverse, (.))



structureEdgeColour :: Attribute
structureEdgeColour = Color [RGB 0 0 200]

shape :: Topo.NodeType a -> Viz.Shape
shape Topo.Crossing = Viz.PlainText
shape Topo.Source = Viz.DiamondShape
shape Topo.AlwaysSource = Viz.MDiamond
shape Topo.Sink = Viz.BoxShape
shape Topo.AlwaysSink = Viz.MSquare
shape (Topo.Storage _) = Viz.Ellipse
shape _ = Viz.BoxShape

color :: Topo.NodeType a -> Attribute
color (Topo.Storage _) = FillColor [RGB 251 177 97] -- ghlightorange
color _ = FillColor [RGB 136 215 251]  -- ghverylightblue

nodeAttrs :: Topo.NodeType a -> Attribute -> [Attribute]
nodeAttrs nt label =
  [ label, Viz.Style [Viz.SItem Viz.Filled []],
    Viz.Shape (shape nt), color nt ]


graphStatementsAcc ::
   Accessor.T (DotGraph t) (DotStatements t)
graphStatementsAcc =
   Accessor.fromSetGet (\s g -> g { graphStatements = s }) graphStatements

attrStmtsAcc ::
   Accessor.T (DotStatements n) [GlobalAttributes]
attrStmtsAcc =
   Accessor.fromSetGet (\stmts as -> as { attrStmts = stmts }) attrStmts

setGlobalAttrs :: GlobalAttributes -> DotGraph T.Text -> DotGraph T.Text
setGlobalAttrs attr =
   Accessor.modify (attrStmtsAcc . graphStatementsAcc) (attr:)

title :: String -> DotGraph T.Text -> DotGraph T.Text
title ti =
   setGlobalAttrs $ GraphAttrs [labelFromString ti]

bgcolour :: X11Colors.X11Color -> DotGraph T.Text -> DotGraph T.Text
bgcolour c =
   setGlobalAttrs $ GraphAttrs [Viz.BgColor [Colors.X11Color c]]


pdf, png, eps, svg, plain, fig, dot :: FilePath -> DotGraph T.Text -> IO ()
pdf file g = void $ runGraphvizCommand Dot g Pdf file
png file g = void $ runGraphvizCommand Dot g Png file
eps file g = void $ runGraphvizCommand Dot g Eps file
svg file g = void $ runGraphvizCommand Dot g Svg file
plain file g = void $ runGraphvizCommand Dot g Plain file
fig file g = void $ runGraphvizCommand Dot g Fig file
dot file g = void $ runGraphvizCommand Dot g DotOutput file

xterm :: DotGraph T.Text -> IO ()
xterm g = void $ runGraphvizCanvas Dot g Xlib


labelFromLines :: [Unicode] -> Attribute
labelFromLines = labelFromString . concatMap (++"\\l") . map unUnicode

labelFromUnicode :: Unicode -> Attribute
labelFromUnicode = labelFromString . unUnicode

labelFromString :: String -> Attribute
labelFromString = Viz.Label . Viz.StrLabel . T.pack


class Part part where
   dotIdentFromPart :: part -> String

instance Part Idx.Section where
   dotIdentFromPart (Idx.Section s) = show s

instance Part Idx.State where
   dotIdentFromPart (Idx.State s) = show s


dotIdentFromNode :: (Node.C node) => node -> T.Text
dotIdentFromNode n = T.pack $ Node.dotId n


topology :: (Node.C node) => Topo.Topology node -> DotGraph T.Text
topology topo = dotFromTopology Map.empty topo

topologyWithEdgeLabels ::
   (Node.C node) =>
   Map (node, node) String -> Topo.Topology node -> DotGraph T.Text
topologyWithEdgeLabels edgeLabels topo =
   dotFromTopology edgeLabels topo

dotFromTopology ::
   (Node.C node) =>
   Map (node, node) String ->
   Topo.Topology node -> DotGraph T.Text
dotFromTopology edgeLabels g =
   DotGraph {
      strictGraph = False,
      directedGraph = False,
      graphID = Just (Int 1),
      graphStatements =
         DotStmts {
            attrStmts = [],
            subGraphs = [],
            nodeStmts = map dotFromTopoNode $ Gr.labNodes g,
            edgeStmts = map (dotFromTopoEdge edgeLabels) $ Gr.edges g
         }
   }

dotFromTopoNode ::
  (Node.C node, StorageLabel store) =>
  Gr.LNode node (Topo.NodeType store) -> DotNode T.Text
dotFromTopoNode (x, typ) =
  DotNode
    (dotIdentFromNode x)
    (nodeAttrs typ $ labelFromUnicode $ Node.display x)

dotFromTopoEdge ::
  (Node.C node) =>
  Map (node, node) String ->
  DirEdge node -> DotEdge T.Text
dotFromTopoEdge edgeLabels e =
  case orientDirEdge e of
     (DirEdge x y, _, _) ->
           let lab = T.pack $ fold $ Map.lookup (x, y) edgeLabels
           in  DotEdge
                 (dotIdentFromNode x)
                 (dotIdentFromNode y)
                 [ Viz.Dir Viz.NoDir, structureEdgeColour,
                   Viz.Label $ Viz.StrLabel lab, Viz.EdgeTooltip lab ]


flowTopologies ::
   (Node.C node) =>
   [FlowTopology node] -> DotGraph T.Text
flowTopologies ts = DotGraph False True Nothing stmts
   where stmts = DotStmts attrs subgs [] []
         subgs = zipWith dotFromFlowTopology [0..] ts
         attrs = []

dotFromFlowTopology ::
   (Node.C node) =>
   Int -> FlowTopology node -> DotSubGraph T.Text
dotFromFlowTopology ident topo =
   DotSG True (Just (Int ident)) $
   DotStmts
      [GraphAttrs [labelFromString $ show ident]] []
      (map mkNode $ Gr.labNodes topo)
      (map mkEdge $ Gr.edges topo)
  where idf x = T.pack $ show ident ++ "_" ++ Node.dotId x
        mkNode x@(n, t) =
           DotNode (idf n)
              (nodeAttrs t $ labelFromUnicode $ formatTypedNode x)
        mkEdge el =
           case orientEdge el of
              (DirEdge x y, d, _) ->
                 DotEdge (idf x) (idf y) [Viz.Dir d]


class Reverse s where
   reverse :: s -> s

instance Reverse [s] where
   reverse = List.reverse


data Order = Id | Reverse deriving (Eq, Show)


orientEdge ::
   (Ord node) =>
   Gr.EitherEdge node -> (DirEdge node, Viz.DirType, Order)
orientEdge e =
   case e of
      Gr.EUnDirEdge ue -> (orientUndirEdge ue, Viz.NoDir, Id)
      Gr.EDirEdge de -> orientDirEdge de

orientUndirEdge :: Ord node => Gr.UnDirEdge node -> DirEdge node
orientUndirEdge (Gr.UnDirEdge x y) = DirEdge x y

orientDirEdge ::
   (Ord node) =>
   DirEdge node -> (DirEdge node, Viz.DirType, Order)
orientDirEdge (DirEdge x y) =
--   if comparing (\(Idx.SecNode s n) -> n) x y == LT
   if x < y
     then (DirEdge x y, Viz.Forward, Id)
     else (DirEdge y x, Viz.Back, Reverse)


class StorageLabel a where
   formatStorageLabel :: a -> String

instance StorageLabel () where
   formatStorageLabel () = ""

instance Show a => StorageLabel (Maybe a) where
   formatStorageLabel Nothing = ""
   formatStorageLabel (Just dir) = " " ++ show dir


showType :: StorageLabel store => Topo.NodeType store -> String
showType typ =
   case typ of
      Topo.Storage store -> "Storage" ++ formatStorageLabel store
      Topo.Sink          -> "Sink"
      Topo.AlwaysSink    -> "AlwaysSink"
      Topo.Source        -> "Source"
      Topo.AlwaysSource  -> "AlwaysSource"
      Topo.Crossing      -> "Crossing"
      Topo.DeadNode      -> "DeadNode"
      Topo.NoRestriction -> "NoRestriction"


formatTypedNode ::
   (Node.C node, StorageLabel store) =>
   (node, Topo.NodeType store) -> Unicode
formatTypedNode (n, l) =
   Unicode $ unUnicode (Node.display n) ++ " - " ++ showType l


cumulatedFlow ::
   (NodeType node, FormatValue a) =>
   CumFlowQuant.Graph node a ->
   DotGraph T.Text
cumulatedFlow =
   graph .
   Gr.mapNodeWithKey (const . dotFromCumNode) .
   Gr.mapEdge (labelFromLines . Fold.toList) .
   CumFlowQuant.mapGraphWithVar
      (\var val -> Format.assign (formatValue var) (formatValue val))


dotFromCumNode ::
   (NodeType node) =>
   node -> Viz.Attributes
dotFromCumNode x =
   nodeAttrs (nodeType x) $ labelFromUnicode $ Node.display x


class Node.C node => NodeType node where
   nodeType :: node -> Topo.NodeType ()


graph ::
   (Node.C node) =>
   Gr.Graph node Gr.DirEdge Viz.Attributes Attribute ->
   DotGraph T.Text
graph g =
   dotDirGraph $
   DotStmts {
      attrStmts = [],
      subGraphs = [],
      nodeStmts = map dotFromNode $ Gr.labNodes g,
      edgeStmts = map dotFromEdge $ Gr.labEdges g
   }

dotFromNode ::
   (Node.C node) =>
   Gr.LNode node Viz.Attributes -> DotNode T.Text
dotFromNode (n, attrs) =
   DotNode (dotIdentFromNode n) attrs

dotFromEdge ::
   (Node.C node) =>
   Gr.LEdge Gr.DirEdge node Attribute -> DotEdge T.Text
dotFromEdge (e, label) =
   case orientDirEdge e of
      (DirEdge x y, dir, _) ->
         DotEdge
            (dotIdentFromNode x) (dotIdentFromNode y)
            [label, Viz.Dir dir, structureEdgeColour]


dotDirGraph :: DotStatements str -> DotGraph str
dotDirGraph stmts =
   DotGraph {
      strictGraph = False,
      directedGraph = True,
      graphID = Just (Int 1),
      graphStatements = stmts
   }
