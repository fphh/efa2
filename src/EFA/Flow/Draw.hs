module EFA.Flow.Draw (
  pdf, png, xterm,
  eps, plain, svg,
  fig, dot,
  title, bgcolour,

  cumulatedFlow,
  ) where

import qualified EFA.Flow.Cumulated.Quantity as CumFlowQuant

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Format (Unicode(unUnicode))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr
import EFA.Graph (DirEdge(DirEdge))

import Data.GraphViz (
          runGraphvizCanvas,
          GraphvizCanvas(Xlib),
          runGraphvizCommand,
          GraphID(Int),
          GlobalAttributes(GraphAttrs),
          GraphvizCommand(Dot),
          GraphvizOutput(..),
          DotEdge(DotEdge),
          DotGraph(DotGraph),
          DotNode(DotNode),
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
import qualified Data.List as List

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


class Reverse s where
   reverse :: s -> s

instance Reverse [s] where
   reverse = List.reverse


data Order = Id | Reverse deriving (Eq, Show)


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
