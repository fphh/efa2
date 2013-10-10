{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Draw (
   pdf, png, xterm,
   eps, plain, svg,
   fig, dot,
   title, bgcolour,

   seqFlowGraph,
   stateFlowGraph,

   Options, optionsDefault,
   absoluteVariable, deltaVariable,
   showVariableIndex, hideVariableIndex,
   showStorageEdge, hideStorageEdge,
   showStorage, hideStorage,
   showEtaNode, hideEtaNode,

   cumulatedFlow,

   topologyWithEdgeLabels,
   topology,
   flowTopologies,
   ) where

import qualified EFA.Flow.Sequence.Quantity as SeqFlowQuant
import qualified EFA.Flow.State.Quantity as StateFlowQuant
import qualified EFA.Flow.Cumulated.Quantity as CumFlowQuant
import qualified EFA.Flow.Quantity as FlowQuant
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.PartMap as PartMap

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue, formatAssign)
import EFA.Report.Format (Format, Unicode(Unicode, unUnicode))

import qualified EFA.Equation.Variable as Var

import EFA.Signal.Signal (SignalIdx(SignalIdx), Range(Range))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph; import EFA.Graph (Graph)
import EFA.Graph.Topology (FlowTopology)
import EFA.Graph (DirEdge(DirEdge))

import qualified EFA.Utility.Map as MapU
import Data.GraphViz (
          GraphID(Int, Str),
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

import qualified Data.GraphViz.Commands as VizCmd
import qualified Data.GraphViz.Attributes.Complete as Viz
import qualified Data.GraphViz.Attributes.Colors as Colors
import qualified Data.GraphViz.Attributes.Colors.X11 as X11Colors

import qualified Data.Accessor.Basic as Accessor

import qualified Data.Text.Lazy as T

import qualified Data.Map as Map
import qualified Data.List as List

import Data.Map (Map)
import Data.Foldable (Foldable, foldMap, fold)
import Data.Maybe (maybeToList)
import Data.Tuple.HT (mapFst, mapFst3, snd3)
import Data.Monoid ((<>))

import Control.Category ((.))
import Control.Monad (void)

import Prelude hiding (sin, reverse, init, last, sequence, (.))



structureEdgeColour :: Attribute
structureEdgeColour = Color [RGB 0 0 200]

storageEdgeColour :: Attribute
storageEdgeColour = Color [RGB 200 0 0]

contentEdgeColour :: Attribute
contentEdgeColour = Color [RGB 0 200 0]

shape :: Node.Type a -> Viz.Shape
shape Node.Crossing = Viz.PlainText
shape Node.Source = Viz.DiamondShape
shape Node.AlwaysSource = Viz.MDiamond
shape Node.Sink = Viz.BoxShape
shape Node.AlwaysSink = Viz.MSquare
shape (Node.Storage _) = Viz.Ellipse
shape _ = Viz.BoxShape

color :: Node.Type a -> Attribute
color (Node.Storage _) = FillColor [RGB 251 177 97] -- ghlightorange
color _ = FillColor [RGB 136 215 251]  -- ghverylightblue

nodeAttrs :: Node.Type a -> Attribute -> [Attribute]
nodeAttrs nt label =
  [ label, Viz.Style [Viz.SItem Viz.Filled []],
    Viz.Shape (shape nt), color nt ]


data Triple a = Triple a a a

instance Foldable Triple where
   foldMap f (Triple pre eta suc) = f pre <> f eta <> f suc

data StructureEdgeLabel =
     HideEtaNode [Unicode]
   | ShowEtaNode (Triple [Unicode])


dotFromFlowGraph ::
   (Part part, Node.C node) =>
   ([DotSubGraph T.Text], [DotEdge T.Text]) ->
   Map node
      ((Unicode, Unicode),
       Map (Idx.StorageEdge part node) [Unicode]) ->
   Map part (String, Graph node Graph.EitherEdge Unicode StructureEdgeLabel) ->
   DotGraph T.Text
dotFromFlowGraph (contentGraphs, contentEdges) sts sq =
   dotDirGraph $
   DotStmts {
      attrStmts = [],
      subGraphs =
         (Map.elems $ Map.mapWithKey dotFromPartGraph sq)
         ++
         dotFromInitExitNodes sq
            (Idx.NoExit Idx.Init, Idx.Exit)
            (fmap fst sts)
         ++
         contentGraphs,
      nodeStmts = [],
      edgeStmts =
         (dotFromStorageEdges $ fmap snd sts)
         ++
         contentEdges
   }


dotFromStorageGraphs ::
   (Node.C node, FormatValue a, Ord (edge node), Graph.Edge edge) =>
   Map node (Map Idx.Boundary a) ->
   Map Idx.Section (Graph node edge (FlowQuant.Sums v) edgeLabel) ->
   ([DotSubGraph T.Text], [DotEdge T.Text])
dotFromStorageGraphs storages sequence =
   (Map.elems $ Map.mapWithKey dotFromStorageGraph $
    fmap (fmap formatValue) $ MapU.flip storages,
    (\(last, inner) ->
       dotFromContentEdge Nothing Idx.initSection
          (fmap (const $ Just Topo.In) storages) ++
       dotFromContentEdge (Just last) Idx.exitSection
          (fmap (const $ Just Topo.Out) storages) ++
       fold inner) $
    Map.mapAccumWithKey
       (\before current gr ->
          (Idx.afterSection current,
           dotFromContentEdge (Just before) (Idx.augment current) $
           fmap FlowQuant.dirFromSums $
           Map.filterWithKey (\node _ -> Node.typ node == Node.Storage ()) $
           Graph.nodeLabels gr))
       Idx.initial sequence)

dotFromStorageGraph ::
   (Node.C node) =>
   Idx.Boundary -> Map node Unicode ->
   DotSubGraph T.Text
dotFromStorageGraph bnd ns =
   DotSG True
      (Just $ Str $ T.pack $ "b" ++ dotIdentFromBoundary bnd) $
   DotStmts
      [GraphAttrs [labelFromString $ "After " ++
       case bnd of
          Idx.Following Idx.Init -> "Init"
          Idx.Following (Idx.NoInit (Idx.Section s)) -> show s]]
      []
      (Map.elems $
       Map.mapWithKey
          (\node -> dotFromBndNode (Idx.PartNode bnd node)) ns)
      []


dotFromInitExitNodes ::
   (Part part, Node.C node) =>
   map part dummy ->
   (Idx.Augmented part, Idx.Augmented part) ->
   Map node (Unicode, Unicode) ->
   [DotSubGraph T.Text]
dotFromInitExitNodes _ (init, exit) initExit =
   dotFromInitOrExitNodes init "Init" (fmap fst initExit) :
   dotFromInitOrExitNodes exit "Exit" (fmap snd initExit) :
   []

dotFromInitOrExitNodes ::
   (Part part, Node.C node) =>
   Idx.Augmented part ->
   String ->
   Map node Unicode ->
   DotSubGraph T.Text
dotFromInitOrExitNodes part name initExit =
   DotSG True (Just $ Str $ T.pack $ dotIdentFromAugmented part) $
   DotStmts
      [GraphAttrs [labelFromString name]]
      []
      (Map.elems $ Map.mapWithKey (dotFromAugNode part) initExit)
      []


dotFromPartGraph ::
   (Part part, Node.C node) =>
   part ->
   (String,
    Graph node Graph.EitherEdge Unicode StructureEdgeLabel) ->
   DotSubGraph T.Text
dotFromPartGraph current (subtitle, gr) =
   let (etaNodes,edges) =
          fold $
          Map.mapWithKey
             (\e labels ->
                case labels of
                   ShowEtaNode l ->
                      mapFst (:[]) $ dotFromStructureEdgeEta current e l
                   HideEtaNode l ->
                      ([], [dotFromStructureEdge current e l])) $
          Graph.edgeLabels gr
   in  DotSG True (Just $ Str $ T.pack $ dotIdentFromPart current) $
       DotStmts
          [GraphAttrs [labelFromString subtitle]]
          []
          ((Map.elems $
            Map.mapWithKey (dotFromAugNode (Idx.augment current)) $
            Graph.nodeLabels gr)
           ++
           etaNodes)
          edges


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
pdf   = runGraphvizCommand VizCmd.Pdf
png   = runGraphvizCommand VizCmd.Png
eps   = runGraphvizCommand VizCmd.Eps
svg   = runGraphvizCommand VizCmd.Svg
fig   = runGraphvizCommand VizCmd.Fig
dot   = runGraphvizCommand VizCmd.DotOutput
plain = runGraphvizCommand VizCmd.Plain

runGraphvizCommand ::
   VizCmd.GraphvizOutput -> FilePath -> DotGraph T.Text -> IO ()
runGraphvizCommand target path g =
   void $ VizCmd.runGraphvizCommand VizCmd.Dot g target path

xterm :: DotGraph T.Text -> IO ()
xterm g = void $ VizCmd.runGraphvizCanvas VizCmd.Dot g VizCmd.Xlib


dotFromAugNode ::
   (Part part, Node.C node) =>
   Idx.Augmented part -> node -> Unicode -> DotNode T.Text
dotFromAugNode part n label =
   DotNode
      (dotIdentFromAugNode $ Idx.PartNode part n)
      (nodeAttrs (Node.typ n) $ labelFromUnicode label)

dotFromBndNode ::
   (Node.C node) =>
   Idx.BndNode node -> Unicode -> DotNode T.Text
dotFromBndNode n label =
   DotNode
      (dotIdentFromBndNode n)
      (nodeAttrs (Node.Storage ()) $
       labelFromUnicode label)

dotFromStructureEdge ::
   (Node.C node, Part part) =>
   part -> Graph.EitherEdge node -> [Unicode] -> DotEdge T.Text
dotFromStructureEdge part e label =
   let (DirEdge x y, dir, ord) = orientFlowEdge $ Idx.InPart part e
   in  DotEdge
          (dotIdentFromPartNode x) (dotIdentFromPartNode y)
          [labelFromLines $ order ord label,
           Viz.Dir dir, structureEdgeColour]

dotFromStructureEdgeEta ::
   (Node.C node, Part part) =>
   part -> Graph.EitherEdge node ->
   Triple [Unicode] ->
   (DotNode T.Text, [DotEdge T.Text])
dotFromStructureEdgeEta part e label =
   let (DirEdge x y, dir, ord) = orientFlowEdge $ Idx.InPart part e
       Triple pre eta suc = order ord label
       did = dotIdentFromEtaNode x y
   in  (DotNode did [labelFromLines eta],
        [DotEdge
            (dotIdentFromPartNode x) did
            [labelFromLines pre,
             Viz.Dir dir, structureEdgeColour],
         DotEdge
            did (dotIdentFromPartNode y)
            [labelFromLines suc,
             Viz.Dir dir, structureEdgeColour]])

dotFromStorageEdges ::
   (Node.C node, Part part) =>
   Map node (Map (Idx.StorageEdge part node) [Unicode]) ->
   [DotEdge T.Text]
dotFromStorageEdges =
   fold .
   Map.mapWithKey
      (\node ->
         Map.elems .
         Map.mapWithKey
            (\edge -> dotFromStorageEdge (Idx.ForNode edge node)))

dotFromStorageEdge ::
   (Node.C node, Part part) =>
   Idx.ForNode (Idx.StorageEdge part) node ->
   [Unicode] -> DotEdge T.Text
dotFromStorageEdge e lns =
   DotEdge
      (dotIdentFromAugNode $ Idx.storageEdgeFrom e)
      (dotIdentFromAugNode $ Idx.storageEdgeTo   e)
      [labelFromLines lns, Viz.Dir Viz.Forward,
       storageEdgeColour, Viz.Constraint True]

dotFromContentEdge ::
   (Node.C node) =>
   Maybe Idx.Boundary ->
   Idx.AugmentedSection ->
   Map node (Maybe Topo.StoreDir) ->
   [DotEdge T.Text]
dotFromContentEdge mbefore aug =
   let dotEdge from to =
          DotEdge from to [Viz.Dir Viz.Forward, contentEdgeColour]
   in  fold .
       Map.mapWithKey
          (\n dir ->
             let sn = Idx.PartNode aug n
                 withBefore f =
                    foldMap (\before -> f $ Idx.PartNode before n) mbefore
                 withCurrent f =
                    foldMap (\current -> f $ Idx.PartNode current n) $
                    Idx.boundaryFromAugSection aug
             in  (withBefore $ \from ->
                  withCurrent $ \to ->
                  [dotEdge (dotIdentFromBndNode from) (dotIdentFromBndNode to)])
                 ++
                 case dir of
                    Nothing -> []
                    Just Topo.In ->
                       withCurrent $ \bn ->
                          [dotEdge (dotIdentFromAugNode sn) (dotIdentFromBndNode bn)]
                    Just Topo.Out ->
                       withBefore $ \bn ->
                          [dotEdge (dotIdentFromBndNode bn) (dotIdentFromAugNode sn)])



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


dotIdentFromPartNode ::
   (Part part, Node.C node) => Idx.PartNode part node -> T.Text
dotIdentFromPartNode (Idx.PartNode s n) =
   T.pack $ "s" ++ dotIdentFromPart s ++ "n" ++ Node.dotId n

dotIdentFromAugNode ::
   (Part part, Node.C node) => Idx.AugNode part node -> T.Text
dotIdentFromAugNode (Idx.PartNode b n) =
   T.pack $ "s" ++ dotIdentFromAugmented b ++ "n" ++ Node.dotId n

dotIdentFromAugmented :: (Part part) => Idx.Augmented part -> String
dotIdentFromAugmented =
   Idx.switchAugmented "init" "exit" dotIdentFromPart

dotIdentFromBndNode :: (Node.C node) => Idx.BndNode node -> T.Text
dotIdentFromBndNode (Idx.PartNode b n) =
   T.pack $ "b" ++ dotIdentFromBoundary b ++ "n" ++ Node.dotId n

dotIdentFromBoundary :: Idx.Boundary -> String
dotIdentFromBoundary (Idx.Following a) =
   case a of
      Idx.Init -> "init"
      Idx.NoInit s -> dotIdentFromPart s

dotIdentFromEtaNode ::
   (Node.C node, Part part) =>
   Idx.PartNode part node -> Idx.PartNode part node -> T.Text
dotIdentFromEtaNode (Idx.PartNode s x) (Idx.PartNode _s y) =
   T.pack $
      "s" ++ dotIdentFromPart s ++
      "x" ++ Node.dotId x ++
      "y" ++ Node.dotId y

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
            nodeStmts = map dotFromTopoNode $ Graph.labNodes g,
            edgeStmts = map (dotFromTopoEdge edgeLabels) $ Graph.edges g
         }
   }

dotFromTopoNode ::
   (Node.C node, StorageLabel store) =>
   Graph.LNode node (Node.Type store) -> DotNode T.Text
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
flowTopologies ts =
   DotGraph False True Nothing $
   DotStmts [] (zipWith dotFromFlowTopology [0..] ts) [] []

dotFromFlowTopology ::
   (Node.C node) =>
   Int -> FlowTopology node -> DotSubGraph T.Text
dotFromFlowTopology ident topo =
   DotSG True (Just (Int ident)) $
   DotStmts
      [GraphAttrs [labelFromString $ show ident]] []
      (map mkNode $ Graph.labNodes topo)
      (map mkEdge $ Graph.edges topo)
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

instance (Reverse a) => Reverse (Triple a) where
   reverse (Triple pre eta suc) =
      Triple (reverse suc) (reverse eta) (reverse pre)


data Order = Id | Reverse deriving (Eq, Show)

order :: Reverse s => Order -> s -> s
order Id = id
order Reverse = reverse


orientFlowEdge ::
   (Ord node) =>
   Idx.InPart part Graph.EitherEdge node ->
   (DirEdge (Idx.PartNode part node), Viz.DirType, Order)
orientFlowEdge (Idx.InPart sec e) =
   mapFst3 (fmap (Idx.PartNode sec)) $
   case e of
      Graph.EUnDirEdge ue -> (orientUndirEdge ue, Viz.NoDir, Id)
      Graph.EDirEdge de -> orientDirEdge de

orientEdge ::
   (Ord node) =>
   Graph.EitherEdge node -> (DirEdge node, Viz.DirType, Order)
orientEdge e =
   case e of
      Graph.EUnDirEdge ue -> (orientUndirEdge ue, Viz.NoDir, Id)
      Graph.EDirEdge de -> orientDirEdge de

orientUndirEdge :: Ord node => Graph.UnDirEdge node -> DirEdge node
orientUndirEdge (Graph.UnDirEdge x y) = DirEdge x y

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


showType :: StorageLabel store => Node.Type store -> String
showType typ =
   case typ of
      Node.Storage store -> "Storage" ++ formatStorageLabel store
      Node.Sink          -> "Sink"
      Node.AlwaysSink    -> "AlwaysSink"
      Node.Source        -> "Source"
      Node.AlwaysSource  -> "AlwaysSource"
      Node.Crossing      -> "Crossing"
      Node.DeadNode      -> "DeadNode"
      Node.NoRestriction -> "NoRestriction"


formatNodeType ::
   (Format output, StorageLabel store) =>
   Node.Type store -> output
formatNodeType = Format.literal . showType

formatTypedNode ::
   (Node.C node, StorageLabel store) =>
   (node, Node.Type store) -> Unicode
formatTypedNode (n, l) =
   Unicode $ unUnicode (Node.display n) ++ " - " ++ showType l


data Options output =
   Options {
      optRecordIndex :: output -> output,
      optVariableIndex :: Bool,
      optStorageEdge :: Bool,
      optStorage :: Bool,
      optEtaNode :: Bool
   }

optionsDefault :: Format output => Options output
optionsDefault =
   Options {
      optRecordIndex = id,
      optVariableIndex = False,
      optStorageEdge = True,
      optStorage = False,
      optEtaNode = False
   }

absoluteVariable, deltaVariable,
   showVariableIndex, hideVariableIndex,
   showStorageEdge, hideStorageEdge,
   showStorage, hideStorage,
   showEtaNode, hideEtaNode
   :: Format output => Options output -> Options output
absoluteVariable opts =
   opts { optRecordIndex = Format.record Idx.Absolute }

deltaVariable opts =
   opts { optRecordIndex = Format.record Idx.Delta }

showVariableIndex opts = opts { optVariableIndex = True }
hideVariableIndex opts = opts { optVariableIndex = False }

{-
If storage edges are shown then the subgraphs are not aligned vertically.
-}
showStorageEdge opts = opts { optStorageEdge = True }
hideStorageEdge opts = opts { optStorageEdge = False }

showStorage opts = opts { optStorage = True }
hideStorage opts = opts { optStorage = False }

showEtaNode opts = opts { optEtaNode = True }
hideEtaNode opts = opts { optEtaNode = False }



seqFlowGraph ::
   (FormatValue a, FormatValue v, Node.C node) =>
   Options Unicode -> SeqFlowQuant.Graph node a v -> DotGraph T.Text
seqFlowGraph opts gr =
   dotFromFlowGraph
      (if optStorage opts
         then
            dotFromStorageGraphs
               (fmap snd3 $ SeqFlowQuant.storages gr)
               (fmap (FlowTopo.topology . snd) $ SeqFlowQuant.sequence gr)
         else ([], []))
      (Map.mapWithKey
          (\node (partMap, _bnds, edges) ->
             (stateInitExitShow node partMap,
              if optStorageEdge opts
                then Map.mapWithKey (storageEdgeSeqShow opts node) edges
                else Map.empty)) $
       SeqFlowQuant.storages gr)
      (snd $
       Map.mapAccumWithKey
          (\before sec (rng, FlowTopoPlain.Section dt topo) ->
             (,) (Idx.afterSection sec) $
             (show sec ++
              " / Range " ++ formatRange rng ++
              " / Time " ++ unUnicode (formatValue dt),
              Graph.mapNodeWithKey
                 (\node sums ->
                    let (partMap, stores, _) =
                           maybe (error "missing node") id $
                           Map.lookup node $
                           SeqFlowQuant.storages gr
                    in  formatNodeStorage opts node
                           (let content bnd =
                                   Map.findWithDefault
                                      (error "no storage content") bnd stores
                            in  (content before,
                                 content $ Idx.afterSection sec))
                           (fmap (maybe (error "missing section") (flip (,)) $
                                  PartMap.lookup sec partMap) $
                            FlowQuant.dirFromSums sums)) $
              Graph.mapEdgeWithKey
                 (\edge ->
                    if optEtaNode opts
                      then ShowEtaNode . structureEdgeShowEta opts sec edge
                      else HideEtaNode . structureEdgeShow opts sec edge)
                 topo))
          Idx.initial $
       SeqFlowQuant.sequence gr)


formatRange :: Range -> String
formatRange (Range (SignalIdx from) (SignalIdx to)) =
   show from ++ "-" ++ show to

formatNodeStorage ::
   (FormatValue a, Format output, Node.C node) =>
   Options output ->
   node ->
   (a, a) -> Maybe (Topo.StoreDir, a) -> output
formatNodeStorage opts node beforeAfter sinout =
   case Node.typ node of
      ty ->
         Format.lines $
         Node.display node :
         formatNodeType ty :
            case ty of
               Node.Storage _ ->
                  if optStorage opts
                    then formatStorageUpdate sinout
                    else formatStorageEquation beforeAfter sinout
               _ -> []


formatStorageUpdate ::
   (FormatValue a, Format output) =>
   Maybe (Topo.StoreDir, a) -> [output]
formatStorageUpdate sinout =
   case sinout of
      Just (_, s)  -> [formatValue s]
      Nothing -> []


formatStorageEquation ::
   (FormatValue a, Format output) =>
   (a, a) -> Maybe (Topo.StoreDir, a) -> [output]
formatStorageEquation (before, after) sinout =
   formatValue before :
   (case sinout of
      Just (Topo.In,  s) -> [Format.plus  Format.empty $ formatValue s]
      Just (Topo.Out, s) -> [Format.minus Format.empty $ formatValue s]
      Nothing -> []) ++
   Format.assign Format.empty (formatValue after) :
   []


stateFlowGraph ::
   (FormatValue a, FormatValue v, Node.C node) =>
   Options Unicode -> StateFlowQuant.Graph node a v -> DotGraph T.Text
stateFlowGraph opts gr =
   dotFromFlowGraph
      ([], [])
      (Map.mapWithKey
          (\node (partMap, edges) ->
             (stateInitExitShow node partMap,
              if optStorageEdge opts
                then Map.mapWithKey (storageEdgeStateShow opts node) edges
                else Map.empty)) $
       StateFlowQuant.storages gr)
      (Map.mapWithKey
          (\state (FlowTopoPlain.Section dt topo) ->
             (show state ++ " / Time " ++ unUnicode (formatValue dt),
              Graph.mapNodeWithKey
                 (\node _sums ->
                    stateNodeShow node $ PartMap.lookup state $
                    maybe (error "Draw.stateFlowGraph") fst $ Map.lookup node $
                    StateFlowQuant.storages gr) $
              Graph.mapEdgeWithKey
                 (\edge ->
                    if optEtaNode opts
                      then ShowEtaNode . structureEdgeShowEta opts state edge
                      else HideEtaNode . structureEdgeShow opts state edge)
                 topo)) $
       StateFlowQuant.states gr)


stateInitExitShow ::
   (Node.C node, FormatValue a, Format output) =>
   node -> PartMap.PartMap part a -> (output, output)
stateInitExitShow node partMap =
   (stateNodeShow node $ Just $ PartMap.init partMap,
    stateNodeShow node $ Just $ PartMap.exit partMap)

stateNodeShow ::
   (Node.C node, FormatValue a, Format output) =>
   node -> Maybe a -> output
stateNodeShow node msum =
   case Node.typ node of
      ty ->
         Format.lines $
         Node.display node :
         formatNodeType ty :
            case ty of
               Node.Storage _ -> maybeToList $ fmap formatValue msum
               _ -> []

storageEdgeSeqShow ::
   (Node.C node, FormatValue a, Format output) =>
   Options output ->
   node ->
   Idx.StorageEdge Idx.Section node ->
   SeqFlowQuant.Carry a ->
   [output]
storageEdgeSeqShow opts node edge carry =
   case SeqFlowQuant.mapCarryWithVar
           (formatAssignWithOpts opts) node edge carry of
      labels ->
         SeqFlowQuant.carryMaxEnergy labels :
         SeqFlowQuant.carryEnergy labels :
         SeqFlowQuant.carryXOut labels :
         SeqFlowQuant.carryXIn labels :
         []

storageEdgeStateShow ::
   (Node.C node, FormatValue a, Format output) =>
   Options output ->
   node ->
   Idx.StorageEdge Idx.State node ->
   StateFlowQuant.Carry a ->
   [output]
storageEdgeStateShow opts node edge carry =
   case StateFlowQuant.mapCarryWithVar
           (formatAssignWithOpts opts) node edge carry of
      labels ->
         StateFlowQuant.carryEnergy labels :
         StateFlowQuant.carryXOut labels :
         StateFlowQuant.carryXIn labels :
         []

structureEdgeShow ::
   (Node.C node, Ord part, FormatValue a, Format.Part part, Format output) =>
   Options output ->
   part -> Graph.EitherEdge node ->
   Maybe (FlowQuant.Flow a) -> [output]
structureEdgeShow opts part =
   FlowQuant.switchEdgeFlow (const []) $ \edge flow ->
   case FlowQuant.mapFlowWithVar (formatAssignWithOpts opts) part edge flow of
      labels ->
         FlowQuant.flowEnergyOut labels :
         FlowQuant.flowXOut labels :
         FlowQuant.flowEta labels :
         FlowQuant.flowXIn labels :
         FlowQuant.flowEnergyIn labels :
         []

structureEdgeShowEta ::
   (Node.C node, Ord part, FormatValue a, Format.Part part, Format output) =>
   Options output ->
   part -> Graph.EitherEdge node ->
   Maybe (FlowQuant.Flow a) -> Triple [output]
structureEdgeShowEta opts part =
   FlowQuant.switchEdgeFlow (const $ Triple [] [] []) $ \edge flow ->
   case FlowQuant.mapFlowWithVar (formatAssignWithOpts opts) part edge flow of
      labels ->
         Triple
            (FlowQuant.flowEnergyOut labels :
             FlowQuant.flowXOut labels :
             [])
            (formatValue (FlowQuant.flowEta flow) :
             [])
            (FlowQuant.flowXIn labels :
             FlowQuant.flowEnergyIn labels :
             [])


cumulatedFlow ::
   (Node.C node, FormatValue a) =>
   CumFlowQuant.Graph node a ->
   DotGraph T.Text
cumulatedFlow =
   graph .
   Graph.mapNodeWithKey (const . dotFromCumNode) .
   Graph.mapEdgeWithKey
      (\e flow ->
         dotFromCumEdge e (CumFlowQuant.flowDTime flow) $
         map ($flow) $
            CumFlowQuant.flowPowerOut :
            CumFlowQuant.flowEnergyOut :
            CumFlowQuant.flowXOut :
            CumFlowQuant.flowEta :
            CumFlowQuant.flowXIn :
            CumFlowQuant.flowEnergyIn :
            CumFlowQuant.flowPowerIn :
            []) .
   CumFlowQuant.mapGraphWithVar formatAssign


dotFromCumNode ::
   (Node.C node) =>
   node -> DotNode T.Text
dotFromCumNode n =
   DotNode (dotIdentFromNode n) $
   nodeAttrs (Node.typ n) $ labelFromUnicode $ Node.display n

dotFromCumEdge ::
   (Node.C node) =>
   Graph.DirEdge node -> Unicode -> [Unicode] -> DotEdge T.Text
dotFromCumEdge e hd label =
   case orientDirEdge e of
      (DirEdge x y, dir, ord) ->
         DotEdge
            (dotIdentFromNode x) (dotIdentFromNode y)
            [labelFromLines $ hd : order ord label,
             Viz.Dir dir, structureEdgeColour]


graph ::
   (Node.C node) =>
   Graph node Graph.DirEdge (DotNode T.Text) (DotEdge T.Text) ->
   DotGraph T.Text
graph g =
   dotDirGraph $
   DotStmts {
      attrStmts = [],
      subGraphs = [],
      nodeStmts = map snd $ Graph.labNodes g,
      edgeStmts = map snd $ Graph.labEdges g
   }


dotDirGraph :: DotStatements str -> DotGraph str
dotDirGraph stmts =
   DotGraph {
      strictGraph = False,
      directedGraph = True,
      graphID = Just (Int 1),
      graphStatements = stmts
   }


formatAssignWithOpts ::
   (Node.C node, Var.FormatIndex idx, Format.EdgeIdx idx,
    FormatValue a, Format output) =>
   Options output -> idx node -> a -> output
formatAssignWithOpts opts idx val =
   Format.assign
      (optRecordIndex opts $
       if optVariableIndex opts
         then Var.formatIndex idx
         else Format.edgeIdent idx)
      (formatValue val)
