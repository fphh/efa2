module EFA.Graph.Draw (
  pdf, png, xterm,
  eps, plain, svg,
  fig, dot,
  title, bgcolour,

  sequFlowGraph,
  sequFlowGraphWithEnv,
  sequFlowGraphAbsWithEnv,
  sequFlowGraphDeltaWithEnv,

  stateFlowGraph,
  stateFlowGraphWithEnv,

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

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Format (Format, Unicode(Unicode, unUnicode))

import qualified EFA.Graph.StateFlow.Environment as StateEnv
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Variable as Var

import qualified EFA.Signal.Sequence as Sequ
import EFA.Signal.Signal (SignalIdx(SignalIdx))

import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph.CumulatedFlow as Cum
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Graph; import EFA.Graph (Graph)
import EFA.Graph.Topology (FlowTopology)
import EFA.Graph (DirEdge(DirEdge))

import qualified EFA.Utility.TotalMap as TMap

import Data.GraphViz (
          runGraphvizCanvas,
          GraphvizCanvas(Xlib),
          runGraphvizCommand,
          GraphID(Int,Str),
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
          graphID,
          GraphvizOutput(..))

import Data.GraphViz.Attributes.Complete (
          Attribute(Color, FillColor), Color(RGB),
          )

import qualified Data.GraphViz.Attributes.Complete as Viz
import qualified Data.GraphViz.Attributes.Colors as Colors
import qualified Data.GraphViz.Attributes.Colors.X11 as X11Colors

import qualified Data.Accessor.Basic as Accessor

import qualified Data.Text.Lazy as T

import qualified Data.Map as Map
import qualified Data.List.HT as ListHT
import qualified Data.List as List

import Data.Map (Map)
import Data.Foldable (Foldable, foldMap, fold)
import Data.Maybe.HT (toMaybe)
import Data.Maybe (mapMaybe)
import Data.Tuple.HT (mapFst, mapFst3, mapPair)
import Data.Monoid ((<>))

import Control.Monad (void)
import Control.Category ((.))

import Prelude hiding (reverse, (.))



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

mkNodeAttrs :: Node.Type a -> Attribute -> [Attribute]
mkNodeAttrs nodeType label =
  [ label, Viz.Style [Viz.SItem Viz.Filled []],
    Viz.Shape (shape nodeType), color nodeType ]


data Triple a = Triple a a a

instance Foldable Triple where
   foldMap f (Triple pre eta suc) = f pre <> f eta <> f suc


data StructureEdgeShow part node =
     HideEtaNode (Idx.InPart part Graph.EitherEdge node -> [Unicode])
   | ShowEtaNode (Idx.InPart part Graph.EitherEdge node -> Triple [Unicode])

type StorageEdgeShow part node =
        Idx.ForNode (Idx.StorageEdge part) node -> [Unicode]

dotFromSequFlowGraph ::
  (Node.C node) =>
  Flow.RangeGraph node ->
  Maybe (Idx.Section -> Unicode) ->
  (Maybe Idx.Boundary -> Topo.LDirNode Idx.Section node -> Unicode) ->
  Maybe (Idx.BndNode node -> Unicode) ->
  StructureEdgeShow Idx.Section node ->
  Maybe (StorageEdgeShow Idx.Section node) ->
  DotGraph T.Text
dotFromSequFlowGraph (rngs, g)
    mtshow nshow storeNShow structEShow storeEShow =
       dotDirGraph stmts

  where (topoEs, interEs) = groupEdges g
        topoNs = groupNodes g

        storageConnections =
           zip
             (Nothing :
              map (assertJust . Idx.boundaryFromAugSection)
                 (Map.keys topoNs)) $
           Map.toAscList $
           TMap.intersectionPartialWith (,) (TMap.cons [] topoEs) topoNs

        stmts =
          DotStmts {
            attrStmts = [],
            subGraphs =
              map
                (uncurry $
                 dotFromSectionGraph rngs mtshow nshow structEShow)
              storageConnections
              ++
              (case storeNShow of
                 Nothing -> []
                 Just s ->
                    mapMaybe
                       (\(a,ns) ->
                           flip fmap (Idx.maybeExit a) $ \b ->
                              dotFromStorageGraph s (b, ns)) $
                    Map.toList topoNs),
            nodeStmts = [],
            edgeStmts =
              (case storeNShow of
                Nothing -> []
                Just _ ->
                  concatMap
                     (\(before, (_current, (_es, ns))) ->
                        dotFromContentEdge before ns)
                     storageConnections)
              ++
              dotFromStorageEdges storeEShow interEs
          }

        assertJust (Just y) = Just y
        assertJust Nothing =
           error "Exit section cannot be predecessor of another section"

dotFromSectionGraph ::
  (Node.C node) =>
  Map Idx.Section Sequ.Range ->
  Maybe (Idx.Section -> Unicode) ->
  (Maybe Idx.Boundary -> Topo.StNode Idx.Section store node -> Unicode) ->
  StructureEdgeShow Idx.Section node ->
  Maybe Idx.Boundary ->
  (Idx.AugmentedSection,
   ([Idx.InSection Graph.EitherEdge node],
    [Topo.StNode Idx.Section store node])) ->
  DotSubGraph T.Text
dotFromSectionGraph rngs mtshow nshow structEShow
  before (current, (es, ns)) =
    DotSG True (Just $ Str $ T.pack $ dotIdentFromAugmented current) $
    DotStmts
      [GraphAttrs [labelFromString $ str current]]
      []
      dns des
  where (dns,des) =
           case structEShow of
             ShowEtaNode eshow ->
                case unzip $ map (dotFromStructureEdgeEta eshow) es of
                   (dns0,des0) ->
                      (map (dotFromAugNode (nshow before)) ns ++ dns0,
                       concat des0)
             HideEtaNode eshow ->
                (map (dotFromAugNode (nshow before)) ns,
                 map (dotFromStructureEdge eshow) es)
        str =
           Idx.switchAugmented "Init" "Exit" $ \s ->
                 show s ++
                 (case Map.lookup s rngs of
                     Just (Sequ.Range (SignalIdx from) (SignalIdx to)) ->
                        " / Range " ++ show from ++ "-" ++ show to
                     Nothing -> error $ "missing range for " ++ show s) ++
                 (flip foldMap mtshow $ \tshow ->
                    " / Time " ++ unUnicode (tshow s))

dotFromStorageGraph ::
  (Node.C node) =>
  (Idx.BndNode node -> Unicode) ->
  (Idx.InitOrSection, [Topo.StNode Idx.Section store node]) ->
  DotSubGraph T.Text
dotFromStorageGraph nshow (current, ns) =
  DotSG True
    (Just $ Str $ T.pack $ "b" ++ dotIdentFromBoundary (Idx.Following current)) $
  DotStmts
    [GraphAttrs [labelFromString $ "After " ++
     case current of
        Idx.Init -> "Init"
        Idx.NoInit (Idx.Section s) -> show s]]
    []
    (map (dotFromBndNode nshow) $
     mapMaybe Idx.bndNodeFromAugNode $ map fst $
     mapMaybe (\(n,t) -> fmap ((,) n) $ Topo.maybeStorage t) ns)
    []


dotFromStateFlowGraph ::
  (Node.C node) =>
  Topo.StateFlowGraph node ->
  Maybe (Idx.State -> Unicode) ->
  (Topo.LDirNode Idx.State node -> Unicode) ->
  StructureEdgeShow Idx.State node ->
  Maybe (StorageEdgeShow Idx.State node) ->
  DotGraph T.Text
dotFromStateFlowGraph g mtshow nshow structEShow storeEShow =
  case groupEdges g of
    (topoEs, interEs) ->
      dotDirGraph $
      DotStmts {
        attrStmts = [],
        subGraphs =
          map (dotFromStateGraph mtshow nshow structEShow) $
          Map.toAscList $
          TMap.intersectionPartialWith (,)
             (TMap.cons [] topoEs)
             (groupNodes g),
        nodeStmts = [],
        edgeStmts = dotFromStorageEdges storeEShow interEs
      }


dotFromStateGraph ::
  (Node.C node) =>
  Maybe (Idx.State -> Unicode) ->
  (Topo.StNode Idx.State store node -> Unicode) ->
  StructureEdgeShow Idx.State node ->
  (Idx.AugmentedState,
   ([Idx.InState Graph.EitherEdge node],
    [Topo.StNode Idx.State store node])) ->
  DotSubGraph T.Text
dotFromStateGraph mtshow nshow structEShow (current, (es, ns)) =
    DotSG True (Just $ Str $ T.pack $ dotIdentFromAugmented current) $
    DotStmts
      [GraphAttrs [labelFromString $ str current]]
      []
      dns des
  where (dns,des) =
           case structEShow of
             ShowEtaNode eshow ->
                case unzip $ map (dotFromStructureEdgeEta eshow) es of
                   (dns0,des0) ->
                      (map (dotFromAugNode nshow) ns ++ dns0,
                       concat des0)
             HideEtaNode eshow ->
                (map (dotFromAugNode nshow) ns,
                 map (dotFromStructureEdge eshow) es)
        str =
           Idx.switchAugmented "Init" "Exit" $ \s ->
              show s ++
              (flip foldMap mtshow $ \tshow ->
                 " / Time " ++ unUnicode (tshow s))


groupEdges ::
   (Ord part, Ord node) =>
   Topo.FlowGraph part node ->
   (Map (Idx.Augmented part) [Idx.InPart part Graph.EitherEdge node],
    [Idx.ForNode (Idx.StorageEdge part) node])
groupEdges =
   mapFst (Map.fromListWith (++)) .
   ListHT.unzipEithers .
   map
      (\e ->
         case Topo.edgeType e of
            Topo.StructureEdge se@(Idx.InPart s _) ->
               Left (Idx.augment s, [se])
            Topo.StorageEdge se -> Right se) .
   Graph.edges

groupNodes ::
   (Ord (e (Idx.PartNode part node)), Ord part, Ord node, Graph.Edge e) =>
   Graph (Idx.PartNode part node) e nl el ->
   Map part [(Idx.PartNode part node, nl)]
groupNodes =
   Map.fromListWith (++) .
   map (\nl@(Idx.PartNode s _, _) -> (s, [nl])) .
   Graph.labNodes


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


dotFromAugNode ::
  (Part part, Node.C node) =>
  (Topo.StNode part store node -> Unicode) ->
  Topo.StNode part store node -> DotNode T.Text
dotFromAugNode nshow n@(x, nodeType) =
  DotNode
    (dotIdentFromAugNode x)
    (mkNodeAttrs nodeType $ labelFromUnicode $ nshow n)

dotFromBndNode ::
  (Node.C node) =>
  (Idx.BndNode node -> Unicode) ->
  Idx.BndNode node -> DotNode T.Text
dotFromBndNode nshow n =
  DotNode
    (dotIdentFromBndNode n)
    (mkNodeAttrs (Node.Storage ()) $
     labelFromUnicode $ nshow n)

dotFromStructureEdge ::
  (Node.C node, Part part) =>
  (Idx.InPart part Graph.EitherEdge node -> [Unicode]) ->
  Idx.InPart part Graph.EitherEdge node -> DotEdge T.Text
dotFromStructureEdge eshow e =
   DotEdge
      (dotIdentFromPartNode x) (dotIdentFromPartNode y)
      [labelFromLines $ order ord $ eshow e,
       Viz.Dir dir, structureEdgeColour]
  where (DirEdge x y, dir, ord) = orientFlowEdge e

dotFromStructureEdgeEta ::
  (Node.C node, Part part) =>
  (Idx.InPart part Graph.EitherEdge node -> Triple [Unicode]) ->
  Idx.InPart part Graph.EitherEdge node ->
  (DotNode T.Text, [DotEdge T.Text])
dotFromStructureEdgeEta eshow e =
   let (DirEdge x y, dir, ord) = orientFlowEdge e
       Triple pre eta suc = order ord $ eshow e
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
  Maybe (StorageEdgeShow part node) ->
  [Idx.ForNode (Idx.StorageEdge part) node] -> [DotEdge T.Text]
dotFromStorageEdges storeEShow interEs =
   flip foldMap storeEShow $ \eshow ->
      map (dotFromStorageEdge eshow) interEs

dotFromStorageEdge ::
  (Node.C node, Part part) =>
  StorageEdgeShow part node ->
  Idx.ForNode (Idx.StorageEdge part) node -> DotEdge T.Text
dotFromStorageEdge eshow e =
   DotEdge
      (dotIdentFromAugNode $ Idx.storageEdgeFrom e)
      (dotIdentFromAugNode $ Idx.storageEdgeTo   e)
      [labelFromLines $ eshow e, Viz.Dir Viz.Forward,
       storageEdgeColour, Viz.Constraint True]

dotFromContentEdge ::
  (Node.C node) =>
  Maybe Idx.Boundary ->
  [Topo.LDirNode Idx.Section node] ->
  [DotEdge T.Text]
dotFromContentEdge mbefore ns =
  let dotEdge from to =
         DotEdge from to [Viz.Dir Viz.Forward, contentEdgeColour]
  in  concatMap
         (\(sn@(Idx.PartNode _sec n), t) ->
            let withBefore f =
                   foldMap (\before -> f $ Idx.PartNode before n) mbefore
                withCurrent f =
                   foldMap f (Idx.bndNodeFromAugNode sn)
            in  (withBefore $ \from ->
                 withCurrent $ \to ->
                 [dotEdge (dotIdentFromBndNode from) (dotIdentFromBndNode to)])
                ++
                case t of
                   Nothing -> []
                   Just Topo.In ->
                      withCurrent $ \bn ->
                         [dotEdge (dotIdentFromAugNode sn) (dotIdentFromBndNode bn)]
                   Just Topo.Out ->
                      withBefore $ \bn ->
                         [dotEdge (dotIdentFromBndNode bn) (dotIdentFromAugNode sn)]) $
      mapMaybe (\(n,t) -> fmap ((,) n) $ Topo.maybeStorage t) ns



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

sequFlowGraph ::
  (Node.C node) =>
  Flow.RangeGraph node -> DotGraph T.Text
sequFlowGraph topo =
  dotFromSequFlowGraph topo Nothing nshow Nothing
     (HideEtaNode $ const []) (Just $ const [])
  where nshow _before (Idx.PartNode _ n, l) = formatTypedNode (n,l)


stateFlowGraph ::
  (Node.C node) =>
  Topo.StateFlowGraph node -> DotGraph T.Text
stateFlowGraph topo =
  dotFromStateFlowGraph topo Nothing nshow
     (HideEtaNode $ const []) (Just $ const [])
  -- where nshow _before (Idx.PartNode _ n, l) = formatTypedNode (n,l)
  where nshow _before = Unicode ""



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
    (mkNodeAttrs typ $ labelFromUnicode $ Node.display x)

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



topology :: (Node.C node) => Topo.Topology node -> DotGraph T.Text
topology topo = dotFromTopology Map.empty topo

topologyWithEdgeLabels ::
  (Node.C node) =>
  Map (node, node) String -> Topo.Topology node -> DotGraph T.Text
topologyWithEdgeLabels edgeLabels topo =
   dotFromTopology edgeLabels topo

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
              (mkNodeAttrs t $ labelFromUnicode $ formatTypedNode x)
        mkEdge el =
           case orientEdge el of
              (DirEdge x y, d, _) ->
                 DotEdge (idf x) (idf y) [Viz.Dir d]

flowTopologies ::
  (Node.C node) =>
  [FlowTopology node] -> DotGraph T.Text
flowTopologies ts = DotGraph False True Nothing stmts
  where stmts = DotStmts attrs subgs [] []
        subgs = zipWith dotFromFlowTopology [0..] ts
        attrs = []


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


formatNodeStorage ::
   (FormatValue a, Format output, Node.C node) =>
   Options output ->
   Env.StorageMap node a ->
   Env.StInSumMap node a ->
   Env.StOutSumMap node a ->
   Maybe Idx.Boundary -> Topo.LDirNode Idx.Section node -> output
formatNodeStorage opts st sis sos mBeforeBnd (Idx.PartNode aug nid, ty) =
   Format.lines $
   Node.display nid :
   formatNodeType ty :
      case ty of
         Node.Storage dir ->
            case (aug, mBeforeBnd) of
               (Idx.Exit, Just _) ->
                  [lookupFormat opts sis $ XIdx.stInSum XIdx.exitSection nid]
               (Idx.NoExit Idx.Init, Nothing) ->
                  [lookupFormat opts sos $ XIdx.stOutSum XIdx.initSection nid]
               (Idx.NoExit Idx.Init, Just _) ->
                  error "initial section has no predecessor"
               (Idx.NoExit (Idx.NoInit sec), Just beforeBnd) ->
                  if optStorage opts
                    then formatStorageUpdate opts sis sos dir sec nid
                    else formatStorageEquation opts st sis sos dir beforeBnd sec nid
               (_, Nothing) ->
                  error "a true section must have a predecessor"
         _ -> []


formatStorageUpdate ::
   (Format output, Node.C node, FormatValue a) =>
   Options output ->
   Map (XIdx.StInSum node) a ->
   Map (XIdx.StOutSum node) a ->
   Maybe Topo.StoreDir ->
   Idx.Section -> node -> [output]
formatStorageUpdate opts sis sos dir sec nid =
   case dir of
      Just Topo.In ->
         [lookupFormat opts sos $ XIdx.stOutSum sec nid]
      Just Topo.Out ->
         [lookupFormat opts sis $ XIdx.stInSum sec nid]
      Nothing -> []

formatStorageEquation ::
   (Format output, Node.C node, FormatValue a) =>
   Options output ->
   Map (XIdx.Storage node) a ->
   Map (XIdx.StInSum node) a ->
   Map (XIdx.StOutSum node) a ->
   Maybe Topo.StoreDir ->
   Idx.Boundary ->
   Idx.Section -> node -> [output]
formatStorageEquation opts st sis sos dir beforeBnd sec nid =
   case (lookupFormat opts st $ XIdx.storage beforeBnd nid,
         lookupFormat opts st $ XIdx.storage (Idx.afterSection sec) nid) of
      (before, after) ->
         before :
         (case dir of
            Just Topo.In ->
               [Format.plus Format.empty $
                lookupFormat opts sos $ XIdx.stOutSum sec nid]
            Just Topo.Out ->
               [Format.minus Format.empty $
                lookupFormat opts sis $ XIdx.stInSum sec nid]
            Nothing -> []) ++
         Format.assign Format.empty after :
         []


formatNodeContent ::
   (FormatValue a, Format output, Node.C node) =>
   Options output ->
   Env.StorageMap node a ->
   Idx.BndNode node -> output
formatNodeContent opts st (Idx.PartNode bnd nid) =
   lookupFormat opts st $ XIdx.storage bnd nid


formatStateNode ::
   (FormatValue a, Format output, Node.C node) =>
   Options output ->
   StateEnv.StInSumMap node a ->
   StateEnv.StOutSumMap node a ->
   Topo.LDirNode Idx.State node -> output
formatStateNode opts sis sos (augNode@(Idx.PartNode _aug nid), ty) =
   Format.lines $
   Node.display nid :
   formatNodeType ty :
      case ty of
         Node.Storage dir ->
            case Topo.viewNodeDir (augNode, dir) of
               Nothing -> []
               Just (Topo.ViewNodeIn (Idx.PartNode part node)) ->
                  [lookupFormat opts sos $ Idx.ForNode (Idx.StOutSum part) node]
               Just (Topo.ViewNodeOut (Idx.PartNode part node)) ->
                  [lookupFormat opts sis $ Idx.ForNode (Idx.StInSum part) node]
         _ -> []


lookupFormat ::
   (Ord (idx node), Var.FormatIndex idx,
    FormatValue a, Format output, Node.C node) =>
   Options output -> Map (idx node) a -> idx node -> output
lookupFormat _opts mp k =
   maybe
      (error $ "Draw.lookupFormat - could not find index "
         ++ (Format.unUnicode $ Var.formatIndex k)
         ++ " in "
         ++ (show $
             map (mapPair (Format.unUnicode . Var.formatIndex,
                           Format.showRaw . showValue)) $
             Map.toList mp))
      showValue $
      Map.lookup k mp
   where showValue = formatValue

lookupFormatAssign ::
   (Ord (idx node), Format.EdgeIdx idx, Var.FormatIndex idx,
    FormatValue a, Format output, Node.C node) =>
   Options output ->
   Map (idx node) a ->
   (edge node -> idx node) ->
   (edge node -> output)
lookupFormatAssign opts mp makeIdx x =
   case makeIdx x of
      idx ->
         Format.assign
            (optRecordIndex opts $
             if optVariableIndex opts
               then Var.formatIndex idx
               else Format.edgeIdent idx)
            (lookupFormat opts mp idx)

sequFlowGraphWithEnv ::
  (FormatValue a, FormatValue v, Node.C node) =>
  Options Unicode -> Flow.RangeGraph node ->
  Env.Complete node a v -> DotGraph T.Text
sequFlowGraphWithEnv opts g
    (Env.Complete (Env.Scalar me st se sx sis sos) (Env.Signal e _p n dt x _s)) =
  dotFromSequFlowGraph g (Just formatTime)
    formatNode (toMaybe (optStorage opts) formatStorageContent)
    (structureEdgeShow opts e x n)
    (toMaybe (optStorageEdge opts) storeEShow)
  where formatMaxEnergy =
           lookupFormatAssign opts me $ Idx.liftForNode Idx.MaxEnergy
        formatStEnergy =
           lookupFormatAssign opts se $ Idx.liftForNode Idx.StEnergy
        formatStX =
           lookupFormatAssign opts sx $ Idx.liftForNode Idx.StX
        formatTime =
           lookupFormat opts dt . flip Idx.InPart Idx.DTime
        formatNode =
           formatNodeStorage opts st sis sos
        formatStorageContent =
           formatNodeContent opts st

        storeEShow edge =
           case Idx.liftForNode Idx.storageTransFromEdge edge of
              te ->
                 formatMaxEnergy edge :
                 formatStEnergy edge :
                 formatStX te :
                 formatStX (Idx.flip te) :
                 []

sequFlowGraphAbsWithEnv ::
   (FormatValue a, FormatValue v, Node.C node) =>
   Flow.RangeGraph node ->
   Env.Complete node a v ->
   DotGraph T.Text
sequFlowGraphAbsWithEnv =
   sequFlowGraphWithEnv $ absoluteVariable optionsDefault

sequFlowGraphDeltaWithEnv ::
   (FormatValue a, FormatValue v, Node.C node) =>
   Flow.RangeGraph node ->
   Env.Complete node a v -> DotGraph T.Text
sequFlowGraphDeltaWithEnv =
   sequFlowGraphWithEnv $ deltaVariable optionsDefault


stateFlowGraphWithEnv ::
  (FormatValue a, FormatValue v, Node.C node) =>
  Options Unicode -> Topo.StateFlowGraph node ->
  StateEnv.Complete node a v -> DotGraph T.Text
stateFlowGraphWithEnv opts g
    (StateEnv.Complete
       (StateEnv.Scalar se sx sis sos)
       (StateEnv.Signal e _p n dt x _s)) =
  dotFromStateFlowGraph g (Just formatTime)
    formatNode
    (structureEdgeShow opts e x n)
    (toMaybe (optStorageEdge opts) storeEShow)
  where formatStEnergy =
           lookupFormatAssign opts se $ Idx.liftForNode Idx.StEnergy
        formatStX =
           lookupFormatAssign opts sx $ Idx.liftForNode Idx.StX
        formatTime =
           lookupFormat opts dt . flip Idx.InPart Idx.DTime
        formatNode =
           formatStateNode opts sis sos

        storeEShow edge =
           case Idx.liftForNode Idx.storageTransFromEdge edge of
              te ->
                 formatStEnergy edge :
                 formatStX te :
                 formatStX (Idx.flip te) :
                 []


structureEdgeShow ::
  (Node.C node, Ord part, FormatValue a, Format.Part part) =>
  Options Unicode ->
  Map (Idx.InPart part Idx.Energy node) a ->
  Map (Idx.InPart part Idx.X node) a ->
  Map (Idx.InPart part Idx.Eta node) a ->
  StructureEdgeShow part node
structureEdgeShow opts e x n =
  let formatEnergy =
         lookupFormatAssign opts e $ Idx.liftInPart Idx.Energy
      formatX =
         lookupFormatAssign opts x $ Idx.liftInPart Idx.X
      formatEta =
         lookupFormatAssign opts n $ Idx.liftInPart Idx.Eta
      formatEtaPlain =
         lookupFormat opts n . Idx.liftInPart Idx.Eta

      structEShowEta (Idx.InPart sec ee) =
         case ee of
            Graph.EUnDirEdge _ -> Triple [] [] []
            Graph.EDirEdge de ->
               case Idx.InPart sec (Topo.structureEdgeFromDirEdge de) of
                  edge ->
                    Triple
                       (formatEnergy edge : formatX edge : [])
                       (formatEtaPlain edge : [])
                       (formatX (Idx.flip edge) :
                        formatEnergy (Idx.flip edge) :
                        [])

      structEShow (Idx.InPart sec ee) =
         case ee of
            Graph.EUnDirEdge _ -> []
            Graph.EDirEdge de ->
               case Idx.InPart sec (Topo.structureEdgeFromDirEdge de) of
                  edge ->
                     formatEnergy edge :
                     formatX edge :
                     formatEta edge :
                     formatX (Idx.flip edge) :
                     formatEnergy (Idx.flip edge) :
                     []

  in  if optEtaNode opts
        then ShowEtaNode structEShowEta
        else HideEtaNode structEShow


cumulatedFlow ::
  (FormatValue a, Node.C node) =>
  Topo.Topology node ->
  Cum.EnergyMap node a ->
  DotGraph T.Text
cumulatedFlow g env =
   dotDirGraph $
      DotStmts {
        attrStmts = [],
        subGraphs = [],
        nodeStmts = map dotFromTopoNode $ Graph.labNodes g,
        edgeStmts = map (dotFromCumEdge env) $ Graph.labEdges g
      }

dotFromCumEdge ::
  (FormatValue a, Node.C node) =>
   Cum.EnergyMap node a ->
   Graph.LEdge Graph.DirEdge node () -> DotEdge T.Text
dotFromCumEdge env (e, ()) =
   DotEdge
      (dotIdentFromNode x) (dotIdentFromNode y)
      [displabel, Viz.Dir dir, structureEdgeColour]
  where (de@(DirEdge x y), dir, _) = orientDirEdge e
        se = Topo.structureEdgeFromDirEdge de
        displabel =
           labelFromLines $
              formatEner se :
              formatEner (Idx.flip se) :
              []
        formatEner idx =
           Format.assign Format.energy
              (maybe
                  (error $ "could not find cumulated energy index")
                  formatValue $
               Map.lookup (Idx.Energy idx) env)


dotDirGraph :: DotStatements str -> DotGraph str
dotDirGraph stmts =
  DotGraph {
    strictGraph = False,
    directedGraph = True,
    graphID = Just (Int 1),
    graphStatements = stmts
  }
