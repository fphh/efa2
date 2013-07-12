module EFA.Graph.Draw (
  pdf, png, xterm,
  eps, plain, svg,
  fig, dot,
  title, bgcolour,

  sequFlowGraph,
  sequFlowGraphWithEnv,
  sequFlowGraphAbsWithEnv,
  sequFlowGraphDeltaWithEnv,

  Options, optionsDefault,
  absoluteVariable, deltaVariable,
  showVariableIndex, hideVariableIndex,
  showStorageEdge, hideStorageEdge,

  cumulatedFlow,
  topologyWithEdgeLabels,
  topology,
  flowTopologies,
  ) where

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Format (Format, Unicode(Unicode, unUnicode))

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Variable as Var

import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Signal (SignalIdx(SignalIdx))

import qualified EFA.Example.Index as XIdx
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph.CumulatedFlow as Cum
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr
import EFA.Graph.Topology (NodeType(Storage), FlowTopology)
import EFA.Graph (DirEdge(DirEdge), labNodes)

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

import Data.GraphViz.Attributes.Complete as Viz
import qualified Data.GraphViz.Attributes.Colors as Colors
import qualified Data.GraphViz.Attributes.Colors.X11 as X11Colors

import qualified Data.Accessor.Basic as Accessor

import qualified Data.Text.Lazy as T

import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.List.HT as ListHT

import Data.Map (Map)
import Data.Foldable (foldMap, fold)
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (mapFst, mapFst3, mapPair)

import Control.Monad (void)
import Control.Category ((.))

import Prelude hiding ((.))



structureEdgeColour :: Attribute
structureEdgeColour = Color [RGB 0 0 200]

storageEdgeColour :: Attribute
storageEdgeColour = Color [RGB 200 0 0]

shape :: NodeType a -> Shape
shape Topo.Crossing = PlainText
shape Topo.Source = DiamondShape
shape Topo.AlwaysSource = MDiamond
shape Topo.Sink = BoxShape
shape Topo.AlwaysSink = MSquare
shape (Topo.Storage _) = Ellipse
shape _ = BoxShape

color :: NodeType a -> Attribute
color (Topo.Storage _) = FillColor [RGB 251 177 97] -- ghlightorange
color _ = FillColor [RGB 136 215 251]  -- ghverylightblue

mkNodeAttrs :: NodeType a -> Attribute -> [Attribute]
mkNodeAttrs nodeType label =
  [ label, Style [SItem Filled []], Shape (shape nodeType), color nodeType ]


dotFromSequFlowGraph ::
  (Node.C node) =>
  Flow.RangeGraph node ->
  Maybe (Idx.Section -> Unicode) ->
  (Maybe Idx.Boundary -> Topo.LDirNode node -> Unicode) ->
  (Idx.InSection Gr.EitherEdge node -> [Unicode]) ->
  Maybe (Idx.ForNode Idx.StorageEdge node -> [Unicode]) ->
  DotGraph T.Text
dotFromSequFlowGraph (rngs, g) mtshow nshow structureEdgeShow storageEdgeShow =
  DotGraph { strictGraph = False,
             directedGraph = True,
             graphID = Just (Int 1),
             graphStatements = stmts }

  where (topoEs, interEs) =
           mapFst (Map.fromListWith (++)) $
           ListHT.unzipEithers $
           map
              (\e ->
                 case Topo.edgeType e of
                    Topo.StructureEdge se@(Idx.InSection s _) ->
                       Left (Idx.augmentSection s, [se])
                    Topo.StorageEdge se -> Right se) $
           Gr.edges g

        topoNs =
           Map.fromListWith (++) $
           map (\nl@(Idx.TimeNode s _, _) -> (s, [nl])) $
           Gr.labNodes g

        stmts =
          DotStmts {
            attrStmts = [],
            subGraphs =
              zipWith
                (dotFromSectionGraph rngs mtshow nshow structureEdgeShow)
                (Nothing :
                 map (assertJust . Idx.boundaryFromAugSection)
                    (Map.keys topoNs)) $
              Map.toAscList $
              TMap.intersectionPartialWith (,) (TMap.cons [] topoEs) topoNs,
            nodeStmts = [],
            edgeStmts =
              flip foldMap storageEdgeShow $ \eshow ->
                map (dotFromStorageEdge eshow) interEs
          }

        assertJust (Just y) = Just y
        assertJust Nothing =
           error "Exit section cannot predecessor of another section"

dotFromSectionGraph ::
  (Node.C node) =>
  Map Idx.Section SD.Range ->
  Maybe (Idx.Section -> Unicode) ->
  (Maybe Idx.Boundary -> Topo.StNode store node -> Unicode) ->
  (Idx.InSection Gr.EitherEdge node -> [Unicode]) ->
  Maybe Idx.Boundary ->
  (Idx.AugmentedSection,
   ([Idx.InSection Gr.EitherEdge node],
    [Topo.StNode store node])) ->
  DotSubGraph T.Text
dotFromSectionGraph rngs mtshow nshow structureEdgeShow
  before (current, (es, ns)) =
    DotSG True (Just $ Str $ T.pack $ dotIdentFromAugSection current) $
    DotStmts
      [GraphAttrs [Label (StrLabel (T.pack str))]]
      []
      (map (dotFromSecNode (nshow before)) ns)
      (map (dotFromStructureEdge structureEdgeShow) es)
  where str =
           case current of
              Idx.Init -> "Init"
              Idx.NoInit Idx.Exit -> "Exit"
              Idx.NoInit (Idx.NoExit s) ->
                 show s ++
                 (case Map.lookup s rngs of
                     Just (SignalIdx from, SignalIdx to) ->
                        " / Range " ++ show from ++ "-" ++ show to
                     Nothing -> error $ "missing range for " ++ show s) ++
                 (flip foldMap mtshow $ \tshow ->
                    " / Time " ++ unUnicode (tshow s))


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
   setGlobalAttrs $ GraphAttrs [Label (StrLabel (T.pack ti))]

bgcolour :: X11Colors.X11Color -> DotGraph T.Text -> DotGraph T.Text
bgcolour c =
   setGlobalAttrs $ GraphAttrs [BgColor [Colors.X11Color c]]


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


dotFromSecNode ::
  (Node.C node) =>
  (Topo.StNode store node -> Unicode) ->
  Topo.StNode store node -> DotNode T.Text
dotFromSecNode nshow n@(x, nodeType) =
  DotNode (dotIdentFromAugNode x) (mkNodeAttrs nodeType displabel)
  where displabel = Label $ StrLabel $ T.pack $ unUnicode $ nshow n

dotFromStructureEdge ::
  (Node.C node) =>
  (Idx.InSection Gr.EitherEdge node -> [Unicode]) ->
  Idx.InSection Gr.EitherEdge node -> DotEdge T.Text
dotFromStructureEdge eshow e =
   DotEdge
      (dotIdentFromSecNode x) (dotIdentFromSecNode y)
      [labelFromLines $ order $ eshow e,
       Viz.Dir dir, structureEdgeColour]
  where (DirEdge x y, dir, order) = orientFlowEdge e

dotFromStorageEdge ::
  (Node.C node) =>
  (Idx.ForNode Idx.StorageEdge node -> [Unicode]) ->
  Idx.ForNode Idx.StorageEdge node -> DotEdge T.Text
dotFromStorageEdge eshow e =
   DotEdge
      (dotIdentFromAugNode $ Idx.storageEdgeFrom e)
      (dotIdentFromAugNode $ Idx.storageEdgeTo   e)
      [labelFromLines $ eshow e, Viz.Dir Forward,
       storageEdgeColour, Constraint True]

labelFromLines :: [Unicode] -> Attribute
labelFromLines =
   Label . StrLabel . T.pack . L.intercalate "\n" . map unUnicode


dotIdentFromSecNode :: (Node.C node) => Idx.SecNode node -> T.Text
dotIdentFromSecNode (Idx.TimeNode s n) =
   T.pack $ "s" ++ dotIdentFromSection s ++ "n" ++ Node.dotId n

dotIdentFromSection :: Idx.Section -> String
dotIdentFromSection (Idx.Section s) = show s

dotIdentFromAugNode :: (Node.C node) => Idx.AugNode node -> T.Text
dotIdentFromAugNode (Idx.TimeNode b n) =
   T.pack $ "s" ++ dotIdentFromAugSection b ++ "n" ++ Node.dotId n

dotIdentFromAugSection :: Idx.AugmentedSection -> String
dotIdentFromAugSection Idx.Init = "init"
dotIdentFromAugSection (Idx.NoInit Idx.Exit) = "exit"
dotIdentFromAugSection (Idx.NoInit (Idx.NoExit s)) = dotIdentFromSection s

dotIdentFromNode :: (Node.C node) => node -> T.Text
dotIdentFromNode n = T.pack $ Node.dotId n

sequFlowGraph ::
  (Node.C node) =>
  Flow.RangeGraph node -> DotGraph T.Text
sequFlowGraph topo =
  dotFromSequFlowGraph topo Nothing nshow eshow (Just eshow)
  where nshow _before (Idx.TimeNode _ n, l) =
           Unicode $ unUnicode (Node.display n) ++ " - " ++ showType l
        eshow _ = []



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
  DotNode (dotIdentFromNode x) (mkNodeAttrs typ displabel)
  where displabel = Label $ StrLabel $ T.pack $ unUnicode (Node.display x)

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
                   Label (StrLabel lab), EdgeTooltip lab ]



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
dotFromFlowTopology ident topo = DotSG True (Just (Int ident)) stmts
  where stmts = DotStmts attrs [] ns es
        attrs = [GraphAttrs [labelf ident]]
        ns = map mkNode (labNodes topo)
        idf x = T.pack $ show ident ++ "_" ++ Node.dotId x
        labelf x = Label $ StrLabel $ T.pack (show x)
        mkNode x@(n, t) =
          DotNode (idf n) (mkNodeAttrs t (labNodef x))
        labNodef (n, l) =
          Label $ StrLabel $ T.pack $
                  unUnicode (Node.display n) ++ " - " ++ showType l
        es = map mkEdge $ Gr.edges topo
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


orientFlowEdge ::
   (Ord node) =>
   Idx.InSection Gr.EitherEdge node ->
   (DirEdge (Idx.SecNode node), DirType, [s] -> [s])
orientFlowEdge (Idx.InSection sec e) =
   mapFst3
      (\(DirEdge x y) ->
         DirEdge
            (Idx.secNode sec x)
            (Idx.secNode sec y)) $
   case e of
      Gr.EUnDirEdge ue -> (orientUndirEdge ue, NoDir, const [])
      Gr.EDirEdge de -> orientDirEdge de

orientEdge ::
   (Ord node) =>
   Gr.EitherEdge node -> (DirEdge node, DirType, [s] -> [s])
orientEdge e =
   case e of
      Gr.EUnDirEdge ue ->
         (orientUndirEdge ue, NoDir, const [])
      Gr.EDirEdge de -> orientDirEdge de

orientUndirEdge :: Ord node => Gr.UnDirEdge node -> DirEdge node
orientUndirEdge (Gr.UnDirEdge x y) = DirEdge x y

orientDirEdge :: Ord node => DirEdge node -> (DirEdge node, DirType, [s] -> [s])
orientDirEdge (DirEdge x y) =
--   if comparing (\(Idx.SecNode s n) -> n) x y == LT
   if x < y
     then (DirEdge x y, Forward, id)
     else (DirEdge y x, Back, reverse)


class StorageLabel a where
   formatStorageLabel :: a -> String

instance StorageLabel () where
   formatStorageLabel () = ""

instance Show a => StorageLabel (Maybe a) where
   formatStorageLabel Nothing = ""
   formatStorageLabel (Just dir) = " " ++ show dir


showType :: StorageLabel store => NodeType store -> String
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


formatNodeType ::
   (Format output, StorageLabel store) =>
   NodeType store -> output
formatNodeType = Format.literal . showType


data Options output =
   Options {
      optRecordIndex :: output -> output,
      optVariableIndex :: Bool,
      optStorageEdge :: Bool
   }

optionsDefault :: Format output => Options output
optionsDefault =
   Options {
      optRecordIndex = id,
      optVariableIndex = False,
      optStorageEdge = True
   }

absoluteVariable, deltaVariable,
   showVariableIndex, hideVariableIndex,
   showStorageEdge, hideStorageEdge
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


formatNodeStorage ::
   (FormatValue a, Format output, Node.C node) =>
   Options output ->
   Env.StorageMap node a ->
   Env.StInSumMap node a ->
   Env.StOutSumMap node a ->
   Maybe Idx.Boundary -> Topo.LDirNode node -> output
formatNodeStorage opts st sis sos mBeforeBnd (Idx.TimeNode aug nid, ty) =
   Format.lines $
   Node.display nid :
   Format.words [formatNodeType ty] :
      case ty of
         Storage dir ->
            case (aug, mBeforeBnd) of
               (Idx.Init, Nothing) ->
                  [lookupFormat opts sos $ XIdx.stOutSum XIdx.initSection nid]
               (Idx.Init, Just _) ->
                  error "initial section has no predecessor"
               (Idx.NoInit Idx.Exit, Just _) ->
                  [lookupFormat opts sis $ XIdx.stInSum XIdx.exitSection nid]
               (Idx.NoInit (Idx.NoExit sec), Just beforeBnd) ->
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
               (Idx.NoInit _, Nothing) ->
                  error "a true section must have a predecessor"
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
               else Format.edgeIdent $ Format.edgeVar idx)
            (lookupFormat opts mp idx)

sequFlowGraphWithEnv ::
  (FormatValue a, FormatValue v, Node.C node) =>
  Options Unicode -> Flow.RangeGraph node ->
  Env.Complete node a v -> DotGraph T.Text
sequFlowGraphWithEnv opts g
    (Env.Complete (Env.Scalar me st se sx sis sos) (Env.Signal e _p n dt x _s)) =
  dotFromSequFlowGraph g
    (Just formatTime) formatNode structEShow
    (toMaybe (optStorageEdge opts) storeEShow)
  where formatEnergy =
           lookupFormatAssign opts e $ Idx.liftInSection Idx.Energy
        formatX =
           lookupFormatAssign opts x $ Idx.liftInSection Idx.X
        formatEta =
           lookupFormatAssign opts n $ Idx.liftInSection Idx.Eta
        formatMaxEnergy =
           lookupFormatAssign opts me $ Idx.liftForNode Idx.MaxEnergy
        formatStEnergy =
           lookupFormatAssign opts se $ Idx.liftForNode Idx.StEnergy
        formatStX =
           lookupFormatAssign opts sx $ Idx.liftForNode Idx.StX
        formatTime =
           lookupFormat opts dt . flip Idx.InSection Idx.DTime
        formatNode =
           formatNodeStorage opts st sis sos
        structEShow (Idx.InSection sec ee) =
           case ee of
              Gr.EUnDirEdge _ -> []
              Gr.EDirEdge (Gr.DirEdge from to) ->
                 case Idx.InSection sec (Idx.StructureEdge from to) of
                    edge ->
                       formatEnergy edge :
                       formatX edge :
                       formatEta edge :
                       formatX (Idx.flip edge) :
                       formatEnergy (Idx.flip edge) :
                       []
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


cumulatedFlow ::
  (FormatValue a, Node.C node) =>
  Topo.Topology node ->
  Cum.EnergyMap node a ->
  DotGraph T.Text
cumulatedFlow g env =
  DotGraph {
    strictGraph = False,
    directedGraph = True,
    graphID = Just (Int 1),
    graphStatements =
      DotStmts {
        attrStmts = [],
        subGraphs = [],
        nodeStmts = map dotFromTopoNode $ Gr.labNodes g,
        edgeStmts = map (dotFromCumEdge env) $ Gr.labEdges g
      }
  }

dotFromCumEdge ::
  (FormatValue a, Node.C node) =>
   Cum.EnergyMap node a ->
   Gr.LEdge Gr.DirEdge node () -> DotEdge T.Text
dotFromCumEdge env (e, ()) =
   DotEdge
      (dotIdentFromNode x) (dotIdentFromNode y)
      [displabel, Viz.Dir dir, structureEdgeColour]
  where (DirEdge x y, dir, _order) = orientDirEdge e
        displabel =
           Label $ StrLabel $ T.pack $
           L.intercalate "\n" $ map unUnicode $
              formatEner (Idx.StructureEdge x y) :
              formatEner (Idx.StructureEdge y x) :
              []
        formatEner idx =
           Format.assign
              (Format.edgeIdent Format.Energy)
              (maybe
                  (error $ "could not find cumulated energy index")
                  formatValue $
               Map.lookup (Idx.Energy idx) env)
