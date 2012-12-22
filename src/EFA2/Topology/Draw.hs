-- these extension are needed for computing Eta in envAbsSignal, envDeltaSignal
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module EFA2.Topology.Draw where

import EFA2.Solver.Equation
          (Term(..), ToIndex, simplify, (&-), (&/), formatTerm)
import qualified EFA2.Report.Format as Format
import EFA2.Report.Format (Plain(Plain, unPlain), deltaChar, heartChar)
import EFA2.Interpreter.Env
          (StorageMap, SingleRecord(SingleRecord))
import qualified EFA2.Interpreter.Env as Interp
import qualified EFA2.Topology.TopologyData as Topo
import EFA2.Topology.TopologyData
          (SequFlowGraph,
           NodeType(Storage),
           EdgeType(OriginalEdge, IntersectionEdge,  InnerStorageEdge),
           edgeType,
           isIntersectionEdge,
           getFlowDirection,
           FlowDirectionField, FlowTopology)
import EFA2.Topology.EfaGraph
          (EfaGraph, Edge(Edge),
           labNodes, labEdges, edgeLabels, delNodes, delEdgeSet)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Data as D
import EFA2.Report.Signal (SDisplay, sdisp)
import EFA2.Report.Typ (TDisp)
import EFA2.Report.Base (Disp)
import EFA2.Signal.Signal (TC, DispApp, Arith, (.-), (.+), (./), (.*))
import EFA2.Signal.Data (Data)
import EFA2.Signal.Base (BSum, BProd)
import EFA2.Signal.Typ (TSum, TProd)

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
import Data.Ratio (Ratio)
import Data.Maybe (fromMaybe)

import qualified Data.Text.Lazy as T

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.HT as HTL
import qualified Data.NonEmpty as NonEmpty
import qualified Data.NonEmpty.Mixed as NonEmptyM

import Control.Concurrent.MVar (MVar, putMVar, readMVar, newEmptyMVar)
import Control.Concurrent (forkIO)
import Control.Monad ((>=>), void, liftM2, liftM4)


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
   Maybe (Plain, Idx.Section -> Plain) ->
   (Topo.LNode -> Plain) ->
   (Topo.LEdge -> [Plain]) ->
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
                      Just (Plain n, timef) ->
                         n ++ " / Time " ++ unPlain (timef sl)
        stmts = DotStmts { attrStmts = [],
                           subGraphs = map sg cs,
                           nodeStmts = [],
                           edgeStmts = map (mkDotEdge eshow) $ M.toList interEs }


mkDotNode:: (Topo.LNode -> Plain) -> Topo.LNode -> DotNode T.Text
mkDotNode nshow n@(x, _) =
   DotNode (dotIdentFromSecNode x)
      [displabel, nodeColour, Style [SItem Filled []], Shape BoxShape ]
  where displabel = Label $ StrLabel $ T.pack $ unPlain $ nshow n

mkDotEdge :: (Topo.LEdge -> [Plain]) -> Topo.LEdge -> DotEdge T.Text
mkDotEdge eshow e@(_, elabel) =
   DotEdge
      (dotIdentFromSecNode x) (dotIdentFromSecNode y)
      [displabel, Viz.Dir dir, colour]
  where (Edge x y, dir, order) = orientEdge e
        displabel =
           Label $ StrLabel $ T.pack $
           L.intercalate "\n" $ map unPlain $ order $ eshow e
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
   Maybe (Plain, Idx.Section -> Plain) ->
   (Topo.LNode -> Plain) ->
   (Topo.LEdge -> [Plain]) ->
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
   printGraph topo Nothing (Plain . show) ((:[]) . Plain . show)


drawTopologySimple :: SequFlowGraph -> IO ()
drawTopologySimple topo =
   printGraph topo Nothing nshow eshow
  where nshow (Idx.SecNode _ n, l) = Plain $ show n ++ " - " ++ showNodeType l
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
orientEdge (e@(Edge x y), l) =
   case getFlowDirection l of
      Topo.UnDir -> (e, NoDir, const [])
      Topo.Dir ->
--         if comparing (\(Idx.SecNode s n) -> n) x y == LT
         if x < y
           then (e, Forward, id)
           else (Edge y x, Back, reverse)

data LineType = ELine | XLine | NLine
   deriving (Eq, Ord, Show, Enum)

lineTypeLetter :: LineType -> Char
lineTypeLetter ELine = 'e'
lineTypeLetter XLine = 'x'
lineTypeLetter NLine = 'n'

data Line = Line LineType Idx.SecNode Idx.SecNode
   deriving (Eq, Ord)


showNodeType :: NodeType -> String
showNodeType = show



class Format.Format output => Format output where
   undetermined :: output
   formatLineAbs :: Line -> output
   formatLineDelta :: Line -> output
   formatRecord :: Idx.Record -> output
   formatAssign :: output -> output -> output
   formatChar :: Char -> output
   formatQuotient :: output -> output -> output
   formatSignal ::
      (SDisplay v, D.Storage v a, Ord a, Disp a,
       TDisp t, DispApp s) =>
      TC s t (Data v a) -> output
   formatNode ::
      (FormatValue a) =>
      Idx.Record -> StorageMap a -> Topo.LNode -> output


instance Format Plain where
   undetermined = Plain [heartChar]
   formatLineAbs = formatLine ""
   formatLineDelta = formatLine [deltaChar]
   formatRecord = Plain . show
   formatAssign (Plain lhs) (Plain rhs) =
      Plain $ lhs ++ " = " ++ rhs
   formatChar = Plain . (:[])
   formatQuotient (Plain x) (Plain y) = Plain $ "(" ++ x ++ ")/(" ++ y ++ ")"
   formatSignal = Plain . sdisp
   formatNode rec st (n@(Idx.SecNode _sec nid), ty) =
      Plain $
      show nid ++ "\n" ++
      "Type: " ++ showNodeType ty ++
         case ty of
            Storage ->
               "\nContent: " ++
               (unPlain $ lookupFormat st (Idx.Storage rec n))
            _ -> ""


formatLine :: String -> Line -> Plain
formatLine prefix (Line t u v) =
   Format.subscript
      (Plain $ prefix ++ lineTypeLetter t : "")
      (Format.sectionNode u
       `Format.connect`
       Format.sectionNode v)

instance Format Format.Latex where
   undetermined = Format.Latex "\\heartsuit "
   formatLineAbs = formatLineLatex ""
   formatLineDelta = formatLineLatex "\\Delta "
   formatRecord = Format.Latex . show
   formatAssign (Format.Latex lhs) (Format.Latex rhs) =
      Format.Latex $ lhs ++ " = " ++ rhs
   formatChar = Format.Latex . (:[])
   formatQuotient (Format.Latex x) (Format.Latex y) =
      Format.Latex $ "\\frac{" ++ x ++ "}{" ++ y ++ "}"
   formatSignal = Format.Latex . sdisp
   formatNode rec st (n@(Idx.SecNode _sec nid), ty) =
      Format.Latex $
      show nid ++ "\\\\ " ++
      "Type: " ++ showNodeType ty ++
         case ty of
            Storage ->
               "\\\\ Content: " ++
               (Format.unLatex $ lookupFormat st (Idx.Storage rec n))
            _ -> ""



formatLineLatex :: String -> Line -> Format.Latex
formatLineLatex prefix (Line t u v) =
   Format.subscript
      (Format.Latex $ prefix ++ lineTypeLetter t : "")
      (Format.sectionNode u
       `Format.connect`
       Format.sectionNode v)


class FormatValue a where
   formatValue :: Format output => a -> output


{- |
The 'Env' shall contain only values and functions for display.
It shall not contain values needed for computations.
-}
data Env output =
   Env {
      recordNumber_ :: output,
      formatEnergy_ :: Idx.SecNode -> Idx.SecNode -> output,
      formatX_      :: Idx.SecNode -> Idx.SecNode -> output,
      formatEta_    :: Idx.SecNode -> Idx.SecNode -> output,
      formatTime_ :: Idx.Section -> output,
      formatNode_ :: Topo.LNode -> output
   }

formatMaybeValue :: (FormatValue a, Format output) => Maybe a -> output
formatMaybeValue = maybe undetermined formatValue

lookupFormat ::
   (Ord idx, Show idx, FormatValue a, Format output) =>
   M.Map idx a -> idx -> output
lookupFormat dt k =
   formatMaybeValue $ M.lookup k dt

formatAssignAbs ::
   (Format output) =>
   LineType ->
   (Idx.SecNode -> Idx.SecNode -> output) ->
   (Idx.SecNode -> Idx.SecNode -> output)
formatAssignAbs lt lookupEdge x y =
   formatAssign (formatLineAbs $ Line lt x y) (lookupEdge x y)

formatAssignDelta ::
   (Format output) =>
   LineType ->
   (Idx.SecNode -> Idx.SecNode -> output) ->
   (Idx.SecNode -> Idx.SecNode -> output)
formatAssignDelta lt lookupEdge x y =
   formatAssign (formatLineDelta $ Line lt x y) (lookupEdge x y)


draw :: SequFlowGraph -> Env Plain -> IO ()
draw g
   (Env rec formatEnergy formatX formatEta tshow nshow) =
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
              InnerStorageEdge ->
                 formatEnergy vid uid :
                 []
              IntersectionEdge ->
                 formatEnergy uid vid :
                 formatX uid vid :
                 formatEnergy vid uid :
                 []

drawTopology ::
   AutoEnv a => SequFlowGraph -> Interp.Envs SingleRecord a -> IO ()
drawTopology topo = draw topo . envAbs

drawDeltaTopology ::
   AutoEnvDelta a => SequFlowGraph -> Interp.Envs SingleRecord a -> IO ()
drawDeltaTopology topo = draw topo . envDelta


envAbs ::
   (AutoEnv a, Format output) =>
   Interp.Envs SingleRecord a -> Env output
envAbs (Interp.Envs (SingleRecord rec) e _de _p _dp _fn _dn dt x _dx _v st) =
   let lookupEnergy a b = M.lookup (Idx.Energy rec a b) e
   in  Env
          (formatRecord rec)
          (formatAssignAbs ELine $
           \a b -> formatMaybeValue $ lookupEnergy a b)
          (formatAssignAbs XLine $
           \a b -> lookupFormat x (Idx.X rec a b))
          (formatAssignAbs NLine $
           \a b ->
             fromMaybe undetermined $
             liftM2 formatEnergyQuotient
                (lookupEnergy b a)
                (lookupEnergy a b))
          (lookupFormat dt . Idx.DTime rec)
          (formatNode rec st)

envDelta ::
   (AutoEnvDelta a, Format output) =>
   Interp.Envs SingleRecord a -> Env output
envDelta
      (Interp.Envs (SingleRecord rec) e de _p _dp _fn _dn dt _x dx _v st) =
   let lookupEnergy a b = M.lookup (Idx.Energy rec a b) e
       lookupDEnergy a b = M.lookup (Idx.DEnergy rec a b) de
   in  Env
          (formatRecord rec)
          (formatAssignDelta ELine $
           \a b -> formatMaybeValue $ lookupDEnergy a b)
          (formatAssignDelta XLine $
           \a b -> lookupFormat dx (Idx.DX rec a b))
          (formatAssignDelta NLine $
           \a b ->
             fromMaybe undetermined $
             liftM4 formatDEnergyQuotient
                (lookupEnergy b a) (lookupEnergy a b)
                (lookupDEnergy b a) (lookupDEnergy a b))
          (lookupFormat dt . Idx.DTime rec)
          (formatNode rec st)


class FormatValue a => AutoEnv a where
   formatEnergyQuotient :: Format output => a -> a -> output

class AutoEnv a => AutoEnvDelta a where
   formatDEnergyQuotient :: Format output => a -> a -> a -> a -> output


instance FormatValue a => FormatValue [a] where
   formatValue = Format.list . map formatValue

instance AutoEnv a => AutoEnv [a] where
   formatEnergyQuotient xs ys =
      Format.list $
      zipWith formatEnergyQuotient xs ys


instance AutoEnvDelta a => AutoEnvDelta [a] where
   formatDEnergyQuotient xs ys dxs dys =
      Format.list $
      L.zipWith4 formatDEnergyQuotient xs ys dxs dys


instance FormatValue Double where
   formatValue = Format.real

instance AutoEnv Double where
   formatEnergyQuotient x y = Format.real $ x/y

instance AutoEnvDelta Double where
   formatDEnergyQuotient ea eb dea deb =
      Format.real $
      (dea*eb - ea*deb)/((eb+deb)*eb)
{-
      (ea+dea)/(eb+deb) - ea/eb
-}

instance (Integral a, Show a) => FormatValue (Ratio a) where
   formatValue = Format.ratio

instance (Integral a, Show a) => AutoEnv (Ratio a) where
   formatEnergyQuotient x y = Format.ratio $ x/y

instance FormatValue Char where
   formatValue = formatChar

instance AutoEnv Char where
   formatEnergyQuotient x y = formatQuotient (formatChar x) (formatChar y)


instance (Eq a, ToIndex a) => FormatValue (Term a) where
   formatValue = formatTerm

instance (Ord a, ToIndex a) => AutoEnv (Term a) where
   formatEnergyQuotient x y = formatTerm $ simplify $ x &/ y

instance (Ord a, ToIndex a) => AutoEnvDelta (Term a) where
   formatDEnergyQuotient ea eb dea deb =
      formatTerm $ simplify $
      (dea :* eb  &-  ea :* deb) &/ ((eb:+deb):*eb)


class FormatValueSignal a where
   formatValueSignal ::
      (DispApp s, TDisp t, Format output) =>
      (TC s t a) -> output

class FormatValueSignal a => AutoEnvSignal a where
   formatEnergyQuotientSignal ::
      (DispApp s, s ~ Arith s s, TDisp t, TProd t t t, Format output) =>
      TC s t a -> TC s t a -> output

instance
   (SDisplay v, D.Storage v a, Disp a, Ord a, BProd a a, D.ZipWith v) =>
      FormatValueSignal (Data v a) where
   formatValueSignal = formatSignal

instance
   (SDisplay v, D.Storage v a, Disp a, Ord a, BProd a a, D.ZipWith v) =>
      AutoEnvSignal (Data v a) where
   formatEnergyQuotientSignal ea eb = formatValue $ ea ./ eb

instance
   (DispApp s, s ~ Arith s s, TDisp t, TProd t t t, FormatValueSignal a) =>
      FormatValue (TC s t a) where
   formatValue = formatValueSignal

instance
   (DispApp s, s ~ Arith s s, TDisp t, TProd t t t, AutoEnvSignal a) =>
      AutoEnv (TC s t a) where
   formatEnergyQuotient = formatEnergyQuotientSignal


class AutoEnvSignal a => AutoEnvDeltaSignal a where
   formatDEnergyQuotientSignal ::
      (DispApp s, s ~ Arith s s, TDisp t, TSum t t t, TProd t t t,
       Format output) =>
      TC s t a -> TC s t a -> TC s t a -> TC s t a -> output

instance
   (SDisplay v, D.Storage v a, Disp a, Ord a, BSum a, BProd a a, D.ZipWith v) =>
      AutoEnvDeltaSignal (Data v a) where
   formatDEnergyQuotientSignal ea eb dea deb =
      formatValue $ (dea.*eb .- ea.*deb)./((eb.+deb).*eb)

instance
   (DispApp s, s ~ Arith s s, TDisp t, TSum t t t, TProd t t t,
    AutoEnvDeltaSignal a) =>
      AutoEnvDelta (TC s t a) where
   formatDEnergyQuotient = formatDEnergyQuotientSignal



-------------------------------------------------------------------------------------------

newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async io = do
  m <- newEmptyMVar
  void $ forkIO $ putMVar m =<< io
  return (Async m)

wait :: Async a -> IO a
wait (Async m) = readMVar m

drawAll :: [IO a] -> IO ()
drawAll = mapM async >=> mapM_ wait
