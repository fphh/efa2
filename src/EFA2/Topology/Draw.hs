-- these extension are needed for computing Eta in envAbsSignal, envDeltaSignal
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module EFA2.Topology.Draw where

import EFA2.Solver.Equation
          (Term(..), ToIndex, simplify, (&-), (&/),
           showEqTerm, showSecNode,
           LatexString(LatexString), unLatexString,
           toLatexString, secNodeToLatexString)
import EFA2.Interpreter.Env
          (StorageMap, SingleRecord(SingleRecord))
import qualified EFA2.Interpreter.Env as Interp
import EFA2.Topology.TopologyData as Topo
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

import Text.Printf (PrintfArg, printf)


nodeColour :: Attribute
nodeColour = FillColor [RGB 230 230 240]

clusterColour :: Attribute
clusterColour = FillColor [RGB 250 250 200]

originalEdgeColour :: Attribute
originalEdgeColour = Color [RGB 0 0 200]

intersectionEdgeColour :: Attribute
intersectionEdgeColour = Color [RGB 200 0 0]

-- coding
noRecord :: Maybe Idx.Record
noRecord = Nothing


mkDotGraph ::
   SequFlowGraph ->
   Maybe Idx.Record ->
   (Idx.DTime -> String) ->
   (Topo.LNode -> String) ->
   (Topo.LEdge -> [String]) ->
   DotGraph T.Text
mkDotGraph g recordNum timef nshow eshow =
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
                   case recordNum of
                      Nothing -> "NoRecord"
                      Just n ->
                         show n ++ " / Time " ++ timef (Idx.DTime n sl)
        stmts = DotStmts { attrStmts = [],
                           subGraphs = map sg cs,
                           nodeStmts = [],
                           edgeStmts = map (mkDotEdge eshow) $ M.toList interEs }


mkDotNode:: (Topo.LNode -> String) -> Topo.LNode -> DotNode T.Text
mkDotNode nshow n@(x, _) =
   DotNode (dotIdentFromSecNode x)
      [displabel, nodeColour, Style [SItem Filled []], Shape BoxShape ]
  where displabel =  Label $ StrLabel $ T.pack (nshow n)

mkDotEdge :: (Topo.LEdge -> [String]) -> Topo.LEdge -> DotEdge T.Text
mkDotEdge eshow e@(_, elabel) =
   DotEdge
      (dotIdentFromSecNode x) (dotIdentFromSecNode y)
      [displabel, Viz.Dir dir, colour]
  where (Edge x y, dir, order) = orientEdge e
        displabel =
           Label $ StrLabel $ T.pack $ L.intercalate "\n" $ order $ eshow e
        colour =
           case edgeType elabel of
              IntersectionEdge -> intersectionEdgeColour
              _ -> originalEdgeColour
        --colour = originalEdgeColour

dotIdentFromSecNode :: Idx.SecNode -> T.Text
dotIdentFromSecNode (Idx.SecNode (Idx.Section s) (Idx.Node n)) =
   T.pack $ "s" ++ show s ++ "n" ++ show n

printGraph ::
   SequFlowGraph ->
   Maybe Idx.Record ->
   (Idx.DTime -> String) ->
   (Topo.LNode -> String) ->
   (Topo.LEdge -> [String]) ->
   IO ()
printGraph g recordNum tshow nshow eshow =
   runGraphvizCanvas Dot (mkDotGraph g recordNum tshow nshow eshow) Xlib
{-
printGraph g recordNum tshow nshow eshow = do
  runGraphvizCommand Dot (mkDotGraph g recordNum tshow nshow eshow) XDot "result/graph.dot"
  return ()
-}

heart :: Char
heart = '\9829'

drawTopologyX' :: SequFlowGraph -> IO ()
drawTopologyX' topo =
   printGraph topo noRecord (const [heart]) show ((:[]) . show)


drawTopologySimple :: SequFlowGraph -> IO ()
drawTopologySimple topo =
   printGraph topo noRecord (const [heart]) nshow eshow
  where nshow (Idx.SecNode _ n, l) = show n ++ " - " ++ showNodeType l
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



class Format output where
   undetermined :: output
   formatLineAbs :: Line -> output
   formatLineDelta :: Line -> output
   formatAssignGen :: output -> output -> output
   formatList :: [output] -> output
   formatTerm :: ToIndex idx => Term idx -> output
   formatChar :: Char -> output
   formatRatio :: (Integral a, Show a) => Ratio a -> output
   formatReal :: (Floating a, PrintfArg a) => a -> output
   formatQuotient :: output -> output -> output
   formatSignal ::
      (SDisplay v, D.Storage v a, Ord a, Disp a,
       TDisp t, DispApp s) =>
      TC s t (Data v a) -> output
   formatNode ::
      (FormatValue a) =>
      Idx.Record -> StorageMap a -> Topo.LNode -> output


newtype Plain = Plain {getPlain :: String}
   deriving (Show)

instance Format Plain where
   undetermined = Plain [heart]
   formatLineAbs = formatLine ""
   formatLineDelta = formatLine "d"
   formatAssignGen (Plain lhs) (Plain rhs) =
      Plain $ lhs ++ " = " ++ rhs
   formatList = Plain . ("["++) . ("]"++) . L.intercalate "," . map getPlain
   formatTerm = Plain . showEqTerm
   formatChar = Plain . (:[])
   formatRatio = Plain . show
   -- formatReal = Plain . show
   formatReal = Plain . printf "%.6f"
   formatQuotient (Plain x) (Plain y) = Plain $ "(" ++ x ++ ")/(" ++ y ++ ")"
   formatSignal = Plain . sdisp
   formatNode rec st (n@(Idx.SecNode _sec nid), ty) =
      Plain $
      show nid ++ "\n" ++
      "Type: " ++ showNodeType ty ++
         case ty of
            Storage ->
               "\nContent: " ++
               (getPlain $ formatMaybe formatValue $
                M.lookup (Idx.Storage rec n) st)
            _ -> ""


formatLine :: String -> Line -> Plain
formatLine prefix (Line t u v) =
   Plain $
   prefix ++ lineTypeLetter t : "_" ++ showSecNode u ++ "_" ++ showSecNode v

instance Format LatexString where
   undetermined = LatexString "\\heartsuit "
   formatLineAbs = formatLineLatex ""
   formatLineDelta = formatLineLatex "\\Delta "
   formatAssignGen (LatexString lhs) (LatexString rhs) =
      LatexString $ lhs ++ " = " ++ rhs
   formatList = LatexString . ("["++) . ("]"++) . L.intercalate ", " . map unLatexString
   formatTerm = toLatexString
   formatChar = LatexString . (:[])
   formatRatio = LatexString . show
   formatReal = LatexString . printf "%f"
   formatQuotient (LatexString x) (LatexString y) =
      LatexString $ "\\frac{" ++ x ++ "}{" ++ y ++ "}"
   formatSignal = LatexString . sdisp
   formatNode rec st (n@(Idx.SecNode _sec nid), ty) =
      LatexString $
      show nid ++ "\\\\ " ++
      "Type: " ++ showNodeType ty ++
         case ty of
            Storage ->
               "\\\\ Content: " ++
               (unLatexString $ formatMaybe formatValue $
                M.lookup (Idx.Storage rec n) st)
            _ -> ""



formatLineLatex :: String -> Line -> LatexString
formatLineLatex prefix (Line t u v) =
   LatexString $
   prefix ++ lineTypeLetter t : "_{" ++
   secNodeToLatexString u ++ "." ++ secNodeToLatexString v ++
   "}"


class FormatValue a where
   formatValue :: Format output => a -> output



data Env output =
   Env {
      recordNumber :: Idx.Record,
      formatEnergy_ :: Idx.SecNode -> Idx.SecNode -> output,
      formatX_      :: Idx.SecNode -> Idx.SecNode -> output,
      formatEta_    :: Idx.SecNode -> Idx.SecNode -> output,
      formatAssign_ :: (Line, output) -> output,
      showTime :: Idx.DTime -> output,
      showNode_ :: Topo.LNode -> output
   }

makeLookup ::
   (Ord idx) =>
   Idx.Record ->
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) -> M.Map idx a ->
   Idx.SecNode -> Idx.SecNode -> Maybe a
makeLookup rec makeIdx mp =
   \uid vid -> M.lookup (makeIdx rec uid vid) mp

makeFormat ::
   (Ord idx, FormatValue a, Format output) =>
   Idx.Record ->
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) -> M.Map idx a ->
   Idx.SecNode -> Idx.SecNode -> output
makeFormat rec makeIdx mp =
   \uid vid -> formatMaybeValue $ makeLookup rec makeIdx mp uid vid

formatMaybe :: Format output => (a -> output) -> Maybe a -> output
formatMaybe = maybe undetermined

formatMaybeValue :: (FormatValue a, Format output) => Maybe a -> output
formatMaybeValue = formatMaybe formatValue

lookupFormat ::
   (Ord idx, Show idx, Format output) =>
   (a -> output) -> M.Map idx a -> idx -> output
lookupFormat format dt k =
   formatMaybe format $ M.lookup k dt


draw :: SequFlowGraph -> Env Plain -> IO ()
draw g
   (Env rec formatEnergy formatX formatEta formatAssign tshow nshow) =
      printGraph g (Just rec)
         (getPlain . tshow) (getPlain . nshow) (map getPlain . eshow)
  where eshow (Edge uid vid, l) =
           map formatAssign $
           case edgeType l of
              OriginalEdge ->
                 (Line ELine uid vid, formatEnergy uid vid) :
                 (Line XLine uid vid, formatX uid vid) :
                 (Line NLine uid vid, formatEta uid vid) :
                 (Line XLine vid uid, formatX vid uid) :
                 (Line ELine vid uid, formatEnergy vid uid) :
                 []
              InnerStorageEdge ->
                 (Line ELine vid uid, formatEnergy vid uid) :
                 []
              IntersectionEdge ->
                 (Line ELine uid vid, formatEnergy uid vid) :
                 (Line XLine uid vid, formatX uid vid) :
                 (Line ELine vid uid, formatEnergy vid uid) :
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
   let lookupEnergy = makeLookup rec Idx.Energy e
   in  Env rec
          (\a b -> formatMaybeValue $ lookupEnergy a b)
          (makeFormat rec Idx.X x)
          (\a b ->
             fromMaybe undetermined $
             liftM2 formatEnergyQuotient
                (lookupEnergy b a)
                (lookupEnergy a b))
          (\(v, ys) -> formatAssignGen (formatLineAbs v) ys)
          (lookupFormat formatValue dt)
          (formatNode rec st)

envDelta ::
   (AutoEnvDelta a, Format output) =>
   Interp.Envs SingleRecord a -> Env output
envDelta
      (Interp.Envs (SingleRecord rec) e de _p _dp _fn _dn dt _x dx _v st) =
   let lookupEnergy = makeLookup rec Idx.Energy e
       lookupDEnergy = makeLookup rec Idx.DEnergy de
   in  Env rec
          (\a b -> formatMaybeValue $ lookupDEnergy a b)
          (makeFormat rec Idx.DX dx)
          (\a b ->
             fromMaybe undetermined $
             liftM4 formatDEnergyQuotient
                (lookupEnergy b a) (lookupEnergy a b)
                (lookupDEnergy b a) (lookupDEnergy a b))
          (\(x, ys) -> formatAssignGen (formatLineDelta x) ys)
          (lookupFormat formatValue dt)
          (formatNode rec st)


class FormatValue a => AutoEnv a where
   formatEnergyQuotient :: Format output => a -> a -> output

class AutoEnv a => AutoEnvDelta a where
   formatDEnergyQuotient :: Format output => a -> a -> a -> a -> output


instance FormatValue a => FormatValue [a] where
   formatValue = formatList . map formatValue

instance AutoEnv a => AutoEnv [a] where
   formatEnergyQuotient xs ys =
      formatList $
      zipWith formatEnergyQuotient xs ys


instance AutoEnvDelta a => AutoEnvDelta [a] where
   formatDEnergyQuotient xs ys dxs dys =
      formatList $
      L.zipWith4 formatDEnergyQuotient xs ys dxs dys


instance FormatValue Double where
   formatValue = formatReal

instance AutoEnv Double where
   formatEnergyQuotient x y = formatReal $ x/y

instance AutoEnvDelta Double where
   formatDEnergyQuotient ea eb dea deb =
      formatReal $
      (dea*eb - ea*deb)/((eb+deb)*eb)
{-
      (ea+dea)/(eb+deb) - ea/eb
-}

instance (Integral a, Show a) => FormatValue (Ratio a) where
   formatValue = formatRatio

instance (Integral a, Show a) => AutoEnv (Ratio a) where
   formatEnergyQuotient x y = formatRatio $ x/y

instance FormatValue Char where
   formatValue = formatChar

instance AutoEnv Char where
   formatEnergyQuotient x y = formatQuotient (formatChar x) (formatChar y)


instance (Eq a, ToIndex a) => FormatValue (Term a) where
   formatValue = formatTerm

instance (Eq a, ToIndex a) => AutoEnv (Term a) where
   formatEnergyQuotient x y = formatTerm $ simplify $ x &/ y

instance (Eq a, ToIndex a) => AutoEnvDelta (Term a) where
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
