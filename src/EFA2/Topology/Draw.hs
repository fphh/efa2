module EFA2.Topology.Draw where

import EFA2.Solver.Equation
          (Term(..), ToIndex, simplify, (&-), (&/),
           showEqTerm, showEqTerms, showSecNode,
           LatexString(LatexString), unLatexString, secNodeToLatexString)
import EFA2.Interpreter.Env
          (DTimeMap, StorageMap,
           SingleRecord(SingleRecord))
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
import EFA2.Signal.Signal (TC, DispApp)
import EFA2.Signal.Data (Data)

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

import qualified Data.Text.Lazy as T

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.HT as HTL
import qualified Data.NonEmpty as NonEmpty
import qualified Data.NonEmpty.Mixed as NonEmptyM

import Control.Concurrent.MVar (MVar, putMVar, readMVar, newEmptyMVar)
import Control.Concurrent (forkIO)
import Control.Monad ((>=>), void, liftM2, liftM4)

import Text.Printf (printf)


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

data Line = ELine Idx.SecNode Idx.SecNode
          | XLine Idx.SecNode Idx.SecNode
          | NLine Idx.SecNode Idx.SecNode
          deriving (Eq, Ord)

showLine :: Line -> String
showLine (ELine u v) = "e_" ++ showSecNode u ++ "_" ++ showSecNode v
showLine (XLine u v) = "x_" ++ showSecNode u ++ "_" ++ showSecNode v
showLine (NLine u v) = "n_" ++ showSecNode u ++ "_" ++ showSecNode v

showLineLatex :: Line -> String
showLineLatex (ELine u v) = "$e_{" ++ secNodeToLatexString u ++ "." ++ secNodeToLatexString v ++ "}$"
showLineLatex (XLine u v) = "$x_{" ++ secNodeToLatexString u ++ "." ++ secNodeToLatexString v ++ "}$"
showLineLatex (NLine u v) = "$n_{" ++ secNodeToLatexString u ++ "." ++ secNodeToLatexString v ++ "}$"

showLineDelta :: Line -> String
showLineDelta (ELine u v) = "de_" ++ showSecNode u ++ "_" ++ showSecNode v
showLineDelta (XLine u v) = "dx_" ++ showSecNode u ++ "_" ++ showSecNode v
showLineDelta (NLine u v) = "dn_" ++ showSecNode u ++ "_" ++ showSecNode v


data Env a =
   Env {
      recordNumber :: Idx.Record,
      lookupEnergy_ :: Idx.Record -> Idx.SecNode -> Idx.SecNode -> Maybe a,
      lookupX_      :: Idx.Record -> Idx.SecNode -> Idx.SecNode -> Maybe a,
      lookupEta_    :: Idx.Record -> Idx.SecNode -> Idx.SecNode -> Maybe a,
      formatAssign_ :: (Line, Maybe a) -> String,
      showTime :: Idx.DTime -> String,
      showNode_ :: Topo.LNode -> String
   }

makeLookup ::
   (Ord idx) =>
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) -> M.Map idx a ->
   Idx.Record -> Idx.SecNode -> Idx.SecNode -> Maybe a
makeLookup makeIdx mp =
   \rec uid vid -> M.lookup (makeIdx rec uid vid) mp

lookupFormat ::
   (Ord idx, Show idx) =>
   (a -> String) -> M.Map idx a -> idx -> String
lookupFormat format dt k =
   case M.lookup k dt of
      Nothing -> [heart]
      Just x -> format x


draw :: SequFlowGraph -> Env a -> IO ()
draw g
   (Env rec lookupEnergy lookupX lookupEta formatAssign tshow nshow) =
      printGraph g (Just rec) tshow nshow eshow
  where eshow = map formatAssign . mkLst

        mkLst (Edge uid vid, l) =
           case edgeType l of
              OriginalEdge ->
                 (ELine uid vid, lookupEnergy rec uid vid) :
                 (XLine uid vid, lookupX rec uid vid) :
                 (NLine uid vid, lookupEta rec uid vid) :
                 (XLine vid uid, lookupX rec vid uid) :
                 (ELine vid uid, lookupEnergy rec vid uid) :
                 []
              InnerStorageEdge ->
                 (ELine vid uid, lookupEnergy rec vid uid) :
                 []
              IntersectionEdge ->
                 (ELine uid vid, lookupEnergy rec uid vid) :
                 (XLine uid vid, lookupX rec uid vid) :
                 (ELine vid uid, lookupEnergy rec vid uid) :
                 []

drawTopology ::
   AutoEnv a => SequFlowGraph -> Interp.Envs SingleRecord a -> IO ()
drawTopology topo = draw topo . envAbs

drawDeltaTopology ::
   AutoEnvDelta a => SequFlowGraph -> Interp.Envs SingleRecord a -> IO ()
drawDeltaTopology topo = draw topo . envDelta


class AutoEnv a where
   envAbs :: Interp.Envs SingleRecord a -> Env a

class AutoEnv a => AutoEnvDelta a where
   envDelta :: Interp.Envs SingleRecord a -> Env a


class AutoEnvList a where
   formatStContList :: Maybe [a] -> String
   formatList :: [a] -> String

   formatAssignList :: AutoEnvList a => (Line, Maybe [a]) -> String
   formatAssignList (x, ys) =
      showLine x ++ " = " ++ formatStContList ys

   showListNode ::
      Idx.Record -> StorageMap [a] ->
      (Maybe [a] -> String) ->
      Topo.LNode -> String
   showListNode = showNode

   divideEnergyList :: [a] -> [a] -> [a]

class AutoEnvList a => AutoEnvDeltaList a where
   formatElement :: a -> String
   divideDEnergyList :: [a] -> [a] -> [a] -> [a] -> [a]

instance AutoEnvList a => AutoEnv [a] where
   envAbs (Interp.Envs (SingleRecord r) e _de _p _dp _fn _dn dt x _dx _v st) =
      let lookupEnergy = makeLookup Idx.Energy e
      in  Env r
             lookupEnergy
             (makeLookup Idx.X x)
             (\rec a b ->
                liftM2 divideEnergyList
                   (lookupEnergy rec a b)
                   (lookupEnergy rec b a))
             formatAssignList
             (lookupFormat formatList dt)
             (showListNode r st formatStContList)


instance AutoEnvDeltaList a => AutoEnvDelta [a] where
   envDelta =
      envDeltaArg
         divideDEnergyList
         (\(x, ys) ->
            showLineDelta x ++ " = " ++
            maybe [heart] (concatMap (("\n"++) . formatElement)) ys)
         (\mys ->
            case mys of
               Just ys -> "[ " ++ L.intercalate ", " (map formatElement ys) ++ " ]"
               Nothing -> [heart])
         (lookupFormat formatList)


instance AutoEnvList Double where
   formatStContList (Just ys) = concatMap (printf "%.6f    ") ys
   formatStContList Nothing = [heart]
   formatList = show
   divideEnergyList = zipWith (/)

instance AutoEnvDeltaList Double where
   formatElement = show
   divideDEnergyList =
      L.zipWith4
         (\ea eb dea deb ->
            (dea*eb - ea*deb)/((eb+deb)*eb))
{-
         (\ea eb dea deb ->
            (ea+dea)/(eb+deb) - ea/eb)
-}

instance (Integral a, Show a) => AutoEnvList (Ratio a) where
   formatStContList (Just ys) = unwords $ map show ys
   formatStContList Nothing = [heart]
   formatList = show
   divideEnergyList = zipWith (/)

instance AutoEnvList Char where
   formatStContList (Just ys) = ys
   formatStContList Nothing = "+"
   formatList = id
   divideEnergyList x y = "(" ++ x ++ ")/(" ++ y ++ ")"

instance AutoEnvList LatexString where
   formatAssignList (x, ys) = showLineLatex x ++ " = " ++ formatStContList ys

   formatStContList (Just ys) = unLatexString (head ys)
   formatStContList Nothing = "+"
   formatList = unLatexString . head
   showListNode = showLatexNode
   divideEnergyList =
      zipWith
         (\(LatexString x) (LatexString y) ->
            LatexString $ "\\frac{" ++ x ++ "}{" ++ y ++ "}")


showLatexNode ::
   Idx.Record -> StorageMap [LatexString] ->
   (Maybe [LatexString] -> String) ->
   Topo.LNode -> String
showLatexNode rec st content (n@(Idx.SecNode _sec nid), ty) =
   show nid ++ "\\\\ " ++
   "Type: " ++ showNodeType ty ++
      case ty of
         Storage -> "\\\\ Content: " ++ content (M.lookup (Idx.Storage rec n) st)
         _ -> ""


instance (Eq a, ToIndex a) => AutoEnvList (Term a) where
   formatStContList (Just ys) = showEqTerms ys
   formatStContList Nothing = [heart]
   formatList = showEqTerms
   divideEnergyList = zipWith (\x y -> simplify $ x &/ y)

instance (Eq a, ToIndex a) => AutoEnvDeltaList (Term a) where
   formatElement = showEqTerm
   divideDEnergyList =
      L.zipWith4
         (\ea eb dea deb ->
            simplify $
            (dea :* eb  &-  ea :* deb) &/ ((eb:+deb):*eb))


showNode ::
   Idx.Record -> StorageMap a ->
   (Maybe a -> String) -> Topo.LNode -> String
showNode rec st content (n@(Idx.SecNode _sec nid), ty) =
   show nid ++ "\n" ++
   "Type: " ++ showNodeType ty ++
      case ty of
         Storage -> "\nContent: " ++ content (M.lookup (Idx.Storage rec n) st)
         _ -> ""

showNodeType :: NodeType -> String
showNodeType = show


envDeltaArg ::
   (a -> a -> a -> a -> a) ->
   ((Line, Maybe a) -> String) ->
   (Maybe a -> String) ->
   (DTimeMap a -> Idx.DTime -> String) ->
   Interp.Envs SingleRecord a ->
   Env a
envDeltaArg divide formatAssign content tshow
      (Interp.Envs (SingleRecord r) e de _p _dp _fn _dn dt _x dx _v st) =
   let lookupEnergy = makeLookup Idx.Energy e
       lookupDEnergy = makeLookup Idx.DEnergy de
   in  Env r
          lookupDEnergy
          (makeLookup Idx.DX dx)
          (\rec a b ->
             liftM4 divide
                (lookupEnergy rec a b) (lookupEnergy rec b a)
                (lookupDEnergy rec a b) (lookupDEnergy rec b a))
          formatAssign
          (tshow dt)
          (showNode r st content)


class AutoEnvSignal a where
   envAbsSignal ::
      (DispApp s, TDisp t) =>
      Interp.Envs SingleRecord (TC s t a) -> Env (TC s t a)

formatAssignSignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   (Line, Maybe (TC s t (Data v d))) -> String
formatAssignSignal (x, tc) = showLine x ++ " = " ++ formatStContSignal tc

formatStContSignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   Maybe (TC s t (Data v d)) -> String
formatStContSignal (Just ys) = sdisp ys
formatStContSignal Nothing = [heart]

instance
   (SDisplay v, D.Storage v a, Disp a, Ord a) =>
      AutoEnvSignal (Data v a) where
   envAbsSignal = envAbsArgSignal

instance
   (DispApp s, TDisp t, AutoEnvSignal a) =>
      AutoEnv (TC s t a) where
   envAbs = envAbsSignal


class AutoEnvSignal a => AutoEnvDeltaSignal a where
   envDeltaSignal ::
      (DispApp s, TDisp t) =>
      Interp.Envs SingleRecord (TC s t a) -> Env (TC s t a)

instance
   (SDisplay v, D.Storage v a, Disp a, Ord a) =>
      AutoEnvDeltaSignal (Data v a) where
   envDeltaSignal = envDeltaArgSignal

instance
   (DispApp s, TDisp t, AutoEnvDeltaSignal a) =>
      AutoEnvDelta (TC s t a) where
   envDelta = envDeltaSignal


envAbsArgSignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   Interp.Envs SingleRecord (TC s t (Data v d)) ->
   Env (TC s t (Data v d))
envAbsArgSignal
      (Interp.Envs (SingleRecord rec0) e _de _p _dp fn _dn dt x _dx _v st) =
   Env rec0
      (makeLookup Idx.Energy e)
      (makeLookup Idx.X x)
      (makeLookup Idx.FEta $
       M.intersectionWith ($) fn $
       M.mapKeys (\(Idx.Energy rec uid vid) -> Idx.FEta rec uid vid) e)
      formatAssignSignal
      (\dtimeIdx -> formatStContSignal $ M.lookup dtimeIdx dt)
      (showNode rec0 st formatStContSignal)

envDeltaArgSignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   Interp.Envs SingleRecord (TC s t (Data v d)) ->
   Env (TC s t (Data v d))
envDeltaArgSignal
      (Interp.Envs (SingleRecord rec0) _e de _p _dp _fn dn dt _x dx _v st) =
   Env rec0
      (makeLookup Idx.DEnergy de)
      (makeLookup Idx.DX dx)
      (makeLookup Idx.DEta $
       M.intersectionWith ($) dn $
       M.mapKeys (\(Idx.DEnergy rec uid vid) -> Idx.DEta rec uid vid) de)
      (\ (x, ys) -> showLineDelta x ++ " = " ++ formatStContSignal ys)
      (\dtimeIdx -> formatStContSignal $ M.lookup dtimeIdx dt)
      (showNode rec0 st formatStContSignal)


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
