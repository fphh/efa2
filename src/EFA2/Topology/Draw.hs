module EFA2.Topology.Draw where

import EFA2.Solver.Equation
          (Term(..), ToIndex, simplify, showEqTerm, showEqTerms, (&-), (&/),
           LatexString(LatexString), unLatexString)
import EFA2.Interpreter.Env
          (DTimeMap, StorageMap,
           SingleRecord(SingleRecord))
import qualified EFA2.Interpreter.Env as Interp
import EFA2.Topology.TopologyData as Topo
import EFA2.Topology.EfaGraph
          (EfaGraph, Edge(Edge),
           lab, labNodes, labEdges, edgeLabels, delNodes, delEdgeSet)

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
import Data.GraphViz.Attributes.Complete

import Data.Graph.Inductive (Node)
import Data.Eq.HT (equating)
import Data.Ratio (Ratio)
import Data.Maybe (fromJust)

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
   EfaGraph Node NLabel ELabel ->
   Maybe Idx.Record ->
   (Idx.DTime -> String) ->
   (Topo.LNode -> String) ->
   (Topo.LEdge -> String) ->
   DotGraph Int
mkDotGraph g recordNum timef nshow eshow =
  DotGraph { strictGraph = False,
             directedGraph = True,
             graphID = Just (Int 1),
             graphStatements = stmts }
  where interEs = M.filter isIntersectionEdge $ edgeLabels g
        g' = delEdgeSet (M.keysSet interEs) g
        cs =
           HTL.removeEach $
           NonEmptyM.groupBy (equating (sectionNLabel . snd)) $
           labNodes g
        sg (ns, ms) = DotSG True (Just (Int $ fromEnum sl)) (DotStmts gattrs [] xs ys)
          where sl = sectionNLabel $ snd $ NonEmpty.head ns
                xs = map (mkDotNode nshow) $ NonEmpty.flatten ns
                ys = map (mkDotEdge eshow) $ labEdges $
                     delNodes (map fst (concatMap NonEmpty.flatten ms)) g'
                gattrs = [GraphAttrs [Label (StrLabel (T.pack str))]]
                str =
                   show sl ++ " / " ++
                   case recordNum of
                      Nothing -> "NoRecord"
                      Just n ->
                         show n ++ " / Time " ++ timef (Idx.DTime sl n)
        stmts = DotStmts { attrStmts = [],
                           subGraphs = map sg cs,
                           nodeStmts = [],
                           edgeStmts = map (mkDotEdge eshow) $ M.toList interEs }


mkDotNode:: (Topo.LNode -> String) -> Topo.LNode -> DotNode Int
mkDotNode nshow n@(x, _) = DotNode x [displabel, nodeColour, Style [SItem Filled []], Shape BoxShape ]
  where displabel =  Label $ StrLabel $ T.pack (nshow n)

mkDotEdge :: (Topo.LEdge -> String) -> Topo.LEdge -> DotEdge Int
mkDotEdge eshow e@(Edge x y, elabel) = DotEdge x y [displabel, edir, colour]
  where flowDir = flowDirection elabel
        displabel =
           Label $ StrLabel $ T.pack $
           case flowDir of
              UnDir -> ""
              _ -> eshow e
        edir =
           Dir $
           case flowDir of
              AgainstDir -> Back
              WithDir -> Forward
              _ -> NoDir
        colour =
           case edgeType elabel of
              IntersectionEdge -> intersectionEdgeColour
              _ -> originalEdgeColour
        --colour = originalEdgeColour

printGraph ::
   EfaGraph Node NLabel ELabel ->
   Maybe Idx.Record ->
   (Idx.DTime -> String) ->
   (Topo.LNode -> String) ->
   (Topo.LEdge -> String) ->
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

drawTopologyX' :: Topology -> IO ()
drawTopologyX' topo =
   printGraph (unTopology topo) noRecord (const [heart]) show show


drawTopologySimple :: Topology -> IO ()
drawTopologySimple topo =
   printGraph (unTopology topo) noRecord (const [heart]) nshow eshow
  where nshow (n, l) = show n ++ " - " ++ show (nodetypeNLabel l)
        eshow _ = ""

dsg :: Int -> Topology -> DotSubGraph String
dsg ident topo = DotSG True (Just (Int ident)) stmts
  where stmts = DotStmts attrs [] ns es
        attrs = [GraphAttrs [labelf ident]]
        ns = map mkNode (labNodes topo)
        idf x = show ident ++ "_" ++ show x
        labelf x = Label $ StrLabel $ T.pack (show x)
        mkNode x@(n, _) = DotNode (idf n) (nattrs x)
        nattrs x = [labNodef x, nodeColour, Style [SItem Filled []], Shape BoxShape ]
        labNodef (n, l) = Label $ StrLabel $ T.pack (show n ++ " - " ++ show (nodetypeNLabel l))
        es = map mkEdge (labEdges topo)
        mkEdge (Edge x y, l) =
           DotEdge (idf x) (idf y) $ (:[]) $ Dir $
           case flowDirection l of
              WithDir -> Forward
              AgainstDir -> Back
              _ -> NoDir

drawTopologyXs' :: [Topology] -> IO ()
drawTopologyXs' ts = runGraphvizCanvas Dot g Xlib
  where g = DotGraph False True Nothing stmts
        stmts = DotStmts attrs subgs [] []
        subgs = zipWith dsg [0..] ts
        attrs = []

data Line = ELine Int Int
          | XLine Int Int
          | NLine Int Int
          | ErrorLine String deriving (Eq, Ord)

showLine :: Line -> String
showLine (ELine u v) = "e_" ++ show u ++ "_" ++ show v
showLine (XLine u v) = "x_" ++ show u ++ "_" ++ show v
showLine (NLine u v) = "n_" ++ show u ++ "_" ++ show v
showLine (ErrorLine str) = str

showLineLatex :: Line -> String
showLineLatex (ELine u v) = "$e_{" ++ show u ++ "." ++ show v ++ "}$"
showLineLatex (XLine u v) = "$x_{" ++ show u ++ "." ++ show v ++ "}$"
showLineLatex (NLine u v) = "$n_{" ++ show u ++ "." ++ show v ++ "}$"
showLineLatex (ErrorLine str) = str

showLineDelta :: Line -> String
showLineDelta (ELine u v) = "de_" ++ show u ++ "_" ++ show v
showLineDelta (XLine u v) = "dx_" ++ show u ++ "_" ++ show v
showLineDelta (NLine u v) = "dn_" ++ show u ++ "_" ++ show v
showLineDelta (ErrorLine str) = str


data Env a =
   Env {
      recordNumber :: Idx.Record,
      lookupEnergy_ :: Idx.Section -> Idx.Record -> Int -> Int -> Maybe a,
      lookupX_      :: Idx.Section -> Idx.Record -> Int -> Int -> Maybe a,
      lookupEta_    :: Idx.Section -> Idx.Record -> Int -> Int -> Maybe a,
      formatAssign_ :: (Line, Maybe a) -> String,
      showTime :: Idx.DTime -> String,
      showNode_ :: Topo.LNode -> String
   }

makeLookup ::
   (Ord idx) =>
   (Idx.Section -> Idx.Record -> Int -> Int -> idx) -> M.Map idx a ->
   Idx.Section -> Idx.Record -> Int -> Int -> Maybe a
makeLookup makeIdx mp =
   \sec rec uid vid -> M.lookup (makeIdx sec rec uid vid) mp

checkedLookupFormat ::
   (Ord idx, Show idx) =>
   String -> (a -> String) -> M.Map idx a -> idx -> String
checkedLookupFormat msg format dt k =
   case M.lookup k dt of
      Nothing ->
         error $
         msg ++ ": " ++ show k ++ "\n" ++ show (fmap format dt)
      Just x -> format x


draw :: Topology -> Env a -> IO ()
draw g
   (Env rec lookupEnergy lookupX lookupEta formatAssign tshow nshow) =
      printGraph (unTopology g) (Just rec) tshow nshow eshow
  where eshow = L.intercalate "\n" . map formatAssign . mkLst

        mkLst (Edge uid vid, l) =
           case edgeType l of
              OriginalEdge ->
                 (ELine uid vid, lookupEnergy usec rec uid vid) :
                 (XLine uid vid, lookupX usec rec uid vid) :
                 ndirlab (flowDirection l) :
                 (XLine vid uid, lookupX vsec rec vid uid) :
                 (ELine vid uid, lookupEnergy vsec rec vid uid) :
                 []
              InnerStorageEdge ->
                 (ELine vid uid, lookupEnergy vsec rec vid uid) :
                 []
              IntersectionEdge ->
                 (ELine uid vid, lookupEnergy usec rec uid vid) :
                 (XLine uid vid, lookupX usec rec uid vid) :
                 (ELine vid uid, lookupEnergy vsec rec vid uid) :
                 []
            where NLabel usec _ _ = fromJust $ lab g uid
                  NLabel vsec _ _ = fromJust $ lab g vid
                  ndirlab WithDir = (NLine uid vid, lookupEta usec rec uid vid)
                  ndirlab _ = (NLine vid uid, lookupEta vsec rec vid uid)

drawTopology ::
   AutoEnv a => Topology -> Interp.Envs SingleRecord a -> IO ()
drawTopology topo = draw topo . envAbs

drawDeltaTopology ::
   AutoEnvDelta a => Topology -> Interp.Envs SingleRecord a -> IO ()
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
      (Int, NLabel) -> String
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
             (\sec rec a b ->
                liftM2 divideEnergyList
                   (lookupEnergy sec rec a b)
                   (lookupEnergy sec rec b a))
             formatAssignList
             (checkedLookupFormat "envAbsList" formatList dt)
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
         (checkedLookupFormat "envDeltaArg" formatList)


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
   (Int, NLabel) -> String
showLatexNode rec st content (num, NLabel sec nid ty) =
   "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\\\\ " ++
   "Type: " ++ showNodeType ty ++
      let showStorage n =
             content (M.lookup (Idx.Storage sec rec n) st)
      in  case ty of
             InitStorage n -> "\\\\ Content: " ++ showStorage n
             Storage n -> "\\\\ Content: " ++ showStorage n
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
   (Maybe a -> String) -> (Int, NLabel) -> String
showNode rec st content (num, NLabel sec nid ty) =
   "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\n" ++
   "Type: " ++ showNodeType ty ++
      let showStorage n =
             content (M.lookup (Idx.Storage sec rec n) st)
      in  case ty of
             InitStorage n -> "\nContent: " ++ showStorage n
             Storage n -> "\nContent: " ++ showStorage n
             _ -> ""

showNodeType :: NodeType -> String
showNodeType (InitStorage (Idx.Store n)) = "InitStorage " ++ show n
showNodeType (Storage (Idx.Store n)) = "Storage " ++ show n
showNodeType nt = show nt


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
          (\sec rec a b ->
             liftM4 divide
                (lookupEnergy sec rec a b) (lookupEnergy sec rec b a)
                (lookupDEnergy sec rec a b) (lookupDEnergy sec rec b a))
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
formatAssignSignal (ErrorLine str, _) = str
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
       M.mapKeys (\(Idx.Energy sec rec uid vid) -> Idx.FEta sec rec uid vid) e)
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
       M.mapKeys (\(Idx.DEnergy sec rec uid vid) -> Idx.DEta sec rec uid vid) de)
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
