module EFA2.Topology.Draw where

import EFA2.Solver.Equation
          (Term(..), ToIndex, showEqTerm, showEqTerms,
           LatexString, unLatexString)
import EFA2.Interpreter.Env
          (DTimeMap, StorageMap,
           RecordNumber(SingleRecord))
import qualified EFA2.Interpreter.Env as Interp
import EFA2.Topology.TopologyData
import EFA2.Topology.EfaGraph (EfaGraph)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Data as D
import EFA2.Display.DispSignal (SDisplay, sdisp)
import EFA2.Display.DispTyp (TDisp)
import EFA2.Display.DispBase (Disp)
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

import Data.Graph.Inductive
          (LNode, LEdge, lab, labNodes, labEdges, delNodes, delEdges)
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
import Control.Monad ((>=>), void)

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
noRecord :: Maybe RecordNumber
noRecord = Nothing


mkDotGraph ::
   EfaGraph NLabel ELabel ->
   Maybe RecordNumber ->
   (Idx.DTime -> String) ->
   (LNode NLabel -> String) ->
   (LEdge ELabel -> String) ->
   DotGraph Int
mkDotGraph g mRecordNum timef nshow eshow =
  DotGraph { strictGraph = False,
             directedGraph = True,
             graphID = Just (Int 1),
             graphStatements = stmts }
  where recordNum =
           case mRecordNum of
              Just (SingleRecord n) -> Just n
              _ -> Nothing
        interEs = L.filter (\(_, _, e) -> isIntersectionEdge e) $ labEdges g
        g' = delEdges (map (\(x, y, _) -> (x, y)) interEs) g
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
                           edgeStmts = map (mkDotEdge eshow) interEs }


mkDotNode:: (LNode NLabel -> String) -> LNode NLabel -> DotNode Int
mkDotNode nshow n@(x, _) = DotNode x [displabel, nodeColour, Style [SItem Filled []], Shape BoxShape ]
  where displabel =  Label $ StrLabel $ T.pack (nshow n)

mkDotEdge :: (LEdge ELabel -> String) -> LEdge ELabel -> DotEdge Int
mkDotEdge eshow e@(x, y, elabel) = DotEdge x y [displabel, edir, colour]
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
   EfaGraph NLabel ELabel ->
   Maybe RecordNumber ->
   (Idx.DTime -> String) ->
   (LNode NLabel -> String) ->
   (LEdge ELabel -> String) ->
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
        mkEdge (x, y, l) =
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
      recordNumber :: RecordNumber,
      lookupEnergy_ :: Idx.Section -> Idx.Record -> Int -> Int -> Maybe a,
      lookupX_      :: Idx.Section -> Idx.Record -> Int -> Int -> Maybe a,
      lookupEta_    :: Idx.Section -> Idx.Record -> Int -> Int -> Maybe a,
      formatAssign_ :: (Line, Maybe a) -> String,
      showTime :: Idx.DTime -> String,
      showNode_ :: LNode NLabel -> String
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


draw ::
   Topology' NLabel ELabel ->
   Env a ->
   IO ()
draw (Topology g)
   (Env rn lookupEnergy lookupX lookupEta formatAssign tshow nshow) =
      printGraph g (Just rn) tshow nshow eshow
  where eshow = L.intercalate "\n" . map formatAssign . mkLst

        mkLst (uid, vid, l) =
           case rn of
              SingleRecord rec
                 | isOriginalEdge l -> [
                    (ELine uid vid, lookupEnergy usec rec uid vid),
                    (XLine uid vid, lookupX usec rec uid vid),
                    ndirlab (flowDirection l),
                    (XLine vid uid, lookupX vsec rec vid uid),
                    (ELine vid uid, lookupEnergy vsec rec vid uid)
                    ]
                 | isInnerStorageEdge l ->
                    [ (ELine vid uid, lookupEnergy vsec rec vid uid) ]
                 | otherwise -> [
                    (ELine uid vid, lookupEnergy usec rec uid vid),
                    (XLine uid vid, lookupX usec rec uid vid),
                    (ELine vid uid, lookupEnergy vsec rec vid uid)
                    ]
                 where NLabel usec _ _ = fromJust $ lab g uid
                       NLabel vsec _ _ = fromJust $ lab g vid
                       ndirlab WithDir = (NLine uid vid, lookupEta usec rec uid vid)
                       ndirlab _ = (NLine vid uid, lookupEta vsec rec vid uid)
              _ -> [ (ErrorLine "Problem with record number", Nothing) ]

drawTopology ::
   DrawTopology a => Topology -> Interp.Envs a -> IO ()
drawTopology topo = draw topo . envTopology

drawDeltaTopology ::
   DrawDeltaTopology a => Topology -> Interp.Envs a -> IO ()
drawDeltaTopology topo = draw topo . envDeltaTopology


class DrawTopology a where
   envTopology :: Interp.Envs a -> Env a

class DrawTopology a => DrawDeltaTopology a where
   envDeltaTopology :: Interp.Envs a -> Env a


class DrawTopologyList a where
   envTopologyList :: Interp.Envs [a] -> Env [a]
   envTopologyList = envAbsTopologyList [defaultEtaArg]

   formatStContList :: Maybe [a] -> String
   formatList :: [a] -> String

   formatAssignList :: DrawTopologyList a => (Line, Maybe [a]) -> String
   formatAssignList (x, ys) =
      showLine x ++ " = " ++ formatStContList ys

   showListNode ::
      RecordNumber -> StorageMap [a] ->
      (Maybe [a] -> String) ->
      (Int, NLabel) -> String
   showListNode = showNode

   {-
   We have to get rid of this because it is undefined for many instances.
   -}
   defaultEtaArg :: a

class DrawTopologyList a => DrawDeltaTopologyList a where
   envDeltaTopologyList :: Interp.Envs [a] -> Env [a]
   formatElement :: a -> String

instance DrawTopologyList a => DrawTopology [a] where
   envTopology = envTopologyList

instance DrawDeltaTopologyList a => DrawDeltaTopology [a] where
   envDeltaTopology = envDeltaTopologyList


instance DrawTopologyList Double where
   formatStContList (Just ys) = concatMap (printf "%.6f    ") ys
   formatStContList Nothing = [heart]
   formatList = show
   defaultEtaArg = 1

instance DrawDeltaTopologyList Double where
   envDeltaTopologyList = envDeltaTopologyList_ [defaultEtaArg]
   formatElement = show

instance (Integral a, Show a) => DrawTopologyList (Ratio a) where
   formatStContList (Just ys) = unwords $ map show ys
   formatStContList Nothing = [heart]
   formatList = show
   defaultEtaArg = 1

instance DrawTopologyList Char where
   formatStContList (Just ys) = ys
   formatStContList Nothing = "+"
   formatList = id
   defaultEtaArg = error "Char 1"

instance DrawTopologyList LatexString where
   formatAssignList (x, ys) = showLineLatex x ++ " = " ++ formatStContList ys

   formatStContList (Just ys) = unLatexString (head ys)
   formatStContList Nothing = "+"
   formatList = unLatexString . head
   showListNode = showLatexNode
   defaultEtaArg = error "LatexString 1"


showLatexNode ::
   RecordNumber -> StorageMap [LatexString] ->
   (Maybe [LatexString] -> String) ->
   (Int, NLabel) -> String
showLatexNode rn st content (num, NLabel sec nid ty) =
   "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\\\\ " ++
   "Type: " ++ show ty ++
      let showStorage n =
             case rn of
                SingleRecord rec -> content (M.lookup (Idx.Storage sec rec n) st)
                _ -> "Problem with record number: " ++ show rn
      in  case ty of
             InitStorage n -> "\\\\ Content: " ++ showStorage n
             Storage n -> "\\\\ Content: " ++ showStorage n
             _ -> ""


instance ToIndex a => DrawTopologyList (Term a) where
   formatStContList (Just ys) = showEqTerms ys
   formatStContList Nothing = [heart]
   formatList = showEqTerms
   defaultEtaArg = error "EqTerm 1"

instance ToIndex a => DrawDeltaTopologyList (Term a) where
   envDeltaTopologyList = envDeltaTopologyList_ [defaultEtaArg]
   formatElement = showEqTerm


envAbsTopologyList ::
   (DrawTopologyList a) =>
   [a] -> Interp.Envs [a] -> Env [a]
envAbsTopologyList etaArg
      (Interp.Envs rec e _de _p _dp fn _dn dt x _dx _v st) =
   Env rec
      (makeLookup Idx.Energy e)
      (makeLookup Idx.X x)
      (makeLookup Idx.FEta $ fmap ($etaArg) fn)
      formatAssignList
      (checkedLookupFormat "envAbsTopologyList" formatList dt)
      (showListNode rec st formatStContList)


showNode ::
   RecordNumber -> StorageMap a ->
   (Maybe a -> String) -> (Int, NLabel) -> String
showNode rn st content (num, NLabel sec nid ty) =
   "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\n" ++
   "Type: " ++ show ty ++
      let showStorage n =
             case rn of
                SingleRecord rec -> content (M.lookup (Idx.Storage sec rec n) st)
                _ -> "Problem with record number: " ++ show rn
      in  case ty of
             InitStorage n -> "\nContent: " ++ showStorage n
             Storage n -> "\nContent: " ++ showStorage n
             _ -> ""


envDeltaTopology_ ::
   ((Line, Maybe a) -> String) ->
   (Maybe a -> String) ->
   (DTimeMap a -> Idx.DTime -> String) ->
   a -> Interp.Envs a ->
   Env a
envDeltaTopology_ formatAssign content tshow etaArg
      (Interp.Envs rec _e de _p _dp _fn dn dt _x dx _v st) =
   Env rec
      (makeLookup Idx.DEnergy de)
      (makeLookup Idx.DX dx)
      (makeLookup Idx.DEta $ fmap ($etaArg) dn)
      formatAssign
      (tshow dt)
      (showNode rec st content)

envDeltaTopologyList_ ::
   (DrawDeltaTopologyList a) =>
   [a] -> Interp.Envs [a] ->
   Env [a]
envDeltaTopologyList_ etaArg =
   envDeltaTopology_
      (\(x, ys) ->
         showLineDelta x ++ " = " ++
         maybe [heart] (concatMap (("\n"++) . formatElement)) ys)
      (\mys ->
         case mys of
            Just ys -> "[ " ++ L.intercalate ", " (map formatElement ys) ++ " ]"
            Nothing -> [heart])
      (checkedLookupFormat "envDeltaTopologyList_" formatList)
      etaArg

class DrawTopologySignal a where
   envTopologySignal ::
      (DispApp s, TDisp t) => Interp.Envs (TC s t a) -> Env (TC s t a)

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
      DrawTopologySignal (Data v a) where
   envTopologySignal = envAbsTopologySignal

instance
   (DispApp s, TDisp t, DrawTopologySignal a) =>
      DrawTopology (TC s t a) where
   envTopology = envTopologySignal


class DrawTopologySignal a => DrawDeltaTopologySignal a where
   envDeltaTopologySignal ::
      (DispApp s, TDisp t) => Interp.Envs (TC s t a) -> Env (TC s t a)

instance
   (SDisplay v, D.Storage v a, Disp a, Ord a) =>
      DrawDeltaTopologySignal (Data v a) where
   envDeltaTopologySignal = envDeltaTopologySignal_

instance
   (DispApp s, TDisp t, DrawDeltaTopologySignal a) =>
      DrawDeltaTopology (TC s t a) where
   envDeltaTopology = envDeltaTopologySignal


envAbsTopologySignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   Interp.Envs (TC s t (Data v d)) ->
   Env (TC s t (Data v d))
envAbsTopologySignal (Interp.Envs rec0 e _de _p _dp fn _dn dt x _dx _v st) =
   Env rec0
      (makeLookup Idx.Energy e)
      (makeLookup Idx.X x)
      (makeLookup Idx.FEta $
       M.intersectionWith ($) fn $
       M.mapKeys (\(Idx.Energy sec rec uid vid) -> Idx.FEta sec rec uid vid) e)
      formatAssignSignal
      (\dtimeIdx -> formatStContSignal $ M.lookup dtimeIdx dt)
      (showNode rec0 st formatStContSignal)

envDeltaTopologySignal_ ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   Interp.Envs (TC s t (Data v d)) ->
   Env (TC s t (Data v d))
envDeltaTopologySignal_ (Interp.Envs rec0 _e de _p _dp _fn dn dt _x dx _v st) =
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