module EFA2.Display.DrawGraph where

import EFA2.Solver.Equation
          (Term(..), ToIndex, showEqTerm, showEqTerms,
           LatexString, unLatexString)
import EFA2.Interpreter.Env
          (DEnergyIdx(..), EnergyIdx(..),
           DEtaIdx(..),    FEtaIdx(..),
           DXIdx(..),      XIdx(..),
           DTimeIdx(..),   DTimeMap,
           StorageIdx(..), StorageMap,
           RecordNumber(SingleRecord))
import qualified EFA2.Interpreter.Env as Interp
import EFA2.Topology.TopologyData
import EFA2.Topology.EfaGraph (EfaGraph)

import qualified EFA2.Signal.Data as D
import EFA2.Display.DispSignal (SDisplay, sdisp)
import EFA2.Display.DispTyp (TDisp)
import EFA2.Display.DispBase (Disp)
import EFA2.Signal.Signal (TC, DispApp)
import EFA2.Signal.Data (Data)

import EFA2.Utils.Utils (safeLookup)

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
   (DTimeIdx -> String) ->
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
        sg (ns, ms) = DotSG True (Just (Int sl)) (DotStmts gattrs [] xs ys)
          where sl = sectionNLabel $ snd $ NonEmpty.head ns
                xs = map (mkDotNode nshow) $ NonEmpty.flatten ns
                ys = map (mkDotEdge eshow) $ labEdges $
                     delNodes (map fst (concatMap NonEmpty.flatten ms)) g'
                gattrs = [GraphAttrs [Label (StrLabel (T.pack str))]]
                str =
                   "Section " ++ show sl ++ " / " ++
                   case recordNum of
                      Nothing -> "NoRecord"
                      Just n ->
                         "Record " ++ show n ++
                         " / Time " ++ timef (DTimeIdx sl n)
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
   (DTimeIdx -> String) ->
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

instance Show Line where
         show (ELine u v) = "e_" ++ show u ++ "_" ++ show v
         show (XLine u v) = "x_" ++ show u ++ "_" ++ show v
         show (NLine u v) = "n_" ++ show u ++ "_" ++ show v
         show (ErrorLine str) = str

showLineDelta :: Line -> String
showLineDelta (ELine u v) = "de_" ++ show u ++ "_" ++ show v
showLineDelta (XLine u v) = "dx_" ++ show u ++ "_" ++ show v
showLineDelta (NLine u v) = "dn_" ++ show u ++ "_" ++ show v
showLineDelta (ErrorLine str) = str


data Env a =
   Env {
      recordNumber :: RecordNumber,
      lookupEnergy_ :: Int -> Int -> Int -> Int -> Maybe a,
      lookupX_      :: Int -> Int -> Int -> Int -> Maybe a,
      lookupEta_    :: Int -> Int -> Int -> Int -> Maybe a,
      showTime :: DTimeIdx -> String,
      showNode_ :: (Maybe a -> String) -> (Int, NLabel) -> String
   }

makeLookup ::
   (Ord idx) =>
   (Int -> Int -> Int -> Int -> idx) -> M.Map idx a ->
   Int -> Int -> Int -> Int -> Maybe a
makeLookup makeIdx mp =
   \sec rec uid vid -> M.lookup (makeIdx sec rec uid vid) mp


draw ::
   ((Line, Maybe a) -> String) ->
   (Maybe a -> String) ->
   Topology' NLabel ELabel ->
   Env a ->
   IO ()
draw f content (Topology g)
   (Env rn lookupEnergy lookupX lookupEta tshow nshow) =
      printGraph g (Just rn) tshow (nshow content) eshow
  where eshow = L.intercalate "\n" . map f . mkLst

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


-- provide a mock for '1' also for non-Num types
-- FIXME: this is still a hack because 'one' is often undefined
class One a where
  one :: a

class DrawTopology a where
   drawTopology :: Topology -> Interp.Envs a -> IO ()

class DrawTopology a => DrawDeltaTopology a where
   drawDeltaTopology :: Topology -> Interp.Envs a -> IO ()


class One a => DrawTopologyList a where
   drawTopologyList :: Topology -> Interp.Envs [a] -> IO ()
   drawTopologyList topo env =
      draw formatAssignList formatStContList topo
         (envAbsTopology
            (\dt k ->
               case M.lookup k dt of
                  Nothing ->
                     error $
                     "drawTopologyList: " ++ show k ++ "\n" ++ show (fmap formatDTimeList dt)
                  Just x -> formatDTimeList x)
            env [one])

   formatStContList :: Maybe [a] -> String
   formatDTimeList :: [a] -> String

   formatAssignList :: DrawTopologyList a => (Line, Maybe [a]) -> String
   formatAssignList (x, ys) =
      show x ++ " = " ++ formatStContList ys

class DrawTopologyList a => DrawDeltaTopologyList a where
   drawDeltaTopologyList :: Topology -> Interp.Envs [a] -> IO ()

instance DrawTopologyList a => DrawTopology [a] where
   drawTopology = drawTopologyList

instance DrawDeltaTopologyList a => DrawDeltaTopology [a] where
   drawDeltaTopology = drawDeltaTopologyList


instance One Double where one = 1

instance DrawTopologyList Double where
   formatStContList (Just ys) = concatMap (printf "%.6f    ") ys
   formatStContList Nothing = [heart]
   formatDTimeList = show

instance DrawDeltaTopologyList Double where
   drawDeltaTopologyList topo env =
      draw f formatStCont
         topo (envDeltaTopology tshow env [one])
     where -- f (x, Just ys) = showLineDelta x ++ " = [ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
           f (x, ys) =
              showLineDelta x ++ " = " ++
              maybe [heart] (concatMap (("\n"++) . show)) ys

           formatStCont (Just ys) = "[ " ++ L.intercalate ", " (map show ys) ++ " ]"
           formatStCont Nothing = [heart]

           tshow dt dtimeIdx = show $ dt `safeLookup` dtimeIdx

instance (Integral a) => One (Ratio a) where one = 1

instance (Integral a, Show a) => DrawTopologyList (Ratio a) where
   formatStContList (Just ys) = unwords $ map show ys
   formatStContList Nothing = [heart]
   formatDTimeList = show

instance DrawTopologyList Char where
   formatStContList (Just ys) = ys
   formatStContList Nothing = "+"
   formatDTimeList = id

instance One LatexString where
   one = error "LatexString 1"

instance DrawTopologyList LatexString where
   drawTopologyList topo env =
      draw formatAssignList formatStContList topo
         (envAbsTopologyLatex (\dt dtimeIdx -> formatDTimeList $ dt `safeLookup` dtimeIdx) env)

   formatAssignList (x, ys) = showX x ++ " = " ++ formatStContList ys
      where
         showX (ELine u v) = "$e_{" ++ show u ++ "." ++ show v ++ "}$"
         showX (XLine u v) = "$x_{" ++ show u ++ "." ++ show v ++ "}$"
         showX (NLine u v) = "$n_{" ++ show u ++ "." ++ show v ++ "}$"
         showX (ErrorLine str) = str

   formatStContList (Just ys) = unLatexString (head ys)
   formatStContList Nothing = "+"
   formatDTimeList = unLatexString . head


envAbsTopologyLatex ::
   (DTimeMap [LatexString] -> DTimeIdx -> String) ->
   Interp.Envs [LatexString] ->
   Env [LatexString]
envAbsTopologyLatex tshow (Interp.Envs rec e _de _p _dp fn _dn dt x _dx _v st) =
   Env rec
      (makeLookup EnergyIdx e)
      (makeLookup XIdx x)
      (makeLookup FEtaIdx $ fmap ($[one]) fn)
      (tshow dt)
      (showLatexNode rec st)

showLatexNode ::
   RecordNumber -> StorageMap [LatexString] ->
   (Maybe [LatexString] -> String) ->
   (Int, NLabel) -> String
showLatexNode rn st content (num, NLabel sec nid ty) =
   "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\\\\ " ++
   "Type: " ++ show ty ++
      let showStorage n =
             case rn of
                SingleRecord rec -> content (M.lookup (StorageIdx sec rec n) st)
                _ -> "Problem with record number: " ++ show rn
      in  case ty of
             InitStorage n -> "\\\\ Content: " ++ showStorage n
             Storage n -> "\\\\ Content: " ++ showStorage n
             _ -> ""


instance One (Term a) where one = error "EqTerm 1"
instance One Char where one = error "Char 1"

instance ToIndex a => DrawTopologyList (Term a) where
   formatStContList (Just ys) = showEqTerms ys
   formatStContList Nothing = [heart]
   formatDTimeList = showEqTerms

instance ToIndex a => DrawDeltaTopologyList (Term a) where
   drawDeltaTopologyList topo env =
      draw f formatStCont topo (envDeltaTopology tshow env [one])
           where -- f (x, Just ys) = showLineDelta x ++ " = [ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
                 f (x, Just ys) = showLineDelta x ++ " = \n" ++ showEqTerms ys

                 f (x, Nothing) = showLineDelta x ++ " = " ++ [heart]
                 formatStCont (Just ys) = "[ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
                 formatStCont Nothing = [heart]

                 tshow dt dtimeIdx =
                    case M.lookup dtimeIdx dt of
                       Nothing ->
                          error $
                          "drawTopologyList: " ++ show dtimeIdx ++ "\n"
                           ++ show (fmap showEqTerms dt)
                       Just t -> showEqTerms t


envAbsTopology ::
   (DTimeMap a -> DTimeIdx -> String) ->
   Interp.Envs a -> a ->
   Env a
envAbsTopology tshow (Interp.Envs rec e _de _p _dp fn _dn dt x _dx _v st) etaArg =
   Env rec
      (makeLookup EnergyIdx e)
      (makeLookup XIdx x)
      (makeLookup FEtaIdx $ fmap ($etaArg) fn)
      (tshow dt)
      (showNode rec st)

showNode ::
   RecordNumber -> StorageMap a ->
   (Maybe a -> String) -> (Int, NLabel) -> String
showNode rn st content (num, NLabel sec nid ty) =
   "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\n" ++
   "Type: " ++ show ty ++
      let showStorage n =
             case rn of
                SingleRecord rec -> content (M.lookup (StorageIdx sec rec n) st)
                _ -> "Problem with record number: " ++ show rn
      in  case ty of
             InitStorage n -> "\nContent: " ++ showStorage n
             Storage n -> "\nContent: " ++ showStorage n
             _ -> ""


envDeltaTopology ::
   (DTimeMap a -> DTimeIdx -> String) ->
   Interp.Envs a -> a ->
   Env a
envDeltaTopology tshow (Interp.Envs rec _e de _p _dp _fn dn dt _x dx _v st) etaArg =
   Env rec
      (makeLookup DEnergyIdx de)
      (makeLookup DXIdx dx)
      (makeLookup DEtaIdx $ fmap ($etaArg) dn)
      (tshow dt)
      (showNode rec st)


class DrawTopologySignal a where
   drawTopologySignal ::
      (DispApp s, TDisp t) => Topology -> Interp.Envs (TC s t a) -> IO ()

formatAssignSignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   (Line, Maybe (TC s t (Data v d))) -> String
formatAssignSignal (ErrorLine str, _) = str
formatAssignSignal (x, tc) = show x ++ " = " ++ formatStContSignal tc

formatStContSignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   Maybe (TC s t (Data v d)) -> String
formatStContSignal (Just ys) = sdisp ys
formatStContSignal Nothing = [heart]

instance
   (SDisplay v, D.Storage v a, Disp a, Ord a) =>
      DrawTopologySignal (Data v a) where
   drawTopologySignal topo env =
      draw
         formatAssignSignal formatStContSignal topo
         (envAbsTopologySignal env)

instance
   (DispApp s, TDisp t, DrawTopologySignal a) =>
      DrawTopology (TC s t a) where
   drawTopology = drawTopologySignal


class DrawTopologySignal a => DrawDeltaTopologySignal a where
   drawDeltaTopologySignal ::
      (DispApp s, TDisp t) => Topology -> Interp.Envs (TC s t a) -> IO ()

instance
   (SDisplay v, D.Storage v a, Disp a, Ord a) =>
      DrawDeltaTopologySignal (Data v a) where
         drawDeltaTopologySignal topo env =
            draw f formatStContSignal topo (envDeltaTopologySignal env)
           where -- f (x, Just ys) = showLineDelta x ++ " = [ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
                 f (x, ys) = showLineDelta x ++ " = " ++ formatStContSignal ys

instance
   (DispApp s, TDisp t, DrawDeltaTopologySignal a) =>
      DrawDeltaTopology (TC s t a) where
   drawDeltaTopology = drawDeltaTopologySignal


envAbsTopologySignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   Interp.Envs (TC s t (Data v d)) ->
   Env (TC s t (Data v d))
envAbsTopologySignal (Interp.Envs rec0 e _de _p _dp fn _dn dt x _dx _v st) =
   Env rec0
      (makeLookup EnergyIdx e)
      (makeLookup XIdx x)
      (makeLookup FEtaIdx $
       M.intersectionWith ($) fn $
       M.mapKeys (\(EnergyIdx sec rec uid vid) -> FEtaIdx sec rec uid vid) e)
      (\dtimeIdx -> formatStContSignal $ M.lookup dtimeIdx dt)
      (showNode rec0 st)

envDeltaTopologySignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   Interp.Envs (TC s t (Data v d)) ->
   Env (TC s t (Data v d))
envDeltaTopologySignal (Interp.Envs rec0 _e de _p _dp _fn dn dt _x dx _v st) =
   Env rec0
      (makeLookup DEnergyIdx de)
      (makeLookup DXIdx dx)
      (makeLookup DEtaIdx $
       M.intersectionWith ($) dn $
       M.mapKeys (\(DEnergyIdx sec rec uid vid) -> DEtaIdx sec rec uid vid) de)
      (\dtimeIdx -> formatStContSignal $ M.lookup dtimeIdx dt)
      (showNode rec0 st)


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
