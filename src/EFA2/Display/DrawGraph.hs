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


data Env eidx xidx nidx a =
   Env {
      recordNumber :: RecordNumber,
      makeEnergyIdx :: Int -> Int -> Int -> Int -> eidx, eMap :: M.Map eidx a,
      makeXIdx      :: Int -> Int -> Int -> Int -> xidx, xMap :: M.Map xidx a,
      makeEtaIdx    :: Int -> Int -> Int -> Int -> nidx, nMap :: M.Map nidx a,
      showTime :: DTimeIdx -> String,
      showNode_ :: (Maybe a -> String) -> (Int, NLabel) -> String
   }


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
      drawAbsTopology formatAssignList formatStContList topo
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
      drawDeltaTopology' f formatStCont
         topo (envDeltaTopology tshow env [one])
     where -- f (x, Just ys) = showDelta x ++ " = [ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
           f (x, ys) =
              showDelta x ++ " = " ++
              maybe [heart] (concatMap (("\n"++) . show)) ys

           formatStCont (Just ys) = "[ " ++ L.intercalate ", " (map show ys) ++ " ]"
           formatStCont Nothing = [heart]

           tshow dt dtimeIdx = show $ dt `safeLookup` dtimeIdx

           showDelta (ELine u v) = "de_" ++ show u ++ "_" ++ show v
           showDelta (XLine u v) = "dx_" ++ show u ++ "_" ++ show v
           showDelta (NLine u v) = "dn_" ++ show u ++ "_" ++ show v
           showDelta (ErrorLine str) = str

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
      drawAbsTopologyLatex formatAssignList formatStContList topo
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
   Env EnergyIdx XIdx FEtaIdx [LatexString]
envAbsTopologyLatex tshow (Interp.Envs rec e _de _p _dp fn _dn dt x _dx _v st) =
   Env rec EnergyIdx e XIdx x FEtaIdx (fmap ($[one]) fn)
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

drawAbsTopologyLatex ::
   ((Line, Maybe a) -> String) ->
   (Maybe a -> String) ->
   Topology' NLabel ELabel ->
   Env EnergyIdx XIdx FEtaIdx a ->
   IO ()
drawAbsTopologyLatex f content (Topology g)
   (Env rec0 energyIdx e xIdx x etaIdx fn tshow nshow) =
      printGraph g (Just rec0) tshow (nshow content) eshow
  where eshow ps = L.intercalate "\n " $ map f $ mkLst rec0 ps

        mkLst (SingleRecord rec) (uid, vid, l)
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (energyIdx usec rec uid vid) e),
                                 (XLine uid vid, M.lookup (xIdx usec rec uid vid) x),
                                 ndirlab (flowDirection l),
                                 (XLine vid uid, M.lookup (xIdx vsec rec vid uid) x),
                                 (ELine vid uid, M.lookup (energyIdx vsec rec vid uid) e) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (energyIdx vsec rec vid uid) e) ]
          | otherwise = [ (ELine uid vid, M.lookup (energyIdx usec rec uid vid) e),
                          (XLine uid vid, M.lookup (xIdx usec rec uid vid) x),
                          (ELine vid uid, M.lookup (energyIdx vsec rec vid uid) e) ]
          where NLabel usec _ _ = fromJust $ lab g uid
                NLabel vsec _ _ = fromJust $ lab g vid
                ndirlab WithDir = (NLine uid vid, M.lookup (etaIdx vsec rec vid uid) fn)
                ndirlab _ = (NLine vid uid, M.lookup (etaIdx usec rec uid vid) fn)
        mkLst _ _ = [ (ErrorLine "Problem with record number", Nothing) ]


instance One (Term a) where one = error "EqTerm 1"
instance One Char where one = error "Char 1"

instance ToIndex a => DrawTopologyList (Term a) where
   formatStContList (Just ys) = showEqTerms ys
   formatStContList Nothing = [heart]
   formatDTimeList = showEqTerms

instance ToIndex a => DrawDeltaTopologyList (Term a) where
   drawDeltaTopologyList topo env =
      drawDeltaTopology' f formatStCont topo (envDeltaTopology tshow env [one])
           where -- f (x, Just ys) = showDelta x ++ " = [ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
                 f (x, Just ys) = showDelta x ++ " = \n" ++ showEqTerms ys

                 f (x, Nothing) = showDelta x ++ " = " ++ [heart]
                 formatStCont (Just ys) = "[ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
                 formatStCont Nothing = [heart]

                 showDelta (ELine u v) = "de_" ++ show u ++ "_" ++ show v
                 showDelta (XLine u v) = "dx_" ++ show u ++ "_" ++ show v
                 showDelta (NLine u v) = "dn_" ++ show u ++ "_" ++ show v
                 showDelta (ErrorLine str) = str
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
   Env EnergyIdx XIdx FEtaIdx a
envAbsTopology tshow (Interp.Envs rec e _de _p _dp fn _dn dt x _dx _v st) etaArg =
   Env rec EnergyIdx e XIdx x FEtaIdx (fmap ($etaArg) fn)
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

drawAbsTopology ::
   ((Line, Maybe a) -> String) ->
   (Maybe a -> String) ->
   Topology' NLabel ELabel ->
   Env EnergyIdx XIdx FEtaIdx a ->
   IO ()
drawAbsTopology f content (Topology g)
   (Env rec0 energyIdx e xIdx x etaIdx fn tshow nshow) =
      printGraph g (Just rec0) tshow (nshow content) eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst rec0 ps

        mkLst (SingleRecord rec) (uid, vid, l)
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (energyIdx usec rec uid vid) e),
                                 (XLine uid vid, M.lookup (xIdx usec rec uid vid) x),
                                 ndirlab (flowDirection l),
                                 (XLine vid uid, M.lookup (xIdx vsec rec vid uid) x),
                                 (ELine vid uid, M.lookup (energyIdx vsec rec vid uid) e) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (energyIdx vsec rec vid uid) e) ]
          | otherwise = [ (ELine uid vid, M.lookup (energyIdx usec rec uid vid) e),
                          (XLine uid vid, M.lookup (xIdx usec rec uid vid) x),
                          (ELine vid uid, M.lookup (energyIdx vsec rec vid uid) e) ]
          where NLabel usec _ _ = fromJust $ lab g uid
                NLabel vsec _ _ = fromJust $ lab g vid
                ndirlab WithDir = (NLine uid vid, M.lookup (etaIdx usec rec uid vid) fn)
                ndirlab _ = (NLine vid uid, M.lookup (etaIdx vsec rec vid uid) fn)
        mkLst _ _ = [ (ErrorLine "Problem with record number", Nothing) ]


envDeltaTopology ::
   (DTimeMap a -> DTimeIdx -> String) ->
   Interp.Envs a -> a ->
   Env DEnergyIdx DXIdx DEtaIdx a
envDeltaTopology tshow (Interp.Envs rec _e de _p _dp _fn dn dt _x dx _v st) etaArg =
   Env rec DEnergyIdx de DXIdx dx DEtaIdx (fmap ($etaArg) dn)
      (tshow dt)
      (showNode rec st)


drawDeltaTopology' ::
   ((Line, Maybe a) -> String) ->
   (Maybe a -> String) ->
   Topology' NLabel ELabel ->
   Env DEnergyIdx DXIdx DEtaIdx a ->
   IO ()
drawDeltaTopology' f content (Topology g)
   (Env rec0 dEnergyIdx de dXIdx dx dEtaIdx dn tshow nshow) =
      printGraph g (Just rec0) tshow (nshow content) eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst rec0 ps

        mkLst (SingleRecord rec) (uid, vid, l)
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (dEnergyIdx usec rec uid vid) de),
                                 (XLine uid vid, M.lookup (dXIdx usec rec uid vid) dx),
                                 ndirlab (flowDirection l),
                                 (XLine vid uid, M.lookup (dXIdx vsec rec vid uid) dx),
                                 (ELine vid uid, M.lookup (dEnergyIdx vsec rec vid uid) de) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (dEnergyIdx vsec rec vid uid) de) ]
          | otherwise = [ (ELine uid vid, M.lookup (dEnergyIdx usec rec uid vid) de),
                          (XLine uid vid, M.lookup (dXIdx usec rec uid vid) dx),
                          (ELine vid uid, M.lookup (dEnergyIdx vsec rec vid uid) de) ]
          where NLabel usec _ _ = fromJust $ lab g uid
                NLabel vsec _ _ = fromJust $ lab g vid
                ndirlab WithDir = (NLine uid vid, M.lookup (dEtaIdx usec rec uid vid) dn)
                ndirlab _ = (NLine vid uid, M.lookup (dEtaIdx vsec rec vid uid) dn)
        mkLst _ _ = [ (ErrorLine "Problem with record number", Nothing) ]



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
      drawAbsTopologySignal
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
            drawDeltaTopologyD f formatStContSignal topo (envDeltaTopologySignal env)
           where -- f (x, Just ys) = showDelta x ++ " = [ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
                 f (x, ys) = showDelta x ++ " = " ++ formatStContSignal ys

                 showDelta (ELine u v) = "de_" ++ show u ++ "_" ++ show v
                 showDelta (XLine u v) = "dx_" ++ show u ++ "_" ++ show v
                 showDelta (NLine u v) = "dn_" ++ show u ++ "_" ++ show v
                 showDelta (ErrorLine str) = str

instance
   (DispApp s, TDisp t, DrawDeltaTopologySignal a) =>
      DrawDeltaTopology (TC s t a) where
   drawDeltaTopology = drawDeltaTopologySignal


envAbsTopologySignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   Interp.Envs (TC s t (Data v d)) ->
   Env EnergyIdx XIdx FEtaIdx (TC s t (Data v d))
envAbsTopologySignal (Interp.Envs rec0 e _de _p _dp fn _dn dt x _dx _v st) =
   Env rec0
      EnergyIdx e XIdx x
      FEtaIdx
      (M.intersectionWith ($) fn $ M.mapKeys (\(EnergyIdx sec rec uid vid) -> FEtaIdx sec rec uid vid) e)
      (\dtimeIdx -> formatStContSignal $ M.lookup dtimeIdx dt)
      (showNode rec0 st)

drawAbsTopologySignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   ((Line, Maybe (TC s t (Data v d))) -> String) ->
   (Maybe (TC s t (Data v d)) -> String) ->
   Topology ->
   Env EnergyIdx XIdx FEtaIdx (TC s t (Data v d)) ->
   IO ()
drawAbsTopologySignal f content (Topology g)
   (Env rec0 energyIdx e xIdx x etaIdx fn tshow nshow) =
      printGraph g (Just rec0) tshow (nshow content) eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst rec0 ps

        mkLst (SingleRecord rec) (uid, vid, l)
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (energyIdx usec rec uid vid) e),
                                 (XLine uid vid, M.lookup (xIdx usec rec uid vid) x),
                                 ndirlab (flowDirection l),
                                 (XLine vid uid, M.lookup (xIdx vsec rec vid uid) x),
                                 (ELine vid uid, M.lookup (energyIdx vsec rec vid uid) e) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (energyIdx vsec rec vid uid) e) ]
          | otherwise = [ (ELine uid vid, M.lookup (energyIdx usec rec uid vid) e),
                          (XLine uid vid, M.lookup (xIdx usec rec uid vid) x),
                          (ELine vid uid, M.lookup (energyIdx vsec rec vid uid) e) ]
          where NLabel usec _ _ = fromJust $ lab g uid
                NLabel vsec _ _ = fromJust $ lab g vid
                ndirlab WithDir = (NLine uid vid, M.lookup (etaIdx usec rec uid vid) fn)
                ndirlab _ = (NLine vid uid, M.lookup (etaIdx vsec rec vid uid) fn)
        mkLst _ _ = [ (ErrorLine "No single record number!", Nothing) ]


envDeltaTopologySignal ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   Interp.Envs (TC s t (Data v d)) ->
   Env DEnergyIdx DXIdx DEtaIdx (TC s t (Data v d))
envDeltaTopologySignal (Interp.Envs rec0 _e de _p _dp _fn dn dt _x dx _v st) =
   Env rec0
      DEnergyIdx de DXIdx dx
      DEtaIdx
      (M.intersectionWith ($) dn $ M.mapKeys (\(DEnergyIdx sec rec uid vid) -> DEtaIdx sec rec uid vid) de)
      (\dtimeIdx -> formatStContSignal $ M.lookup dtimeIdx dt)
      (showNode rec0 st)

drawDeltaTopologyD ::
   (DispApp s, TDisp t, SDisplay v, D.Storage v d, Ord d, Disp d) =>
   ((Line, Maybe (TC s t (Data v d))) -> String) ->
   (Maybe (TC s t (Data v d)) -> String) ->
   Topology ->
   Env DEnergyIdx DXIdx DEtaIdx (TC s t (Data v d)) ->
   IO ()
drawDeltaTopologyD f content (Topology g)
   (Env rec0 dEnergyIdx de dXIdx dx dEtaIdx dn tshow nshow) =
      printGraph g (Just rec0) tshow (nshow content) eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst rec0 ps

        mkLst (SingleRecord rec) (uid, vid, l)
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (dEnergyIdx usec rec uid vid) de),
                                 (XLine uid vid, M.lookup (dXIdx usec rec uid vid) dx),
                                 ndirlab (flowDirection l),
                                 (XLine vid uid, M.lookup (dXIdx vsec rec vid uid) dx),
                                 (ELine vid uid, M.lookup (dEnergyIdx vsec rec vid uid) de) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (dEnergyIdx vsec rec vid uid) de) ]
          | otherwise = [ (ELine uid vid, M.lookup (dEnergyIdx usec rec uid vid) de),
                          (XLine uid vid, M.lookup (dXIdx usec rec uid vid) dx),
                          (ELine vid uid, M.lookup (dEnergyIdx vsec rec vid uid) de) ]
          where NLabel usec _ _ = fromJust $ lab g uid
                NLabel vsec _ _ = fromJust $ lab g vid
                ndirlab WithDir = (NLine uid vid , M.lookup (dEtaIdx usec rec uid vid) dn)
                ndirlab _ = (NLine vid uid, M.lookup (dEtaIdx vsec rec vid uid) dn)
        mkLst _ _ = [ (ErrorLine "No single record number!", Nothing) ]



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
