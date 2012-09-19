{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts #-}

module EFA2.Display.DrawGraph where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.HT as HTL

import Data.Ratio

import Data.Maybe
--import Data.Graph.Inductive
import qualified Data.Text.Lazy as T
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import Control.Concurrent
import Control.Applicative

import Text.Printf

import EFA2.Solver.Equation
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.InTerm
import EFA2.Interpreter.Env
import EFA2.Topology.TopologyData
import EFA2.Topology.EfaGraph

import EFA2.Display.DispSignal
import EFA2.Signal.Signal (TC(TC), Sc, UTFSig)
import EFA2.Signal.Base

import EFA2.Utils.Utils

nodeColour :: Attribute 
nodeColour = FillColor [RGB 230 230 240]

clusterColour :: Attribute
clusterColour = FillColor [RGB 250 250 200]

originalEdgeColour :: Attribute
originalEdgeColour = Color [RGB 0 0 200]

intersectionEdgeColour :: Attribute
intersectionEdgeColour = Color [RGB 200 0 0]

-- coding
noRecord :: RecordNumber
noRecord = SingleRecord (-99)


mkDotGraph :: EfaGraph NLabel ELabel -> RecordNumber -> (Int -> Int -> String) -> (LNode NLabel -> String) -> (LEdge ELabel -> String) -> DotGraph Int
mkDotGraph g (MixedRecord _) timef nshow eshow = mkDotGraph g noRecord timef nshow eshow
mkDotGraph g (NoRecord) timef nshow eshow = mkDotGraph g noRecord timef nshow eshow
mkDotGraph g (SingleRecord recordNum) timef nshow eshow =
  DotGraph { strictGraph = False,
             directedGraph = True,
             graphID = Just (Int 1),
             graphStatements = stmts }
  where es = labEdges g
        (interEs, origEs) = L.partition (\(_, _, e) -> isIntersectionEdge e) es
        g' = delEdges (map (\(x, y, _) -> (x, y)) interEs) g
        cs = HTL.removeEach (L.groupBy sameSection (labNodes g))
        sameSection (_, l1) (_, l2) = sectionNLabel l1 == sectionNLabel l2
        comps = map sg cs
        sg ns@(x:_, _) = DotSG True (Just (Int sl)) (ds sl recordNum ns)
          where sl = sectionNLabel $ snd x
        ds sl rl (ns, ms) = DotStmts gattrs [] xs ys
          where xs = map (mkDotNode nshow) ns
                ys = map (mkDotEdge eshow) (labEdges (delNodes (map fst (concat ms)) g'))
                gattrs = [GraphAttrs [Label (StrLabel (T.pack str))]]
                str = "Section " ++ show sl ++ " / Record " ++ show recordNum ++ " / Time " ++ timef sl recordNum
        stmts = DotStmts { attrStmts = [],
                           subGraphs = comps,
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

printGraph :: EfaGraph NLabel ELabel -> RecordNumber -> (Int -> Int -> String) -> (LNode NLabel -> String) -> (LEdge ELabel -> String) -> IO ()
printGraph g recordNum tshow nshow eshow = runGraphvizCanvas Dot (mkDotGraph g recordNum tshow nshow eshow) Xlib
{-
printGraph g recordNum tshow nshow eshow = do
  runGraphvizCommand Dot (mkDotGraph g recordNum tshow nshow eshow) XDot "result/graph.dot"
  return ()
-}

drawTopologyX' :: Topology -> IO ()
drawTopologyX' topo = printGraph g noRecord (const2 "♥") show show
  where g = unTopology topo


drawTopologySimple :: Topology -> IO ()
drawTopologySimple topo = printGraph g noRecord (const2 "♥") nshow eshow
  where g = unTopology topo
        nshow (n, l) = show n ++ " - " ++ show (nodetypeNLabel l)
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


-- provide a mock for '1' also for non-Num types
-- FIXME: this is still a hack because 'one' is often undefined
class One a where
  one :: a

-- The argument t is for node labels. Until now, it is not used.
class DrawTopology a where
      drawTopology :: Topology -> Envs a -> IO ()

class DrawTopology a => DrawDeltaTopology a where
      drawDeltaTopology :: Topology -> Envs a -> IO ()


instance One Double where one = 1

instance DrawTopology [Double] where
         drawTopology = drawAbsTopology f formatStCont tshow
           where f (x, Just ys) = show x ++ " = " ++ (concatMap (printf "%.6f    ") ys)
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = concatMap (printf "%.6f    ") ys
                 formatStCont Nothing = "♥"
                 tshow dt s r = show $ dt `safeLookup` (DTimeIdx s r)

instance DrawDeltaTopology [Double] where
         drawDeltaTopology = drawDeltaTopology' f formatStCont tshow
           where -- f (x, Just ys) = showDelta x ++ " = [ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
                 f (x, Just ys) = showDelta x ++ " = \n" ++  L.intercalate "\n" (map show ys)

                 f (x, Nothing) = showDelta x ++ " = ♥"
                 formatStCont (Just ys) = "[ " ++ L.intercalate ", " (map show ys) ++ " ]"
                 formatStCont Nothing = "♥"
                 --tshow dt s r = showEqTerm $ dt `safeLookup` (DTimeIdx s r)

                 showDelta (ELine u v) = "de_" ++ show u ++ "_" ++ show v
                 showDelta (XLine u v) = "dx_" ++ show u ++ "_" ++ show v
                 showDelta (NLine u v) = "dn_" ++ show u ++ "_" ++ show v
                 showDelta (ErrorLine str) = str
                 tshow dt s r = show $ dt `safeLookup` (DTimeIdx s r)

instance (Integral a) => One (Ratio a) where one = 1

instance (Integral a, Show a) => DrawTopology [Ratio a] where
         drawTopology = drawAbsTopology f formatStCont tshow
           where f (x, Just ys) = show x ++ " = " ++ (concatMap show ys)
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = concatMap show ys
                 formatStCont Nothing = "♥"
                 tshow dt s r = show $ dt `safeLookup` (DTimeIdx s r)

instance DrawTopology String where
         drawTopology = drawAbsTopology f formatStCont tshow
           where f (x, Just ys) = show x ++ " = " ++ ys
                 f (x, Nothing) = show x ++ " = +"
                 formatStCont (Just ys) = ys
                 formatStCont Nothing = "+"
                 tshow dt s r = dt `safeLookup` (DTimeIdx s r)

instance One LatexString where
  one = error "LatexString 1"

instance DrawTopology [LatexString] where
         drawTopology = drawAbsTopologyLatex f formatStCont tshow
           where f (x, Just ys) = showX x ++ " = " ++ unLatexString (head ys)
                 f (x, Nothing) = showX x ++ " = +"
                 formatStCont (Just ys) = unLatexString (head ys)
                 formatStCont Nothing = "+"
                 tshow dt s r = unLatexString $ head $ dt `safeLookup` (DTimeIdx s r)

                 showX (ELine u v) = "$e_{" ++ show u ++ "." ++ show v ++ "}$"
                 showX (XLine u v) = "$x_{" ++ show u ++ "." ++ show v ++ "}$"
                 showX (NLine u v) = "$n_{" ++ show u ++ "." ++ show v ++ "}$"
                 showX (ErrorLine str) = str


drawAbsTopologyLatex ::
  One t =>
  ((Line, Maybe [t]) -> String) ->
  (Maybe [t] -> String) ->
  (DTimeMap [t] -> Int -> Int -> String) ->
  Topology' NLabel ELabel ->
  Envs [t] ->
  IO ()
drawAbsTopologyLatex f content tshow (Topology g) (Envs rec e de p dp fn dn dt x dx v st) = printGraph g rec (tshow dt) nshow eshow
  where eshow ps = L.intercalate "\n " $ map f $ mkLst rec ps
        --tshow' = tshow dt
        nshow (num, NLabel sec nid ty) = 
          "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\\\\ " ++
          "Type: " ++ show ty ++ stContent rec ty
            where stContent (SingleRecord rec) (InitStorage n) 
                    = "\\\\ Content: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent (SingleRecord rec) (Storage n) = "\\\\ Content: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent rn (InitStorage n) = "\\\\ Content: Problem with record number: " ++ show rn
                  stContent rn (Storage n) = "\\\\ Content: Problem with record number: " ++ show rn
                  stContent _ _ = ""

        node n = nodeNLabel (fromJust (lab g n))
        mkLst (SingleRecord rec) (uid, vid, l)
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (EnergyIdx usec rec uid vid) e), 
                                 (XLine uid vid, M.lookup (XIdx usec rec uid vid) x),
                                 ndirlab (flowDirection l),
                                 (XLine vid uid, M.lookup (XIdx vsec rec vid uid) x),
                                 (ELine vid uid, M.lookup (EnergyIdx vsec rec vid uid) e) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (EnergyIdx vsec rec vid uid) e) ]
          | otherwise = [ (ELine uid vid, M.lookup (EnergyIdx usec rec uid vid) e),
                          (XLine uid vid, M.lookup (XIdx usec rec uid vid) x),
                          (ELine vid uid, M.lookup (EnergyIdx vsec rec vid uid) e) ]
          where NLabel usec _ _ = fromJust $ lab g uid
                NLabel vsec _ _ = fromJust $ lab g vid
                ndirlab WithDir = (NLine uid vid, M.lookup (FEtaIdx vsec rec vid uid) fn <*> Just [one])
                ndirlab _ = (NLine vid uid, M.lookup (FEtaIdx usec rec uid vid) fn <*> Just [one])
        mkLst _ _ = [ (ErrorLine "Problem with record number", Nothing) ]



instance One (InTerm Val) where one = 1

instance DrawTopology [InTerm Val] where
         drawTopology = drawAbsTopology f formatStCont tshow
           where f (x, Just ys) = show x ++ " = " ++ (concatMap showInTerm ys)
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = concatMap showInTerm ys
                 formatStCont Nothing = "♥"
                 tshow dt s r = showInTerms $ dt `safeLookup` (DTimeIdx s r)

instance One EqTerm where one = error "EqTerm 1"
instance One Char where one = error "Char 1"

instance DrawTopology [EqTerm] where
         drawTopology = drawAbsTopology f formatStCont tshow
           where f (x, Just ys) = show x ++ " = " ++ showEqTerms ys
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = showEqTerms ys
                 formatStCont Nothing = "♥"
                 tshow dt s r = showEqTerms $ dt `safeLookup` (DTimeIdx s r)

instance DrawDeltaTopology [EqTerm] where
         drawDeltaTopology = drawDeltaTopology' f formatStCont tshow
           where -- f (x, Just ys) = showDelta x ++ " = [ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
                 f (x, Just ys) = showDelta x ++ " = \n" ++ showEqTerms ys

                 f (x, Nothing) = showDelta x ++ " = ♥"
                 formatStCont (Just ys) = "[ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
                 formatStCont Nothing = "♥"
                 --tshow dt s r = showEqTerm $ dt `safeLookup` (DTimeIdx s r)

                 showDelta (ELine u v) = "de_" ++ show u ++ "_" ++ show v
                 showDelta (XLine u v) = "dx_" ++ show u ++ "_" ++ show v
                 showDelta (NLine u v) = "dn_" ++ show u ++ "_" ++ show v
                 showDelta (ErrorLine str) = str
                 tshow dt s r = showEqTerms $ dt `safeLookup` (DTimeIdx s r)


drawAbsTopology ::
  One t =>
  ((Line, Maybe [t]) -> String) ->
  (Maybe [t] -> String) ->
  (DTimeMap [t] -> Int -> Int -> String) ->
  Topology' NLabel ELabel ->
  Envs [t] ->
  IO ()
drawAbsTopology f content tshow (Topology g) (Envs rec e de p dp fn dn dt x dx v st) = printGraph g rec (tshow dt) nshow eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst rec ps
        --tshow' = tshow dt
        nshow (num, NLabel sec nid ty) = 
          "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\n" ++
          "Type: " ++ show ty ++ stContent rec ty
            where stContent (SingleRecord rec) (InitStorage n) 
                    = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent (SingleRecord rec) (Storage n) = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent rn (InitStorage n) = "\nContent: Problem with record number: " ++ show rn
                  stContent rn (Storage n) = "\nContent: Problem with record number: " ++ show rn
                  stContent _ _ = ""

        node n = nodeNLabel (fromJust (lab g n))
        mkLst (SingleRecord rec) (uid, vid, l)
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (EnergyIdx usec rec uid vid) e), 
                                 (XLine uid vid, M.lookup (XIdx usec rec uid vid) x),
                                 ndirlab (flowDirection l),
                                 (XLine vid uid, M.lookup (XIdx vsec rec vid uid) x),
                                 (ELine vid uid, M.lookup (EnergyIdx vsec rec vid uid) e) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (EnergyIdx vsec rec vid uid) e) ]
          | otherwise = [ (ELine uid vid, M.lookup (EnergyIdx usec rec uid vid) e),
                          (XLine uid vid, M.lookup (XIdx usec rec uid vid) x),
                          (ELine vid uid, M.lookup (EnergyIdx vsec rec vid uid) e) ]
          where NLabel usec _ _ = fromJust $ lab g uid
                NLabel vsec _ _ = fromJust $ lab g vid
                ndirlab WithDir = (NLine uid vid, M.lookup (FEtaIdx vsec rec vid uid) fn <*> Just [one])
                ndirlab _ = (NLine vid uid, M.lookup (FEtaIdx usec rec uid vid) fn <*> Just [one])
        mkLst _ _ = [ (ErrorLine "Problem with record number", Nothing) ]


--drawDeltaTopology' :: (Show a, Num a, Ord a) => ((Line, Maybe [a]) -> String) -> (Maybe [a] -> String) -> Topology -> Envs [a] -> IO ()
drawDeltaTopology' f content tshow (Topology g) (Envs rec e de p dp fn dn dt x dx v st) = printGraph g rec (tshow dt) nshow eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst rec ps
        -- tshow s r = show $ dt `safeLookup` (DTimeIdx s r)
        nshow (num, NLabel sec nid ty) = 
          "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\n" ++
          "Type: " ++ show ty ++ stContent rec ty
            where stContent (SingleRecord rec) (InitStorage n) 
                    = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent (SingleRecord rec) (Storage n) = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent rn (InitStorage n) = "\nContent: Problem with record number: " ++ show rn
                  stContent rn (Storage n) = "\nContent: Problem with record number: " ++ show rn
                  stContent _ _ = ""

        node n = nodeNLabel (fromJust (lab g n))
        mkLst (SingleRecord rec) (uid, vid, l)
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (DEnergyIdx usec rec uid vid) de), 
                                 (XLine uid vid, M.lookup (DXIdx usec rec uid vid) dx),
                                 ndirlab (flowDirection l),
                                 (XLine vid uid, M.lookup (DXIdx vsec rec vid uid) dx),
                                 (ELine vid uid, M.lookup (DEnergyIdx vsec rec vid uid) de) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (DEnergyIdx vsec rec vid uid) de) ]
          | otherwise = [ (ELine uid vid, M.lookup (DEnergyIdx usec rec uid vid) de),
                          (XLine uid vid, M.lookup (DXIdx usec rec uid vid) dx),
                          (ELine vid uid, M.lookup (DEnergyIdx vsec rec vid uid) de) ]
          where NLabel usec _ _ = fromJust $ lab g uid
                NLabel vsec _ _ = fromJust $ lab g vid
                ndirlab WithDir = (NLine uid vid, M.lookup (DEtaIdx vsec rec vid uid) dn <*> Just [one])
                ndirlab _ = (NLine vid uid, M.lookup (DEtaIdx usec rec uid vid) dn <*> Just [one])
        mkLst _ _ = [ (ErrorLine "Problem with record number", Nothing) ]



instance DrawTopology UTFSig where
         drawTopology = drawAbsTopology' f formatStCont
           where f (x@(ELine _ _), Just (TC ys)) = show x ++ " = " ++ sdisp (TC ys :: UTFSig)
                 f (x@(XLine _ _), Just (TC ys)) = show x ++ " = " ++ sdisp (TC ys :: UTFSig)
                 f (x@(NLine _ _), Just (TC ys)) = show x ++ " = " ++ sdisp (TC ys :: UTFSig)
                 f (ErrorLine str, _) = str
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = sdisp ys
                 formatStCont Nothing = "♥"


instance DrawTopology Sc where
         drawTopology = drawAbsTopology' f formatStCont
           where f (x@(ELine _ _), Just (TC ys)) = show x ++ " = " ++ sdisp (TC ys :: Sc)
                 f (x@(XLine _ _), Just (TC ys)) = show x ++ " = " ++ sdisp (TC ys :: Sc)
                 f (x@(NLine _ _), Just (TC ys)) = show x ++ " = " ++ sdisp (TC ys :: Sc)
                 f (ErrorLine str, _) = str
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = sdisp ys
                 formatStCont Nothing = "♥"

instance DrawDeltaTopology Sc where
         drawDeltaTopology = drawDeltaTopologyD f formatStCont
           where -- f (x, Just ys) = showDelta x ++ " = [ " ++ L.intercalate ", " (map showEqTerm ys) ++ " ]"
                 f (x, Just ys) = showDelta x ++ " = " ++ sdisp ys

                 f (x, Nothing) = showDelta x ++ " = ♥"
                 formatStCont (Just ys) = sdisp ys
                 formatStCont Nothing = "♥"
                 --tshow dt s r = showEqTerm $ dt `safeLookup` (DTimeIdx s r)

                 showDelta (ELine u v) = "de_" ++ show u ++ "_" ++ show v
                 showDelta (XLine u v) = "dx_" ++ show u ++ "_" ++ show v
                 showDelta (NLine u v) = "dn_" ++ show u ++ "_" ++ show v
                 showDelta (ErrorLine str) = str
                -- tshow dt s r = showEqTerms $ dt `safeLookup` (DTimeIdx s r)


drawAbsTopology' :: forall s t a. SDisplay (TC s t a) =>
                  ((Line, Maybe (TC s t a)) -> String) -> (Maybe (TC s t a) -> String) -> Topology -> Envs (TC s t a) ->  IO ()
drawAbsTopology' f content (Topology g) (Envs rec e de p dp fn dn dt x dx v st) = printGraph g rec tshow nshow eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst rec ps
        tshow s r = case M.lookup (DTimeIdx s r) dt of
                         Just (TC ts) -> sdisp (TC ts :: TC s t a)
                         Nothing -> "♥"
        nshow (num, NLabel sec nid ty) = 
          "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\n" ++
          "Type: " ++ show ty ++ stContent rec ty
            where stContent (SingleRecord rec) (InitStorage n) = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent (SingleRecord rec) (Storage n) = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent rn (InitStorage n) = "\nContent: Problem with record number: " ++ show rn
                  stContent rn (Storage n) = "\nContent: Problem with record number: " ++ show rn
                  stContent _ _ = ""

        node n = nodeNLabel (fromJust (lab g n))
        mkLst (SingleRecord rec) (uid, vid, l)
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (EnergyIdx usec rec uid vid) e), 
                                 (XLine uid vid, M.lookup (XIdx usec rec uid vid) x),
                                 ndirlab (flowDirection l),
                                 (XLine vid uid, M.lookup (XIdx vsec rec vid uid) x),
                                 (ELine vid uid, M.lookup (EnergyIdx vsec rec vid uid) e) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (EnergyIdx vsec rec vid uid) e) ]
          | otherwise = [ (ELine uid vid, M.lookup (EnergyIdx usec rec uid vid) e),
                          (XLine uid vid, M.lookup (XIdx usec rec uid vid) x),
                          (ELine vid uid, M.lookup (EnergyIdx vsec rec vid uid) e) ]
          where NLabel usec _ _ = fromJust $ lab g uid
                NLabel vsec _ _ = fromJust $ lab g vid
                e1 = M.lookup (EnergyIdx usec rec uid vid) e
                e2 = M.lookup (EnergyIdx vsec rec vid uid) e
                ndirlab WithDir = (NLine uid vid , M.lookup (FEtaIdx usec rec uid vid) fn <*> e2)
                ndirlab _ = (NLine vid uid, M.lookup (FEtaIdx vsec rec vid uid) fn <*> e1)
        mkLst _ _ = [ (ErrorLine "No single record number!", Nothing) ]


drawDeltaTopologyD :: forall s t a. (SDisplay (TC s t a), Show a) =>
                  ((Line, Maybe (TC s t a)) -> String) -> (Maybe (TC s t a) -> String) -> Topology -> Envs (TC s t a) ->  IO ()
drawDeltaTopologyD f content (Topology g) (Envs rec e de p dp fn dn dt x dx v st) = printGraph g rec tshow nshow eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst rec ps
        tshow s r = case M.lookup (DTimeIdx s r) dt of
                         Just (TC ts) -> sdisp (TC ts :: TC s t a)
                         Nothing -> "♥"
        nshow (num, NLabel sec nid ty) = 
          "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\n" ++
          "Type: " ++ show ty ++ stContent rec ty
            where stContent (SingleRecord rec) (InitStorage n) = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent (SingleRecord rec) (Storage n) = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent rn (InitStorage n) = "\nContent: Problem with record number: " ++ show rn
                  stContent rn (Storage n) = "\nContent: Problem with record number: " ++ show rn
                  stContent _ _ = ""

        node n = nodeNLabel (fromJust (lab g n))
        mkLst (SingleRecord rec) (uid, vid, l)
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (DEnergyIdx usec rec uid vid) de), 
                                 (XLine uid vid, M.lookup (DXIdx usec rec uid vid) dx),
                                 ndirlab (flowDirection l),
                                 (XLine vid uid, M.lookup (DXIdx vsec rec vid uid) dx),
                                 (ELine vid uid, M.lookup (DEnergyIdx vsec rec vid uid) de) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (DEnergyIdx vsec rec vid uid) de) ]
          | otherwise = [ (ELine uid vid, M.lookup (DEnergyIdx usec rec uid vid) de),
                          (XLine uid vid, M.lookup (DXIdx usec rec uid vid) dx),
                          (ELine vid uid, M.lookup (DEnergyIdx vsec rec vid uid) de) ]
          where NLabel usec _ _ = fromJust $ lab g uid
                NLabel vsec _ _ = fromJust $ lab g vid
                e1 = M.lookup (DEnergyIdx usec rec uid vid) de
                e2 = M.lookup (DEnergyIdx vsec rec vid uid) de
                ndirlab WithDir = (NLine uid vid , M.lookup (DEtaIdx usec rec uid vid) dn <*> e2)
                ndirlab _ = (NLine vid uid, M.lookup (DEtaIdx vsec rec vid uid) dn <*> e1)
        mkLst _ _ = [ (ErrorLine "No single record number!", Nothing) ]



-------------------------------------------------------------------------------------------

newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async io = do
  m <- newEmptyMVar
  forkIO $ do r <- io; putMVar m r
  return (Async m)

wait :: Async a -> IO a
wait (Async m) = readMVar m

drawAll :: [IO a] -> IO ()
drawAll ds = mapM async ds >>= mapM wait >> return ()

