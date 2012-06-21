{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances, MultiParamTypeClasses, ScopedTypeVariables, FlexibleContexts #-}

module EFA2.Display.DrawGraph where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.HT as HTL
import qualified Data.Vector.Unboxed as UV

import Data.Ratio
import Data.Function

import Data.Maybe
--import Data.Graph.Inductive
import qualified Data.Text.Lazy as T
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import Control.Concurrent
import Control.Exception
import Control.Applicative

import Text.Printf

import Debug.Trace


import EFA2.Solver.Equation
import EFA2.Solver.DependencyGraph
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.InTerm
import EFA2.Interpreter.Env
--import EFA2.Interpreter.Arith
import EFA2.Topology.TopologyData
import EFA2.Topology.EfaGraph
import EFA2.Topology.Topology

import EFA2.Signal.Signal
import EFA2.Signal.Typ
import EFA2.Display.DispSignal
import EFA2.Signal.Base
import EFA2.Signal.Data


import EFA2.Topology.Flow
--import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import EFA2.Utils.Utils

nodeColour :: Attribute 
nodeColour = FillColor (RGB 230 230 240)

clusterColour :: Attribute
clusterColour = FillColor (RGB 250 250 200)

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
        displabel | UnDir <- flowDir = Label $ StrLabel $ T.pack ""
                  | otherwise = Label $ StrLabel $ T.pack (eshow e)
        edir | AgainstDir <- flowDir = Dir Back
             | WithDir <- flowDir = Dir Forward
             | otherwise = Dir NoDir
        etype = edgeType elabel
        colour | IntersectionEdge <- etype = intersectionEdgeColour
               | otherwise = originalEdgeColour
        --colour = originalEdgeColour

printGraph :: EfaGraph NLabel ELabel -> RecordNumber -> (Int -> Int -> String) -> (LNode NLabel -> String) -> (LEdge ELabel -> String) -> IO ()
printGraph g recordNum tshow nshow eshow = runGraphvizCanvas Dot (mkDotGraph g recordNum tshow nshow eshow) Xlib

drawTopologyX' :: Topology -> IO ()
drawTopologyX' topo = printGraph g noRecord (const2 "♥") show show
  where g = unTopology topo

data Line = ELine Int Int
          | XLine Int Int
          | NLine Int Int
          | ErrorLine String deriving (Eq, Ord)

instance Show Line where
         show (ELine u v) = "e_" ++ show u ++ "_" ++ show v
         show (XLine u v) = "x_" ++ show u ++ "_" ++ show v
         show (NLine u v) = "n_" ++ show u ++ "_" ++ show v
         show (ErrorLine str) = str


-- The argument t is for node labels. Until now, it is not used.
class DrawTopology a where
      drawTopology :: Topology -> Envs a -> IO ()
      drawDeltaTopology :: Topology -> Envs a -> IO ()

instance DrawTopology [Double] where
         drawTopology = drawAbsTopology f formatStCont tshow
           where f (x, Just ys) = show x ++ " = " ++ (concatMap (printf "%.6f    ") ys)
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = concatMap (printf "%.6f    ") ys
                 formatStCont Nothing = "♥"
                 tshow dt s r = show $ dt `safeLookup` (DTimeIdx s r)

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

instance (Integral a) => DrawTopology [Ratio a] where
         drawTopology = drawAbsTopology f formatStCont tshow
           where f (x, Just ys) = show x ++ " = " ++ (concatMap show ys)
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = concatMap show ys
                 formatStCont Nothing = "♥"
                 tshow dt s r = show $ dt `safeLookup` (DTimeIdx s r)

instance DrawTopology [InTerm Val] where
         drawTopology = drawAbsTopology f formatStCont tshow
           where f (x, Just ys) = show x ++ " = " ++ (concatMap showInTerm ys)
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = concatMap showInTerm ys
                 formatStCont Nothing = "♥"
                 tshow dt s r = showInTerms $ dt `safeLookup` (DTimeIdx s r)

instance Num EqTerm

instance DrawTopology [EqTerm] where
         drawTopology = drawAbsTopology f formatStCont tshow
           where f (x, Just ys) = show x ++ " = " ++ showEqTerms ys
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = showEqTerms ys
                 formatStCont Nothing = "♥"
                 tshow dt s r = showEqTerms $ dt `safeLookup` (DTimeIdx s r)

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


--drawAbsTopology :: (Show a, Num a, Ord a) => ((Line, Maybe [a]) -> String) -> (Maybe [a] -> String) -> Topology -> Envs [a] -> IO ()
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
                ndirlab WithDir = (NLine uid vid, M.lookup (FEtaIdx vsec rec vid uid) fn <*> Just [1])
                ndirlab _ = (NLine vid uid, M.lookup (FEtaIdx usec rec uid vid) fn <*> Just [1])
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
                ndirlab WithDir = (NLine uid vid, M.lookup (DEtaIdx vsec rec vid uid) dn <*> Just [1])
                ndirlab _ = (NLine vid uid, M.lookup (DEtaIdx usec rec uid vid) dn <*> Just [1])
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
                 --formatStCont (Just ys) = sdisp ys
                 formatStCont Nothing = "♥"

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


drawDeltaTopologyD :: forall s t a. SDisplay (TC s t a) =>
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

