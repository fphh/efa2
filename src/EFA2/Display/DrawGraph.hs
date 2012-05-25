{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances, MultiParamTypeClasses #-}

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


mkDotGraph :: EfaGraph NLabel ELabel -> (LNode NLabel -> String) -> (LEdge ELabel -> String) -> DotGraph Int
mkDotGraph g nshow eshow =
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
        sg ns@(x:_, _) = DotSG True (Just (Int sl)) (ds sl rl ns)
          where sl = sectionNLabel $ snd x
                rl = recordNLabel $ snd x
        ds sl rl (ns, ms) = DotStmts gattrs [] xs ys
          where xs = map (mkDotNode nshow) ns
                ys = map (mkDotEdge eshow) (labEdges (delNodes (map fst (concat ms)) g'))
                gattrs = [GraphAttrs [Label (StrLabel (T.pack str))]]
                str = "Section " ++ show sl ++ " / " ++ "Record " ++ show rl
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

printGraph :: EfaGraph NLabel ELabel -> (LNode NLabel -> String) -> (LEdge ELabel -> String) -> IO ()
printGraph g nshow eshow = runGraphvizCanvas Dot (mkDotGraph g nshow eshow) Xlib

drawTopologyX' :: Topology -> IO ()
drawTopologyX' topo = printGraph g show show -- runGraphvizCanvas Dot (mkDotGraph g (show, show)) Xlib
  where g = unTopology topo

{-
drawFlowTop :: FlowTopology -> IO ()
drawFlowTop (FlowTopology g) = printGraph g show show -- runGraphvizCanvas Dot (mkDotGraph g (show, show)) Xlib

drawSequFlowTops :: SequFlowTops -> IO ()
drawSequFlowTops (SequData flowTops) = mapM_ drawFlowTop flowTops
r-}


{-
drawTopologyX :: TheGraph a -> IO ()
drawTopologyX (TheGraph g _) = printGraph g show show
-}

data Line = ELine Int Int
          | XLine Int Int
          | NLine Int Int deriving (Eq, Ord)


instance Show Line where
         show (ELine u v) = "e_" ++ show u ++ "_" ++ show v
         show (XLine u v) = "x_" ++ show u ++ "_" ++ show v
         show (NLine u v) = "n_" ++ show u ++ "_" ++ show v




-- The argument t is for node labels. Until now, it is not used.
class DrawTopology a where
      drawTopology :: Topology -> Envs a ->  IO ()

instance DrawTopology [Double] where
         drawTopology = drawAbsTopology f formatStCont
           where f (x, Just ys) = show x ++ " = " ++ (concatMap (printf "%.6f    ") ys)
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = concatMap (printf "%.6f    ") ys
                 formatStCont Nothing = "♥"

instance (Integral a) => DrawTopology [Ratio a] where
         drawTopology = drawAbsTopology f formatStCont
           where f (x, Just ys) = show x ++ " = " ++ (concatMap show ys)
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = concatMap show ys
                 formatStCont Nothing = "♥"

instance DrawTopology [InTerm Val] where
         drawTopology = drawAbsTopology f formatStCont
           where f (x, Just ys) = show x ++ " = " ++ (concatMap showInTerm ys)
                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = concatMap showInTerm ys
                 formatStCont Nothing = "♥"


drawAbsTopology :: (Show a, Num a, Ord a) => ((Line, Maybe [a]) -> String) -> (Maybe [a] -> String) -> Topology -> Envs [a] ->  IO ()
drawAbsTopology f content (Topology g) (Envs e de p dp fn dn t x v st) = printGraph g nshow eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst ps
        nshow (num, NLabel sec rec nid ty) = 
          "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\n" ++
          "Type: " ++ show ty ++ stContent ty
            where stContent (InitStorage n) = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent (Storage n) = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent _ = ""

        node n = nodeNLabel (fromJust (lab g n))
        mkLst (uid, vid, l) 
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (EnergyIdx usec urec uid vid) e), 
                                 (XLine uid vid, M.lookup (XIdx usec urec uid vid) x),
                                 (NLine uid vid, M.lookup (FEtaIdx usec urec uid vid) fn <*> Just [1]),
                                 (NLine vid uid, M.lookup (FEtaIdx vsec vrec vid uid) fn <*> Just [1]),
                                 (XLine vid uid, M.lookup (XIdx vsec vrec vid uid) x),
                                 (ELine vid uid, M.lookup (EnergyIdx vsec vrec vid uid) e) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (EnergyIdx vsec vrec vid uid) e) ]
          | otherwise = [ (ELine uid vid, M.lookup (EnergyIdx usec urec uid vid) e),
                          (XLine uid vid, M.lookup (XIdx usec urec uid vid) x),
                          (ELine vid uid, M.lookup (EnergyIdx vsec vrec vid uid) e) ]
          where NLabel usec urec _ _ = fromJust $ lab g uid
                NLabel vsec vrec _ _ = fromJust $ lab g vid


instance DrawTopology UTFSig where
         drawTopology = drawAbsTopology' f formatStCont
           where f (x@(ELine _ _), Just (TC ys)) = show x ++ " = " ++ sdisp (TC ys :: FSig)
                 f (x@(XLine _ _), Just (TC ys)) = show x ++ " = " ++ sdisp (TC ys :: FSig1 (Typ A X Tt) Val)
                 f (x@(NLine _ _), Just (TC ys)) = show x ++ " = " ++ sdisp (TC ys :: FSig1 (Typ A N Tt) Val)

                 f (x, Nothing) = show x ++ " = ♥"
                 formatStCont (Just ys) = sdisp ys
                 formatStCont Nothing = "♥"


drawAbsTopology' :: ((Line, Maybe UTFSig) -> String) -> (Maybe UTFSig -> String) -> Topology -> Envs UTFSig ->  IO ()
drawAbsTopology' f content (Topology g) (Envs e de p dp fn dn t x v st) = printGraph g nshow eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst ps
        nshow (num, NLabel sec rec nid ty) = 
          "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\n" ++
          "Type: " ++ show ty ++ stContent ty
            where stContent (InitStorage n) = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent (Storage n) = "\nContent: " ++ content (M.lookup (StorageIdx sec rec n) st)
                  stContent _ = ""

        node n = nodeNLabel (fromJust (lab g n))
        mkLst (uid, vid, l) 
          | isOriginalEdge l = [ (ELine uid vid, M.lookup (EnergyIdx usec urec uid vid) e), 
                                 (XLine uid vid, M.lookup (XIdx usec urec uid vid) x),
                                 (NLine uid vid, M.lookup (FEtaIdx usec urec uid vid) fn <*> e1),
                                 (NLine vid uid, M.lookup (FEtaIdx vsec vrec vid uid) fn <*> e2),
                                 (XLine vid uid, M.lookup (XIdx vsec vrec vid uid) x),
                                 (ELine vid uid, M.lookup (EnergyIdx vsec vrec vid uid) e) ]
          | isInnerStorageEdge l = [ (ELine vid uid, M.lookup (EnergyIdx vsec vrec vid uid) e) ]
          | otherwise = [ (ELine uid vid, M.lookup (EnergyIdx usec urec uid vid) e),
                          (XLine uid vid, M.lookup (XIdx usec urec uid vid) x),
                          (ELine vid uid, M.lookup (EnergyIdx vsec vrec vid uid) e) ]
          where NLabel usec urec _ _ = fromJust $ lab g uid
                NLabel vsec vrec _ _ = fromJust $ lab g vid
                e1 = M.lookup (EnergyIdx usec urec uid vid) e
                e2 = M.lookup (EnergyIdx vsec vrec vid uid) e

{-
instance DrawTopology InTerm where
         drawTopology = drawAbsTopology f
           where f (PLine, es) = "p = " ++ (L.intercalate " | " $ map showInTerm es)
                 f (XLine, e:_) = "x = " ++ showInTerm e
                 f (NLine, e:_) = "n = " ++ showInTerm e
 
-}

{-
instance DrawTopology InTerm where
         drawTopology = drawDiffTopology f
           where f (PLine, es) = "p = " ++ (L.intercalate " | " $ map showInTerm es)
                 f (XLine, e:_) = "x = " ++ showInTerm e
                 f (NLine, e:_) = "n = " ++ showInTerm e
 -}

{-
drawDiffTopology :: (Arith a, Show a) => ((Line, [a]) -> String) -> t -> TheGraph [a] -> DiffEnv [a] -> M.Map EnergyIdx [a] ->  IO ()
drawDiffTopology f nenv (TheGraph g _) (DiffEnv dpenv deenv eenv xenv) penv = printGraph g show eshow
  where penv' = mkEnv $ mkEnergyEnv penv
        eshow ps = L.intercalate "\n" $ map f $ mkLst ps
        mkLst (x, y) = [ (PLine, penv' (EnergyIdx x y)), 
                         (XLine, xenv (XIdx x y)),
                         (NLine, eenv (EtaIdx x y)),
                         (XLine, xenv (XIdx y x)),
                         (PLine, penv' (EnergyIdx y x)) ]

-}

{-
drawDependencyGraph :: Gr EqTerm () -> IO ()
drawDependencyGraph g = printGraph g nshow (const "")
  where m = M.fromList $ labNodes g
        nshow (x, _) = show x ++ ": " ++ showEqTerm (m M.! x)
       -- x = nmap nshow g
-}

{-
drawDependencyGraphTransClose :: TheGraph t -> [(EnergyIdx, b)] -> IO ()
drawDependencyGraphTransClose theGraph@(TheGraph g _) given = printGraph (transClose g') nshow (const "")
  where gvs = give $ map (Energy . fst) given
        g' = makeDependencyGraph theGraph gvs
        m = M.fromList $ labNodes g'
        nshow x = show x ++ ": " ++ showEqTerm (m M.! x)

-}


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

