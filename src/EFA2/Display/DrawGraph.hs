{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances, MultiParamTypeClasses #-}

module EFA2.Display.DrawGraph where

import qualified Data.Map as M
import qualified Data.List as L

import Data.Ratio

import Data.Maybe
import Data.Graph.Inductive
import qualified Data.Text.Lazy as T
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import Control.Concurrent
import Control.Exception

import Text.Printf

import Debug.Trace


import EFA2.Solver.Equation
import EFA2.Solver.DependencyGraph
import EFA2.Interpreter.Interpreter
import EFA2.Interpreter.InTerm
import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith
import EFA2.Topology.TopologyData

import EFA2.Topology.Flow
--import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData

nodeColour :: Attribute 
nodeColour = FillColor (RGB 230 230 240)

clusterColour :: Attribute
clusterColour = FillColor (RGB 250 250 200)

originalEdgeColour :: Attribute
originalEdgeColour = Color [RGB 0 0 200]

intersectionEdgeColour :: Attribute
intersectionEdgeColour = Color [RGB 200 0 0]

mkDotGraph :: Gr NLabel ELabel -> (LNode NLabel -> String) -> (LEdge ELabel -> String) -> DotGraph Int
mkDotGraph g nshow eshow =
  DotGraph { strictGraph = False,
             directedGraph = True,
             graphID = Just (Int 1),
             graphStatements = stmts }
  where stmts = DotStmts { attrStmts = [],
                           subGraphs = [],
                           nodeStmts = map (mkDotNode nshow) (labNodes g),
                           edgeStmts = map (mkDotEdge eshow) (labEdges g) }


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
        colour | OriginalEdge <- etype = originalEdgeColour
               | IntersectionEdge <- etype = intersectionEdgeColour

printGraph :: Gr NLabel ELabel -> (LNode NLabel -> String) -> (LEdge ELabel -> String) -> IO ()
printGraph g nshow eshow = runGraphvizCanvas Dot (mkDotGraph g nshow eshow) Xlib

drawTopologyX' :: Topology -> IO ()
drawTopologyX' (Topology g) = printGraph g show show -- runGraphvizCanvas Dot (mkDotGraph g (show, show)) Xlib

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

data Line = PLine Int Int
          | XLine Int Int
          | NLine Int Int deriving (Eq, Ord)


instance Show Line where
         show (PLine u v) = "p_" ++ show u ++ "_" ++ show v
         show (XLine u v) = "x_" ++ show u ++ "_" ++ show v
         show (NLine u v) = "n_" ++ show u ++ "_" ++ show v




-- The argument t is for node labels. Until now, it is not used.
class DrawTopology a where
      drawTopology :: Topology -> Envs a ->  IO ()

instance DrawTopology [Double] where
         drawTopology = drawAbsTopology f
           where f (x, Just ys) = show x ++ " = " ++ (concatMap (printf "%.3f    ") ys)
                 f (x, Nothing) = show x ++ " = ♥"

instance (Integral a) => DrawTopology [Ratio a] where
         drawTopology = drawAbsTopology f
           where f (x, Just ys) = show x ++ " = " ++ (concatMap show ys)
                 f (x, Nothing) = show x ++ " = ♥"

instance DrawTopology [InTerm Val] where
         drawTopology = drawAbsTopology f
           where f (x, Just ys) = show x ++ " = " ++ (concatMap showInTerm ys)
                 f (x, Nothing) = show x ++ " = ♥"


drawAbsTopology :: (Show a) => ((Line, Maybe a) -> String) -> Topology -> Envs a ->  IO ()
drawAbsTopology f (Topology g) (Envs p dp e de x v) = printGraph g nshow eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst ps
        nshow (num, NLabel sec rec nid ty) = "NodeId: " ++ show nid ++ " (" ++ show num ++ ")\n" ++ 
                                             "Section: " ++ show sec ++ "\n" ++ 
                                             "Record: " ++ show rec ++ "\n" ++ 
                                             "Type: " ++ show ty

        node n = nodeNLabel (fromJust (lab g n))
        mkLst (u, v, _) = [ (PLine (node u) (node v), M.lookup (PowerIdx usec urec uid vid) p), 
                            (XLine (node u) (node v), M.lookup (XIdx usec urec uid vid) x),
                            (NLine (node u) (node v), M.lookup (EtaIdx usec urec uid vid) e),
                            (XLine (node v) (node u), M.lookup (XIdx vsec vrec vid uid) x),
                            (PLine (node v) (node u), M.lookup (PowerIdx vsec vrec vid uid) p) ]
                          where NLabel usec urec uid _ = fromJust $ lab g u
                                NLabel vsec vrec vid _ = fromJust $ lab g v
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
drawDiffTopology :: (Arith a, Show a) => ((Line, [a]) -> String) -> t -> TheGraph [a] -> DiffEnv [a] -> M.Map PowerIdx [a] ->  IO ()
drawDiffTopology f nenv (TheGraph g _) (DiffEnv dpenv deenv eenv xenv) penv = printGraph g show eshow
  where penv' = mkEnv $ mkPowerEnv penv
        eshow ps = L.intercalate "\n" $ map f $ mkLst ps
        mkLst (x, y) = [ (PLine, penv' (PowerIdx x y)), 
                         (XLine, xenv (XIdx x y)),
                         (NLine, eenv (EtaIdx x y)),
                         (XLine, xenv (XIdx y x)),
                         (PLine, penv' (PowerIdx y x)) ]

-}

{-
drawDependencyGraph :: Gr EqTerm () -> IO ()
drawDependencyGraph g = printGraph g nshow (const "")
  where m = M.fromList $ labNodes g
        nshow (x, _) = show x ++ ": " ++ showEqTerm (m M.! x)
       -- x = nmap nshow g
-}

{-
drawDependencyGraphTransClose :: TheGraph t -> [(PowerIdx, b)] -> IO ()
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