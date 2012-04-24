{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances, MultiParamTypeClasses #-}

module EFA2.Display.DrawGraph where

import qualified Data.Map as M
import qualified Data.List as L

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
import EFA2.Topology.Topology

import EFA2.Topology.Flow
import EFA2.Signal.Sequence

nodeColour :: Attribute 
nodeColour = FillColor (RGB 230 230 240)

clusterColour :: Attribute
clusterColour = FillColor (RGB 250 250 200)

mkDotGraph :: Gr a b -> (LNode a -> String, LEdge b -> String) -> DotGraph Int
mkDotGraph g (nodef, edgef) = 
  DotGraph { strictGraph = False,
             directedGraph = True,
             graphID = Just (Int 1),
             graphStatements = stmts }
  where stmts = DotStmts { attrStmts = [],
                           subGraphs = [],
                           nodeStmts = map (\n -> mkDotNode (fst n) $ nodef n) (labNodes g),
                           edgeStmts = map (\n@(a, b, _) -> mkDotEdge (a, b) $ edgef n) (labEdges g) }


mkDotNode:: Node -> String -> DotNode Int
mkDotNode x str = DotNode x [displabel, nodeColour, Style [SItem Filled []], Shape BoxShape ] 
  where displabel =  Label $ StrLabel $ T.pack str

mkDotEdge :: Edge -> String -> DotEdge Int
mkDotEdge (x, y) str = DotEdge x y [displabel]
  where displabel = Label $ StrLabel $ T.pack str

printGraph :: Gr a b -> (LNode a -> String) -> (LEdge b -> String) -> IO ()
printGraph g nshow eshow = runGraphvizCanvas Dot (mkDotGraph g (nshow, eshow)) Xlib

drawTopologyX' :: (Show a, Show b) => Gr a b -> IO ()
drawTopologyX' g = printGraph g show show -- runGraphvizCanvas Dot (mkDotGraph g (show, show)) Xlib

drawFlowTop :: FlowTopology -> IO ()
drawFlowTop (FlowTopology g) = printGraph g show show -- runGraphvizCanvas Dot (mkDotGraph g (show, show)) Xlib

drawSequFlowTops :: SequFlowTops -> IO ()
drawSequFlowTops (SequData flowTops) = mapM_ drawFlowTop flowTops



{-
drawTopologyX :: TheGraph a -> IO ()
drawTopologyX (TheGraph g _) = printGraph g show show
-}

data Line = PLine Int Int
          | XLine Int Int
          | NLine Int Int deriving (Eq, Ord)

instance Show Line where
         show (PLine u v) = "p." ++ show u ++ "." ++ show v
         show (XLine u v) = "x" ++ show u ++ "." ++ show v
         show (NLine u v) = "n" ++ show u ++ "." ++ show v




-- The argument t is for node labels. Until now, it is not used.
class DrawTopology a where
      drawTopology :: Gr NLabel c -> Envs a ->  IO ()

instance DrawTopology [Val] where
         drawTopology = drawAbsTopology f
           where f (x, Just ys) = show x ++ " = " ++ (concatMap (printf "%.3f    ") ys)
                 f (x, Nothing) = show x ++ " = ♥"

instance DrawTopology [InTerm Val] where
         drawTopology = drawAbsTopology f
           where f (x, Just ys) = show x ++ " = " ++ (concatMap showInTerm ys)
                 f (x, Nothing) = show x ++ " = ♥"


drawAbsTopology :: (Arith a, Show a) => ((Line, Maybe a) -> String) -> Gr NLabel c -> Envs a ->  IO ()
drawAbsTopology f g (Envs p dp e de x v) = printGraph g nshow eshow
  where eshow ps = L.intercalate "\n" $ map f $ mkLst ps
        nshow (nid, NLabel sec rec from to) = show nid ++ ": " ++ show sec ++ "_" ++ show rec ++ "_" ++ show from ++ "_" ++ show to
        mkLst (u, v, _) = [ (PLine u v, M.lookup (PowerIdx usec urec uid vid) p), 
                            (XLine u v, M.lookup (XIdx usec urec uid vid) x),
                            (NLine u v, M.lookup (EtaIdx usec urec uid vid) e),
                            (XLine v u, M.lookup (XIdx vsec vrec vid uid) x),
                            (PLine v u, M.lookup (PowerIdx vsec vrec vid uid) p) ]
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

drawDependencyGraph :: Gr EqTerm () -> IO ()
drawDependencyGraph g = printGraph g nshow (const "")
  where m = M.fromList $ labNodes g
        nshow (x, _) = show x ++ ": " ++ showEqTerm (m M.! x)
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