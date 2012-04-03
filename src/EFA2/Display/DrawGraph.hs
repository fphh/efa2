{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances, MultiParamTypeClasses #-}

module EFA2.Display.DrawGraph where

import qualified Data.Map as M
import qualified Data.List as L

import Data.Maybe
import Data.Graph.Inductive
import qualified Data.Text.Lazy as T
import Data.GraphViz
import Data.GraphViz.Attributes.Complete

import Text.Printf

import Debug.Trace

import EFA2.Graph.GraphData
import EFA2.Graph.Graph
import EFA2.Graph.DependencyGraph

import EFA2.Term.Equation
import EFA2.Term.EqInterpreter
import EFA2.Term.TermData
import EFA2.Term.Solver

import EFA2.Example.SymSig
import EFA2.Signal.Arith


nodeColour :: Attribute 
nodeColour = FillColor (RGB 230 230 240)

clusterColour :: Attribute
clusterColour = FillColor (RGB 250 250 200)

mkDotGraph :: Gr a b -> (Node -> String, Edge -> String) -> DotGraph Int
mkDotGraph g (nodef, edgef) = 
  DotGraph { strictGraph = False,
             directedGraph = True,
             graphID = Just (Int 1),
             graphStatements = stmts }
  where stmts = DotStmts { attrStmts = [],
                           subGraphs = [],
                           nodeStmts = map (\n -> mkDotNode n $ nodef n) (nodes g),
                           edgeStmts = map (\n -> mkDotEdge n $ edgef n) (edges g) }


mkDotNode:: Node -> String -> DotNode Int
mkDotNode x str = DotNode x [displabel, nodeColour, Style [SItem Filled []], Shape BoxShape ] 
  where displabel =  Label $ StrLabel $ T.pack str

mkDotEdge :: Edge -> String -> DotEdge Int
mkDotEdge (x, y) str = DotEdge x y [displabel]
  where displabel = Label $ StrLabel $ T.pack str

printGraph :: Gr a b -> (Node -> String) -> (Edge -> String) -> IO ()
printGraph g nshow eshow = runGraphvizCanvas Dot (mkDotGraph g (nshow, eshow)) Xlib

drawTopologyX :: TheGraph a -> IO ()
drawTopologyX (TheGraph g _) = runGraphvizCanvas Dot (mkDotGraph g (show, show)) Xlib


data Line = PLine | XLine | NLine deriving (Eq, Ord)

instance Show Line where
         show PLine = "p"
         show XLine = "x"
         show NLine = "n"

-- The argument t is for node labels. Until now, it is not used.
class DrawTopology env a where
      drawTopology :: t -> TheGraph [a] -> env [a] -> M.Map PowerIdx [a] ->  IO ()

instance DrawTopology AbsEnv Val where
         drawTopology = drawAbsTopology f
           where f (x, ys) = show x ++ " = " ++ (concatMap (printf "%.3f    ") ys)

instance DrawTopology AbsEnv InTerm where
         drawTopology = drawAbsTopology f
           where f (PLine, es) = "p = " ++ (L.intercalate " | " $ map showInTerm es)
                 f (XLine, e:_) = "x = " ++ showInTerm e
                 f (NLine, e:_) = "n = " ++ showInTerm e
 
drawAbsTopology :: (Arith a) => ((Line, [a]) -> String) -> t -> TheGraph [a] -> AbsEnv [a] -> M.Map PowerIdx [a] ->  IO ()
drawAbsTopology f nenv (TheGraph g _) (AbsEnv eenv xenv) penv = printGraph g show eshow
  where penv' = mkEnv $ mkPowerEnv penv
        eshow ps = L.intercalate "\n" $ map f $ mkLst ps
        mkLst (x, y) = [ (PLine, penv' (PowerIdx x y)), 
                         (XLine, xenv (XIdx x y)),
                         (NLine, eenv (EtaIdx x y)),
                         (XLine, xenv (XIdx y x)),
                         (PLine, penv' (PowerIdx y x)) ]

instance DrawTopology DiffEnv InTerm where
         drawTopology = drawDiffTopology f
           where f (PLine, es) = "p = " ++ (L.intercalate " | " $ map showInTerm es)
                 f (XLine, e:_) = "x = " ++ showInTerm e
                 f (NLine, e:_) = "n = " ++ showInTerm e
 

drawDiffTopology :: (Arith a) => ((Line, [a]) -> String) -> t -> TheGraph [a] -> DiffEnv [a] -> M.Map PowerIdx [a] ->  IO ()
drawDiffTopology f nenv (TheGraph g _) (DiffEnv dpenv deenv eenv xenv) penv = printGraph g show eshow
  where penv' = mkEnv $ mkPowerEnv penv
        eshow ps = L.intercalate "\n" $ map f $ mkLst ps
        mkLst (x, y) = [ (PLine, penv' (PowerIdx x y)), 
                         (XLine, xenv (XIdx x y)),
                         (NLine, eenv (EtaIdx x y)),
                         (XLine, xenv (XIdx y x)),
                         (PLine, penv' (PowerIdx y x)) ]


drawDependencyGraph :: TheGraph t -> [(PowerIdx, b)] -> IO ()
drawDependencyGraph (TheGraph g _) given = printGraph g' nshow (const "")
  where gvs = give $ map (Energy . fst) given
        g' = makeDependencyGraph g gvs
        m = M.fromList $ labNodes g'
        nshow x = show x ++ ": " ++ showEqTerm (m M.! x)
