{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}

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
import EFA2.Example.SymSig
import EFA2.Signal.Arith
--import EFA2.Example.Loop
import EFA2.Example.Linear

import EFA2.Utils.Utils

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


drawTopologyX :: Gr a b -> IO ()
drawTopologyX g = runGraphvizCanvas Dot (mkDotGraph g (show, show)) Xlib

class DrawTopology a where
      drawTopology :: t -> LRPowerEnv [a] -> LREtaEnv [a] -> LRXEnv [a] -> Gr b c -> IO ()

instance DrawTopology Val where
         drawTopology = drawTopologyA

drawTopologyA :: (PrintfArg a) => t -> LRPowerEnv [a] -> LREtaEnv [a] -> LRXEnv [a] -> Gr b c -> IO ()
drawTopologyA nenv penv eenv xenv g = runGraphvizCanvas Dot (mkDotGraph g (show, eshow)) Xlib
  where eshow1 (x, y) = [("p: ", penv (PowerIdx x y)), ("x: ", xenv (XIdx x y)), ("n: ", eenv (EtaIdx x y))]
        eshow2 (x, y) = [("x: ", xenv (XIdx y x)), ("p: ", penv (PowerIdx y x))]
        eshow ps = L.intercalate "\n" $ map (f . fmap fromRight) $ (eshow1 ps ++ eshow2 ps)
        fromRight (Right x) = x
        fromRight (Left x) = error (show x)
        f (x, ys) = x ++ (concatMap (printf "%.4f    ") ys)

instance DrawTopology InTerm where
         drawTopology = drawTopologyT

drawTopologyT :: t -> LRPowerEnv [InTerm] -> LREtaEnv [InTerm] -> LRXEnv [InTerm] -> Gr a b -> IO ()
drawTopologyT nenv penv eenv xenv g = runGraphvizCanvas Dot (mkDotGraph g (show, eshow)) Xlib
  where eshow1 (x, y) = [("p", penv (PowerIdx x y)), ("x", xenv (XIdx x y)), ("n", eenv (EtaIdx x y))]
        eshow2 (x, y) = [("x", xenv (XIdx y x)), ("p", penv (PowerIdx y x))]
        eshow ps = L.intercalate "\n" $ map (f . fmap fromRight) $ (eshow1 ps ++ eshow2 ps)
        fromRight (Right x) = x
        fromRight (Left x) = error (show x)
        f (x, e@(eq:_)) = x ++ " = " ++ (showInTerm eq)

--drawDependencyGraph :: Gr NLabel ELabel -> [EqTerm NLabel] -> IO ()
drawDependencyGraph g given = runGraphvizCanvas Dot (mkDotGraph g' (nshow, (const ""))) Xlib
  where -- g' :: Gr x y
        g' = makeDependencyGraph g given
        m = M.fromList $ ufold f [] g'
        f (_, n, l, _) acc = (n, l):acc
        nshow x = show x ++ ": " ++ showEqTerm (fromJust (M.lookup x m))
