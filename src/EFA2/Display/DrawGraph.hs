{-# LANGUAGE ImpredicativeTypes, FlexibleContexts #-}
module EFA2.Display.DrawGraph (module EFA2.Display.DrawGraph) where

import qualified Data.Map as M

import Data.GraphViz
import Data.Graph.Inductive
import Data.GraphViz.Printing
import Data.GraphViz.Types.Canonical
import Data.GraphViz.Attributes.Complete
import qualified Data.Text.Lazy as T
import qualified Data.Vector.Unboxed as UV


import EFA2.Signal.SignalData
import EFA2.Graph.Graph
import EFA2.Term.Term
import EFA2.Term.EquationOrder


nodeColour = FillColor (RGB 230 230 240)
clusFillColour = FillColor (RGB 250 250 200)

drawTopologyX :: (Show b, Show a) => Gr a b -> IO ()
drawTopologyX g = runGraphvizCanvas Dot (mkDotGraph g (dispNode,dispEdge)) Xlib 

drawFlowX :: (Show b, Show a) => (Gr a b,SampleEnv PSample) -> IO ()
drawFlowX (g,sigs) = runGraphvizCanvas Dot (mkDotGraph g (dispNode,dispEdgeVals sigs)) Xlib 

mkDotGraph g (dispFunctNode, dispFunctEdge) = DotGraph { strictGraph = False
                                                       , directedGraph = True
                                                       , graphID = Just (Int 1) 
                                                       , graphStatements = DotStmts { attrStmts = [] 
                                                                                    , subGraphs=[]
                                                                                    , nodeStmts = map (mkDotNode dispFunctNode) (labNodes g)
                                                                                    , edgeStmts = map (mkDotEdge dispFunctEdge) (labEdges g)}}



dispNode id1 typ = "N"++show id1

dispEdge id1 id2 typ = "T_"++ show id1 ++ "_" ++ show id2

dispEdgeVals vals id1 id2 typ  = "T_"++ show id1 ++ "_" ++ show id2 ++ ": " ++ show (vals M.! (mkIdx id1 id2)) ++ show (vals M.! (mkIdx id2 id1))

-- mkDotNode:: Show a => LNode a  -> DotNode a  
mkDotNode  dispFunct  (x,label)= DotNode x [label,nodeColour,Style [SItem Filled []]] 
  where label =  Label $ StrLabel $ T.pack $ dispFunct x label

-- mkDotEdge :: (Show a) => LEdge a -> DotEdge a
mkDotEdge dispFunct (x, y,label) = DotEdge x y [label]
  where label = Label $ StrLabel $ T.pack $ dispFunct x y label
        
        


        





