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
import EFA2.Graph.GraphData


nodeColour = FillColor (RGB 230 230 240)
clusFillColour = FillColor (RGB 250 250 200)

-- Deaw Topology Graph on X_Canvas
drawTopologyX :: Gr a b -> IO ()
drawTopologyX g = runGraphvizCanvas Dot (mkDotGraph g (dispNode, dispEdge)) Xlib 

-- Draw Topology with on X_Canvas Flow Values
drawFlowX :: (Gr String String, FlowData Flow) -> IO ()
drawFlowX (g,flowData) = runGraphvizCanvas Dot (mkDotGraph g (dispNode, dispEdgeVals (eta flowData))) Xlib 

-- function to Generate Dot Graph
-- mkDotGraph :: Gr a b -> 
mkDotGraph g (dispFunctNode, dispFunctEdge) = DotGraph { strictGraph = False
                                                       , directedGraph = True
                                                       , graphID = Just (Int 1) 
                                                       , graphStatements = DotStmts { attrStmts = [] 
                                                                                    , subGraphs=[]
                                                                                    , nodeStmts = map (mkDotNode dispFunctNode) (nodes g)
                                                                                    , edgeStmts = map (mkDotEdge dispFunctEdge) (edges g)}}
 

-- Node display function without values 
dispNode :: NodeIndex -> String
dispNode nodeIdx = disp nodeIdx

-- Edge display function without Values
dispEdge :: EdgeIndex -> String
dispEdge edgeIdx = disp edgeIdx

-- Edge display function with Values
dispEdgeVals :: (Display a) => EdgeData a -> EdgeIndex -> String 
dispEdgeVals vals edgeIdx = disp edgeIdx ++ ": " ++ disp (vals ~! edgeIdx)

mkDotNode:: (NodeIndex -> String) -> Node  -> DotNode Int
mkDotNode  dispFunct  x = DotNode x [displabel,nodeColour,Style [SItem Filled []]] 
  where displabel =  Label $ StrLabel $ T.pack $ dispFunct (NodeIndex x) 

mkDotEdge :: (EdgeIndex -> String) -> Edge -> DotEdge Int
mkDotEdge dispFunct (x, y) = DotEdge x y [displabel]
  where displabel = Label $ StrLabel $ T.pack $ dispFunct (mkIdx x y) 
        
        


        





