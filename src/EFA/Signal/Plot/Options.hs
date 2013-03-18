{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}


module EFA.Signal.Plot.Options where

import qualified Graphics.Gnuplot.Terminal as Terminal
-- import qualified Graphics.Gnuplot.Advanced as AGP
-- import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Frame as Frame
-- import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
-- import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.WXT as WXT

import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.PostScript as PS


import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
-- import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import EFA.Report.Typ (DisplayType(Typ_T), getDisplayUnit)
-- import EFA.Signal.Record (SigId(..))
import qualified EFA.Signal.Record as Record
-- import qualified Data.Map as Map 


data Split = Split Int | NoSplit deriving Eq


data  T id term = T {
  gridAcc :: Bool, 
  terminalAcc :: Term term,
  showIdAcc :: id -> String,
  extractAcc :: [id],
  normAcc :: Bool, 
  splitAcc :: Split,
  titleAcc :: String, 
  wtitleAcc:: String,
  showRecIdxAcc:: Record.Idx -> String,
  pointSizeAcc :: LineSpec.T -> LineSpec.T,
  pointTypeAcc :: LineSpec.T -> LineSpec.T, 
  lineWidthAcc :: LineSpec.T -> LineSpec.T, 
  lineStyleAcc :: LineSpec.T -> LineSpec.T,            
  lineTypeAcc  :: LineSpec.T -> LineSpec.T} 

grid :: Bool -> T id term -> T id term 
grid b opts = opts { gridAcc = b } 

title :: String -> T id term -> T id term 
title ti opts = opts { titleAcc = ti }
 
terminal :: Term term -> T id WXT.T -> T id term 
terminal term opts = opts { terminalAcc = term} 

wtitle :: String -> T id term -> T id term 
wtitle tis opts = opts { wtitleAcc = tis }

rectitle :: (Record.Idx -> String) -> T id term -> T id term 
rectitle f opts = opts { showRecIdxAcc = f }

showId :: (id -> String) -> T id term ->  T id term
showId f opts = opts {showIdAcc = f}

section :: [id] -> T id term ->  T id term 
section sigIds opts = opts {extractAcc = sigIds}

norm :: Bool ->  T id term ->  T id term 
norm b opts = opts {normAcc = b}

split :: Split ->  T id term ->  T id term 
split n opts = opts {splitAcc = n}

pointSize :: Double ->  T id term -> T id term  
pointSize x opts = opts { pointSizeAcc =  LineSpec.pointSize x }

pointType :: Int ->  T id term -> T id term  
pointType x opts = opts { pointTypeAcc = LineSpec.pointType x } 
  
lineWidth :: Double ->  T id term -> T id term                    
lineWidth x opts = opts { lineWidthAcc = LineSpec.lineWidth x }
  
lineStyle :: Int ->  T id term -> T id term  
lineStyle x opts = opts { lineStyleAcc = LineSpec.lineStyle x }

lineType :: Int ->  T id term -> T id term  
lineType x opts = opts { lineTypeAcc = LineSpec.lineType x }

extract :: [id] ->  T id term -> T id term
extract x opts = opts { extractAcc = x }

-- | Set Default Values for Global Options
deflt :: Show id => T id WXT.T
deflt = T {
  gridAcc = True,
  titleAcc = "", 
  extractAcc = [],
  normAcc = False,
  terminalAcc = WXTTerm,
  showIdAcc = show,
  splitAcc = NoSplit,              
  wtitleAcc = "",           
  showRecIdxAcc = show,
  pointSizeAcc = LineSpec.pointSize 0.3,
  pointTypeAcc = LineSpec.pointType 1, 
  lineWidthAcc = LineSpec.lineWidth 1, 
  lineStyleAcc = LineSpec.lineStyle 1,            
  lineTypeAcc =  LineSpec.lineType 1            
  }

        
build :: (Show id) => (T id WXT.T -> T id term) -> T id term
build opts = opts deflt


{- @HT  Not working -- help appreciated      
buildFrame:: Graph.C graph =>
                             [Char]
                             -> T id term
                             -> Plot.T graph
                             -> Frame.T graph
-}

buildFrame wti opts  = 
  Frame.cons $
  Opts.title (titleAcc opts ++ "_" ++ wti) $
  Opts.grid (gridAcc opts) $     
  Opts.xLabel ("Time [" ++ (show $ getDisplayUnit Typ_T) ++ "]") $
  Opts.yLabel ("")
  Opts.deflt

data  Term a where
  X11Term :: Term X11.T
  WXTTerm :: Term WXT.T
  PSTerm  :: String -> Term PS.T
  PNGTerm  :: String -> Term PNG.T
  SVGTerm  :: String -> Term SVG.T

buildTerminal :: Terminal.C term =>  String -> T id term -> term    
buildTerminal wti opts = f (terminalAcc opts)
  where
    f :: Term term -> term
    f X11Term  = X11.title name $ X11.cons 
    f WXTTerm  = WXT.title name $ WXT.cons   
    f (PSTerm filePath) = PS.cons (filePath ++ "/" ++ name ++ ".ps")
    f (PNGTerm filePath) = PNG.cons (filePath ++ "/" ++ name ++ ".png")
    f (SVGTerm filePath) = SVG.cons (filePath ++ "/" ++ name ++ ".svg")
    name = titleAcc opts ++ "_" ++ wti


-- | Build the function to condition the record before plotting 
buildPrepFunction :: (Ord id, Show id) => T id term ->  
                     (Record.Record s t1 t2 id v a ->  
                      Record.Record s t1 t2 id v a)      
buildPrepFunction  opts = g opts 
  where
    -- | do nothing with emtpy list
    g o | extractAcc o == [] = id
    g o | otherwise = Record.extract (extractAcc o)
    
-- | Build the line style for plotting a record
buildStyle :: (Show id) => Record.Idx -> T id term -> id -> Plot2D.T x y -> Plot2D.T x y
buildStyle recIdx opts key =
   fmap $ Graph2D.lineSpec $
      pointSizeAcc opts $
      pointTypeAcc opts $ 
      lineWidthAcc opts $
      LineSpec.title (recName recIdx ++ showIdf key) $ -- show record name in legend
      LineSpec.deflt
      where
        showIdf = showIdAcc opts  
        recName x@(Record.Idx _) = (showRecIdxAcc opts) x ++ "_"
        recName (Record.NoIdx)  = ""