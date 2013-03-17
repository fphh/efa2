{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}


module EFA.Signal.Plot.Options where

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Advanced as AGP
import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.WXT as WXT
import qualified Graphics.Gnuplot.Terminal.PostScript as PS


import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import EFA.Report.Typ (TDisp, DisplayType(Typ_T), getDisplayUnit, getDisplayTypName)
import EFA.Signal.Record (SigId(..))
import qualified EFA.Signal.Record as Record
import qualified Data.Map as Map 


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
  pointSizeAcc :: LineSpec.T -> LineSpec.T,
  pointTypeAcc :: LineSpec.T -> LineSpec.T, 
  lineWidthAcc :: LineSpec.T -> LineSpec.T, 
  lineStyleAcc :: LineSpec.T -> LineSpec.T,            
  lineTypeAcc  :: LineSpec.T -> LineSpec.T, 
  lineTitleAcc  :: LineSpec.T -> LineSpec.T} 

grid :: Bool -> T id term -> T id term 
grid b gopts = gopts { gridAcc = b } 

title :: String -> T id term -> T id term 
title ti gopts = gopts { titleAcc = ti }
 
terminal :: Term term -> T id term -> T id term 
terminal term gopts = gopts { terminalAcc = term} 

showId :: (id -> String) -> T id term ->  T id term
showId f opts = opts {showIdAcc = f}

section :: [id] -> T id term ->  T id term 
section sigIds opts = opts {extractAcc = sigIds}

norm :: Bool ->  T id term ->  T id term 
norm b opts = opts {normAcc = b}

split :: Split ->  T id term ->  T id term 
split n opts = opts {splitAcc = n}

wtitle :: String -> T id term -> T id term 
wtitle tis gopts = gopts { wtitleAcc = tis }

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
 
sigTitle :: String ->  T id term -> T id term 
sigTitle x opts =  opts { lineTitleAcc = LineSpec.title x }

-- | Set Default Values for Global Options
deflt :: Show id => T id X11.T
deflt = T {
  gridAcc = False,
  titleAcc = "", 
  extractAcc = [],
  normAcc = False,
  terminalAcc = X11Term,
  showIdAcc = show,
  splitAcc = NoSplit,              
  wtitleAcc = "",           
  pointSizeAcc = LineSpec.pointSize 0.3,
  pointTypeAcc = LineSpec.pointType 1, 
  lineWidthAcc = LineSpec.lineWidth 1, 
  lineStyleAcc = LineSpec.lineStyle 1,            
  lineTypeAcc =  LineSpec.lineType 1,            
  lineTitleAcc = LineSpec.title ""               
  }

        
build :: (Show id) => (T id X11.T -> T id term) -> T id term
build opts = opts deflt


-- | Line Style
buildStyle :: (Show id) => T id term -> id -> Plot2D.T x y -> Plot2D.T x y
buildStyle opts key =
   fmap $ Graph2D.lineSpec $
      pointSizeAcc opts $
      pointTypeAcc opts $ 
      lineWidthAcc opts $
      lineStyleAcc opts $
      lineTitleAcc opts $
      LineSpec.deflt
      where
        showfunct = showIdAcc opts  
      
      

buildFrame opts  = 
  Frame.cons $
  Opts.title (titleAcc opts ++ "_" ++ wtitleAcc opts) $
  Opts.grid (gridAcc opts) $     
  Opts.xLabel ("Time [" ++ (show $ getDisplayUnit Typ_T) ++ "]") $
  Opts.yLabel ("")
  Opts.deflt


data  Term a where
  X11Term :: Term X11.T
  WXTTerm :: Term WXT.T
--  PSTerm  :: Term PS.T 


getTerminal :: Terminal.C term =>  T id term -> term    
getTerminal opts = f (terminalAcc opts)
  where
    f :: Term term -> term
    f X11Term  = X11.cons 
    f WXTTerm  = WXT.cons   
--    f (PSTerm filePath) = PS.cons filePath


buildPrepFunction :: (Ord id, Show id) => T id term ->  
                     (Record.Record s t1 t2 id v a ->  
                      Record.Record s t1 t2 id v a)      
buildPrepFunction  opts = g opts 
  where
        
    -- | do nothing with emtpy list
    g o | extractAcc o == [] = id
    g o | otherwise = Record.extract (extractAcc o)
    
