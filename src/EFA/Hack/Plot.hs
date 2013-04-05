{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances#-}

module EFA.Hack.Plot where

import qualified EFA.Signal.Plot.Options as PlOpts

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
-- import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Terminal as Terminal
-- import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.WXT as WXT

-- import qualified EFA.Signal.Plot as Plot
import EFA.Signal.Plot(getData)

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.Record (Record(Record))
import EFA.Signal.SequenceData (SequData(..))

import EFA.Report.Typ (TDisp)
import qualified EFA.Graph.Topology.Index as Idx
import EFA.Report.FormatValue (FormatValue, formatValue)
import qualified Graphics.Gnuplot.Advanced as AGP
import qualified EFA.Report.Format as Format
import qualified EFA.Equation.Variable as Var
import qualified EFA.Graph.Topology.Node as TN 
-- import EFA.Signal.Record (SigId(..))
import qualified EFA.Signal.Vector as SV

import Control.Functor.HT (void)
import qualified Data.Foldable as Fold
import qualified Data.Map as M
import Control.Monad (zipWithM_)
import qualified Data.List as L
import Data.Foldable (foldMap)

-- | Draw a histogram of a flow change stack
histogram ::
   (Fold.Foldable f, 
    FormatValue term, 
    TN.C node) =>
   Idx.InSection Idx.Energy node ->
   f (term, Double) -> Frame.T (Graph2D.T Int Double)
histogram key =
   Frame.cons (
      Opts.title "Decomposition of total output energy" $
      Histogram.rowstacked $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      Opts.xTicks2d [(Format.unASCII $ formatValue $
                      Idx.delta $ Var.index key, 0)] $
      Opts.xRange2d (-1,3) $
      Opts.deflt) .
   Fold.foldMap (\(term,val) ->
      fmap (Graph2D.lineSpec
              (LineSpec.title (Format.unASCII $ formatValue term) LineSpec.deflt)) $
      Plot2D.list Graph2D.histograms [val])


histogrammIO ::
   (Fold.Foldable f, TN.C node, FormatValue term) =>
   f (term, Double) -> Idx.InSection Idx.Energy node -> IO ()
histogrammIO  stack key = do
  void $ AGP.plotDefault $ histogram key stack




------------------------------------------------------------
--- Neuer Plot Ansatz fuer rPlot
            
-- data Idx rec sec part = Idx rec sec part                  
                  
newtype RecList s t1 t2 id v a = RecList [Record.Record s t1 t2 id v a]
newtype Sq s t1 t2 id v a = Sq  (SequData (Record.Record s t1 t2 id v a))
newtype SqList s t1 t2 id v a = SqList ([SequData (Record.Record s t1 t2 id v a)])
data RecSq s t1 t2 id v a =  RecSq  (Record.Record s t1 t2 id v a) (SequData (Record.Record s t1 t2 id v a))

            
class Time r id where
  record2 :: (Terminal.C term, 
              Fractional a,
              Ord id,
              Show id,
              SV.Walker v,
              SV.Storage v a,
              SV.FromList v,
              TDisp t1,
              TDisp t2,
              Atom.C a,
              Tuple.C a) => 
             (PlOpts.T id WXT.T -> PlOpts.T id term)
             -> r s t1 t2 id v a
             -> IO ()

-- | Plot a single record / eventually split plot in several windows 
instance Time Record.Record id where
   record2 optsIn rec = (f opts) 
     where  
       opts = PlOpts.build optsIn
       f a | PlOpts.splitAcc a == PlOpts.NoSplit = recordIO2 opts "" [rec]
       f a | otherwise = zipWithM_  (recordIO2 opts) wtitleList (L.transpose $ map (Record.split x) [rec])               
         where (PlOpts.Split x) = PlOpts.splitAcc a
               wtitleList = map (\y -> "Part" ++ show y) [(0 ::Int) ..]

-- | Plot a list of records against each other / eventually split in several windows
instance Time RecList id where
  record2 optsIn (RecList recList) = (f opts) 
    where
      opts = PlOpts.build optsIn
      f a | PlOpts.splitAcc a == PlOpts.NoSplit = recordIO2 opts "" recList 
      
      f a | otherwise = zipWithM_ (recordIO2 opts) wtitleList (L.transpose $ map (Record.split x) recList)               
        where (PlOpts.Split x) = PlOpts.splitAcc a
              wtitleList = map (\y -> "Part" ++ show y) [(0::Int) ..]


-- | Plot a SequenceRecord, each Section in a new Window
instance Time Sq id where
           record2 optsIn (Sq sqRecList) = (f opts)
             where
               opts = PlOpts.build optsIn
               f a | PlOpts.splitAcc a == PlOpts.NoSplit = Fold.sequence_ $ SD.mapWithSection (recordIO2Sec opts) (fmap (\x -> [x]) sqRecList)
               f _ | otherwise = error "Splitting not implemented for Sequence Records"


{-
-- | Plot a List of SequenceRecords against each other
instance Time SqList id where
           record2 optsIn (SqList sList) = (f opts) 
             where
               opts = PlOpts.build optsIn
               f a | PlOpts.splitAcc a == PlOpts.NoSplit = zipWithM_ (recordIO2 opts) wtitles (L.transpose $ map (\ (SequData x) -> x) sList) 
                 where wtitles = map (\x -> "Sec"++ show x) [(0 ::Int) ..] 
               f _ | otherwise = error "Splitting not implemented for Sequence Records"        


-- | Plot Sequence Signals on top of Record (e.g. Test Signals after cutting)
instance Time RecSq id where
           record2 optsIn (RecSq rec (SequData recList) ) = (f opts) 
             where
               opts = PlOpts.build optsIn
               f a | PlOpts.splitAcc a == PlOpts.NoSplit = recordIO2 opts "" (rec:recList) 
               f a | otherwise = zipWithM_ (recordIO2 opts) wtitles (L.transpose $ map (Record.split x) (rec:recList))                  
                 where (PlOpts.Split x) = PlOpts.splitAcc a
                       wtitles = map (\y -> "Sec"++ show y) [(0::Int) ..]
-}


------------------------------------------------------------
-- | Generate Frame and Terminal and Plot a Single Window
recordIO2 :: (Terminal.C term, 
               Ord id, 
               Show id,
               Fractional a, 
               SV.Walker v, 
               SV.Storage v a,
               SV.FromList v, 
               TDisp typ1, 
               TDisp typ0, 
               Tuple.C a, 
               Atom.C a) =>
               (PlOpts.T id term)
              -> String 
              -> [Record.Record s typ0 typ1 id v a] 
              -> IO ()
recordIO2 opts wtitle xs = void $ AGP.plot term $ frame $ Fold.fold $ zipWith (plotSingleRecord opts) recIdxList (map treatRecord xs)
  where    
    recIdxList = map Record.Idx [0 .. ]
    frame = PlOpts.buildFrame wtitle opts
    treatRecord = PlOpts.buildPrepFunction opts
    term = PlOpts.buildTerminal wtitle opts

recordIO2Sec :: (Terminal.C term,
               Ord id,
               Show id,
               Fractional a,
               SV.Walker v,
               SV.Storage v a,
               SV.FromList v,
               TDisp typ1,
               TDisp typ0,
               Tuple.C a,
               Atom.C a) =>
               (PlOpts.T id term)
              -> Idx.Section
              -> [Record.Record s typ0 typ1 id v a]
              -> IO ()
recordIO2Sec opts (Idx.Section sec) =
   recordIO2 opts ("Sec" ++ show sec)


-- | Generate gnuplot-Data from a single record
plotSingleRecord ::
   (Show id, TDisp typ0, TDisp typ1,
    SV.Walker v, SV.FromList v,
    SV.Storage v a, Fractional a, Atom.C a, Tuple.C a) =>
   PlOpts.T id term -> Record.Idx -> Record s typ0 typ1 id v a -> Plot2D.T a a
plotSingleRecord opts recIdx (Record time pMap) =
   foldMap
      (\(key, sig) ->
         PlOpts.buildStyle recIdx opts key $
         Plot2D.list Graph2D.linesPoints $
         zip (getData time) (getData sig)) $
   M.toList pMap


