{-# LANGUAGE TypeFamilies, ScopedTypeVariables, RankNTypes #-}


module Main where

import EFA.Example.Utility (makeEdges)
import EFA.Utility.Async (concurrentlyMany_)

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr
import qualified EFA.Example.Absolute as EqGen

import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Example.Index as XIdx
import qualified EFA.Graph.Topology.Index as TIdx

import qualified EFA.Equation.Record as EqRec
import qualified EFA.Equation.Arithmetic as EqArith
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Signal.PlotIO as PlotIO

import EFA.Example.Absolute ( (.=), (%=), (=.=) )
import EFA.Equation.Result (Result(..))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Map (checkedLookup)
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.ConvertTable as Table

import Control.Applicative (liftA)


import Data.Monoid (mconcat, (<>))


sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0



source, crossing, sink, storage :: Node.Int
source :~ crossing :~ sink :~ storage :~ _ = Stream.enumFrom minBound

topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.fromList ns (makeEdges es)
  where ns = [ (source, TD.Source),
               (crossing, TD.Crossing),
               (sink, TD.Sink),
               (storage, TD.Storage ()) ]
        es = [ (source, crossing),
               (crossing, sink), 
               (crossing, storage) ]



select :: [topo] -> [Int] -> SD.SequData topo
select ts = SD.fromList . map (ts !!)


seqTopo = Flow.mkSequenceTopology (select sol states)
  where states = [0, 3]
        sol = StateAnalysis.advanced topoDreibein


lookupStCrDown ::
  EqGen.Expression node s a v Double -> 
  EqGen.Expression node s a v Double
lookupStCrDown = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

lookupStCrUp ::
  EqGen.Expression node s a v Double -> 
  EqGen.Expression node s a v Double
lookupStCrUp = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

lookupCrSi ::
  EqGen.Expression node s a v Double -> 
  EqGen.Expression node s a v Double
lookupCrSi = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.60, 0.61 .. 0.9]


lookupSoCr ::
  EqGen.Expression node s a v Double -> 
  EqGen.Expression node s a v Double
lookupSoCr = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.70, 0.71 .. 1]

commonEnv :: EqGen.EquationSystem Node.Int s Double Double
commonEnv =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (XIdx.dTime sec1 .= 1) :
   (XIdx.storage TIdx.Initial storage .= 0) :
   (XIdx.power sec0 storage crossing %= XIdx.power sec1 storage crossing) :
   []

givenSec0Mean :: Double -> Double -> EqGen.EquationSystem Node.Int s Double Double
givenSec0Mean psink psinkConst =
   (commonEnv <>) $
   mconcat $

   (XIdx.power sec0 sink crossing .= psinkConst) :
   (XIdx.eta sec0 crossing sink .= 0.8) :
   
   (XIdx.eta sec0 source crossing .= 0.9) :

   (XIdx.eta sec0 crossing storage .= 0.85) :

   ((EqGen.variable $ XIdx.eta sec1 storage crossing) =.=
     lookupStCrUp (EqGen.variable $ XIdx.power sec1 crossing storage)) :

   ((EqGen.variable $ XIdx.eta sec1 crossing sink) =.=
     lookupCrSi (EqGen.variable $ XIdx.power sec1 sink crossing)) :

   (XIdx.power sec1 sink crossing .= psink) :
   []

givenSec1Mean :: Double -> Double -> EqGen.EquationSystem Node.Int s Double Double
givenSec1Mean psink psinkConst =
   (commonEnv <>) $
   mconcat $


   (XIdx.power sec1 sink crossing .= psinkConst) :

   (XIdx.eta sec1 crossing sink .= 0.8) :

   (XIdx.eta sec1 storage crossing .= 0.85) :


   ((EqGen.variable $ XIdx.eta sec0 crossing storage) =.=
     lookupStCrDown (EqGen.variable $ XIdx.power sec0 storage crossing)) :

   ((EqGen.variable $ XIdx.eta sec0 crossing sink) =.=
     lookupCrSi (EqGen.variable $ XIdx.power sec0 sink crossing)) :

   ((EqGen.variable $ XIdx.eta sec0 source crossing) =.=
     lookupSoCr (EqGen.variable $ XIdx.power sec0 crossing source)) :

   (XIdx.power sec0 sink crossing .= psink) :

   []



etaSys ::
  EqEnv.Complete
    Node.Int
    (EqRec.Absolute (Result Double))
    (EqRec.Absolute (Result Double)) ->
  Double
etaSys env =
  (lookup eSinkSec0 + lookup eSinkSec1) / (lookup eSource) 
  where 
        lookup n | 
          EqRec.Absolute (Determined x) <-
            checkedLookup (EqEnv.energyMap $ EqEnv.signal env) n = x
        eSource = XIdx.energy sec0 source crossing
        eSinkSec0 = XIdx.energy sec0 sink crossing
        eSinkSec1 = XIdx.energy sec1 sink crossing


sinkRange :: [Double]
sinkRange = [0.1, 0.2 .. 12]

sinkRangeMean :: [Double]
sinkRangeMean = [0.1, 0.2 .. 5]

varX', varY' :: [[Double]]
(varX', varY') = Table.varMat sinkRange sinkRangeMean 



main :: IO ()
main = do

  let eqs = map givenSec1Mean sinkRange

{-
      f0 x = etaSys $ EqGen.solve seqTopo $ givenSec0Mean x
      f1 x = etaSys $ EqGen.solve seqTopo $ givenSec1Mean x

      sinkRangeSig :: Sig.PSignal [] Double
      sinkRangeSig = Sig.fromList sinkRange

      etaSys0, etaSys1 :: Sig.NSignal [] Double
      etaSys0 = Sig.fromList $ map f0 sinkRange
      etaSys1 = Sig.fromList $ map f1 sinkRange
-}
      varX, varY :: Sig.PSignal2 [] [] Double
      varX = Sig.fromList2 varX'
      varY = Sig.fromList2 varY'

      f0 x y = etaSys $ EqGen.solve seqTopo $ givenSec0Mean x y
      f1 x y = etaSys $ EqGen.solve seqTopo $ givenSec1Mean x y

      
      --etaSys0, etaSys1 :: Sig.NSignal2 [] [] Double
      -- etaSys0 = Sig.fromList2 $ map f0 (liftA (,) sinkRange sinkRangeMean)
      --etaSys1 = Sig.fromList2 $ map f1 (liftA (,) sinkRange sinkRangeMean)
      --etaSys1 = undefined


      etaSys0, etaSys1 :: Sig.NSignal2 [] [] Double
      etaSys0 = Sig.fromList2 $ zipWith (zipWith f0) varX' varY'
      etaSys1 = Sig.fromList2 $ zipWith (zipWith f1) varX' varY'

      --f = ("Sektion " ++) . (++ " Durchschnitt") . show


 -- PlotIO.xy "Test" DefaultTerm.cons id f
 --           sinkRangeSig [etaSys0, etaSys1]

      maxEtaSys :: Sig.NSignal2 [] [] Double
      maxEtaSys = Sig.zipWith max etaSys0 etaSys1

  concurrentlyMany_ [
    --PlotIO.surface "Test" DefaultTerm.cons id f varX varY [etaSys0, etaSys1],
    PlotIO.surface "Test" DefaultTerm.cons id (const "Max") varX varY maxEtaSys ]
