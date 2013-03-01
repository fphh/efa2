module Main where

import EFA.Example.Utility
          (edgeVar, makeEdges, constructSeqTopo, (.=))

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Symbolic.OperatorTree as Op
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Env as Env

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)


import qualified Graphics.Gnuplot.Advanced as GP

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.LineSpecification as LineSpec


import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Foldable (foldMap, )
import Data.Monoid (mempty, (<>))
import Control.Functor.HT (void)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)

import System.IO


sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1, node2 :: Node.Int
node0 :~ node1 :~ node2 :~ _ = Stream.enumFrom minBound


topoLinear :: TD.Topology Node.Int
topoLinear = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Crossing),
              (node2, TD.Sink)]
        es = [(node0, node1), (node1, node2)]


{- |
Symbol equipped with a numeric value.
-}
data
   Symbol =
      Symbol {
         index :: Idx.Record Idx.Delta (Var.Index Node.Int),
         value :: Double
      }

instance Eq Symbol where
   (==)  =  equating index

instance Ord Symbol where
   compare  =  comparing index


infixr 6 =<>

(=<>) ::
   (Eq (term Symbol), Num (term Symbol), Ord (t Node.Int),
    Var.MkVarC term, Var.MkIdxC t, Env.AccessMap t) =>
   (Idx.Record Idx.Delta (t Node.Int), Double) ->
   EqGen.EquationSystem Env.Delta Node.Int s (term Symbol) ->
   EqGen.EquationSystem Env.Delta Node.Int s (term Symbol)
(idx, x) =<> eqsys =
   (idx .= Var.mkVarCore (Symbol (fmap Var.mkIdx idx) x)) <> eqsys


{-
Use SumProduct.Term here since it simplifies automatically.
-}
given ::
   EqGen.EquationSystem Env.Delta Node.Int s
      (SumProduct.Term Symbol)
given =
   (Idx.delta (Idx.DTime Idx.initSection) .= 0) <>
   (Idx.delta (Idx.DTime sec0) .= 0) <>

   (Idx.before (Idx.DTime Idx.initSection) .= 1) <>
   (Idx.before (Idx.DTime sec0) .= 1) <>

   (Idx.before (edgeVar Idx.Energy sec0 node0 node1), 4) =<>
   (Idx.before (edgeVar Idx.Eta sec0 node0 node1), 0.25) =<>
   (Idx.before (edgeVar Idx.Eta sec0 node1 node2), 0.85) =<>

   (Idx.delta (edgeVar Idx.Energy sec0 node0 node1), -0.6) =<>
   (Idx.delta (edgeVar Idx.Eta sec0 node0 node1), 0.1) =<>
   (Idx.delta (edgeVar Idx.Eta sec0 node1 node2), 0.05) =<>

   mempty


eout :: Idx.Energy Node.Int
eout = edgeVar Idx.Energy sec0 node2 node1

histogram ::
   (Fold.Foldable f, FormatValue term) =>
   f (term, Double) -> Frame.T (Graph2D.T Int Double)
histogram =
   Frame.cons (
      Opts.title "Decomposition of total output energy" $
      Histogram.rowstacked $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      Opts.xTicks2d [(Format.unASCII $ formatValue $
                      Idx.delta $ Var.mkIdx eout, 0)] $
      Opts.xRange2d (-1,3) $
      Opts.deflt) .
   foldMap (\(term,val) ->
      fmap (Graph2D.lineSpec
              (LineSpec.title (Format.unASCII $ formatValue term) LineSpec.deflt)) $
      Plot2D.list Graph2D.histograms [val])


main :: IO ()
main = do
  -- hSetEncoding stdout utf8
   let seqTopo = constructSeqTopo topoLinear [0]
       env = EqGen.solve given seqTopo

   case Map.lookup eout (Env.energyMap env) of
      Nothing -> error "undefined E_2_1"
      Just d ->
         case Env.delta d of
            Result.Undetermined -> error "undetermined E_2_1"
            Result.Determined x -> do
               let assigns =
                      fmap
                         (\symbol ->
                            (fmap index symbol,
                             Op.evaluate value symbol)) $
                      Op.group $ Op.expand $ Op.fromNormalTerm x
               Fold.forM_ assigns $ \(term,val) -> do
                  putStrLn $
                     (Format.unUnicode $ formatValue term) ++ " = " ++ show val
               void $ GP.plotDefault $ histogram assigns

   Draw.sequFlowGraphDeltaWithEnv seqTopo $
      fmap (fmap (fmap (SumProduct.map index))) env
