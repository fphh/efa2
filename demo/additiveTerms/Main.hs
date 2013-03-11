{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Example.Utility as Utility
import EFA.Example.Utility
          (edgeVar, makeEdges, constructSeqTopo)
import EFA.Equation.Absolute ((.=))
import EFA.Equation.Stack (Stack)

import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Absolute as EqGen
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Signal.Plot as Plot

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)


import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty
import Data.Monoid (mempty, (<>))
import Data.Tuple.HT (mapFst)



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


type SignalTerm = Utility.SignalTerm Record.Delta SumProduct.Term Node.Int
type ScalarTerm = Utility.ScalarTerm Record.Delta SumProduct.Term Node.Int


type
   EquationSystemSymbolic s =
      EqGen.EquationSystem Node.Int s
         (Stack (Var.Any Node.Int) ScalarTerm)
         (Stack (Var.Any Node.Int) SignalTerm)

infixr 6 *=<>, -=<>

(*=<>) ::
   (Ord (idx Node.Int), Env.AccessMap idx,
    Var.Index idx, Var.Type idx ~ Var.Signal) =>
   idx Node.Int -> EquationSystemSymbolic s -> EquationSystemSymbolic s
idx *=<> eqsys =
   (idx .= (Stack.singleton $ Utility.symbol $ Idx.before $ Var.index idx))
   <>
   eqsys

(-=<>) ::
   (Ord (idx Node.Int), Env.AccessMap idx,
    Var.Index idx, Var.Type idx ~ Var.Signal) =>
   idx Node.Int -> EquationSystemSymbolic s -> EquationSystemSymbolic s
idx -=<> eqsys =
   (idx .=
      let var = Var.index idx
      in  Stack.deltaPair
             (Var.Signal var)
             (Utility.symbol (Idx.before var))
             (Utility.symbol (Idx.delta  var)))
   <>
   eqsys


givenSymbolic :: EquationSystemSymbolic s
givenSymbolic =
   (Idx.DTime Idx.initSection .= Arith.fromInteger 1) <>
   (Idx.DTime sec0 .= Arith.fromInteger 1) <>

   edgeVar Idx.Energy sec0 node0 node1 -=<>
   edgeVar Idx.Eta sec0 node0 node1 -=<>
   edgeVar Idx.Eta sec0 node1 node2 -=<>

   mempty


mainSymbolic :: IO ()
mainSymbolic = do

   let seqTopo = constructSeqTopo topoLinear [0]
   let env@(Env.Complete scalarEnv signalEnv) =
          EqGen.solve seqTopo givenSymbolic

   putStrLn $ Format.unUnicode $ formatValue $
      Env.Complete
         (fmap Record.unAbsolute scalarEnv)
         (fmap Record.unAbsolute signalEnv)

   Draw.sequFlowGraphAbsWithEnv seqTopo env



type
   EquationSystemNumeric s =
      EqGen.EquationSystem Node.Int s
         (Stack (Var.Any Node.Int) Double)
         (Stack (Var.Any Node.Int) Double)

deltaPair ::
   (Ord (idx Node.Int), Env.AccessMap idx,
    Var.Index idx, Var.Type idx ~ Var.Signal) =>
   idx Node.Int -> Double -> Double -> EquationSystemNumeric s
deltaPair idx before delta =
   idx .= Stack.deltaPair (Var.Signal $ Var.index idx) before delta


givenNumeric :: EquationSystemNumeric s
givenNumeric =
   (Idx.DTime Idx.initSection .= Arith.fromInteger 1) <>
   (Idx.DTime sec0 .= Arith.fromInteger 1) <>

   deltaPair (edgeVar Idx.Energy sec0 node0 node1) 4 (-0.6) <>
   deltaPair (edgeVar Idx.Eta sec0 node0 node1) 0.25 0.1 <>
   deltaPair (edgeVar Idx.Eta sec0 node1 node2) 0.85 0.05 <>

   mempty


eout :: Idx.Energy Node.Int
eout = edgeVar Idx.Energy sec0 node2 node1



mainNumeric :: IO ()
mainNumeric = do

   let seqTopo = constructSeqTopo topoLinear [0]
       Env.Complete _scalarEnv signalEnv = EqGen.solve seqTopo givenNumeric

   case Map.lookup eout (Env.energyMap signalEnv) of
      Nothing -> error "undefined E_2_1"
      Just d ->
         case Record.unAbsolute d of
            Result.Undetermined -> error "undetermined E_2_1"
            Result.Determined x -> do
               let assigns =
                      fmap (mapFst (foldl (\p i -> p * SumProduct.Atom i) 1)) $
                      NonEmpty.tail $
                      Stack.assigns x
               Fold.forM_ assigns $ \(term,val) -> do
                  putStrLn $
                     (Format.unUnicode $ formatValue term) ++ " = " ++ show val
               Plot.stackIO "Decomposition of total output energy"
                  (Idx.delta $ Var.index eout) assigns


main :: IO ()
main = mainNumeric >> mainSymbolic
