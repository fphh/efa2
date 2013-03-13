{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Example.Utility as Utility
import EFA.Example.Utility
          (symbol, edgeVar, makeEdges, constructSeqTopo)
import EFA.Equation.Arithmetic ((~-), (~*))
import EFA.Equation.System ((=.=))
import EFA.Equation.Result (Result)

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Symbolic.OperatorTree as Op
import qualified EFA.Symbolic.Mixed as Term

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
import Control.Applicative (Applicative, pure, liftA2)
import Data.Monoid (mempty, (<>))
import Data.Tuple.HT (mapFst, mapSnd)



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

type IdxMultiDelta = Idx.ExtDelta (Idx.ExtDelta (Idx.ExtDelta Idx.Absolute))
type RecMultiDelta = Record.ExtDelta (Record.ExtDelta (Record.ExtDelta Record.Absolute))

type
   EquationSystemSymbolic s =
      EqGen.EquationSystem RecMultiDelta Node.Int s ScalarTerm SignalTerm



clear :: Arith.Sum a => a -> a
clear x = x~-x


data Extruder f a =
   Extruder {
      extrudeOuter :: f (Maybe a) -> Record.ExtDelta f (Maybe a),
      extrudeInner ::
         (a -> f (Maybe a)) ->
         a -> a -> Record.ExtDelta f (Maybe a)
   }

extrudeStart :: InnerExtrusion Record.Absolute a
extrudeStart = InnerExtrusion $ Record.Absolute . Just


beforeDelta :: (Applicative f, Arith.Sum a) => Extruder f a
beforeDelta =
   Extruder {
      extrudeOuter = \x ->
         Record.ExtDelta {
            Record.extBefore = x,
            Record.extAfter = pure Nothing,
            Record.extDelta = fmap (fmap clear) x
         },
      extrudeInner = \cons x y ->
         Record.ExtDelta {
            Record.extBefore = cons x,
            Record.extAfter = pure Nothing,
            Record.extDelta = cons y
         }
   }


newtype
   InnerExtrusion f a =
      InnerExtrusion {runInnerExtrusion :: a -> f (Maybe a)}
newtype
   OuterExtrusion f a =
      OuterExtrusion {runOuterExtrusion :: a -> a -> f (Maybe a)}


infixr 0 <&, <&>, &>

(&>) ::
   (Arith.Sum a) =>
   Extruder f a ->
   InnerExtrusion f a ->
   InnerExtrusion (Record.ExtDelta f) a
e &> InnerExtrusion f =
   InnerExtrusion $ \x -> extrudeInner e f x (clear x)

(<&>) ::
   (Arith.Sum a) =>
   Extruder f a ->
   InnerExtrusion f a ->
   OuterExtrusion (Record.ExtDelta f) a
e <&> InnerExtrusion f = OuterExtrusion $ extrudeInner e f

(<&) ::
   (Arith.Sum a) =>
   Extruder f a ->
   OuterExtrusion f a ->
   OuterExtrusion (Record.ExtDelta f) a
e <& OuterExtrusion f =
   OuterExtrusion $ \x y -> extrudeOuter e $ f x y



absolute :: (Arith.Sum a) => InnerExtrusion RecMultiDelta a
absolute = beforeDelta &> beforeDelta  &> beforeDelta  &> extrudeStart

param0, param1, param2 :: (Arith.Sum a) => OuterExtrusion RecMultiDelta a
param0 = beforeDelta <&  beforeDelta <&  beforeDelta <&> extrudeStart
param1 = beforeDelta <&  beforeDelta <&> beforeDelta  &> extrudeStart
param2 = beforeDelta <&> beforeDelta  &> beforeDelta  &> extrudeStart


eout, ein :: Idx.Energy Node.Int
ein  = edgeVar Idx.Energy sec0 node0 node1
eout = edgeVar Idx.Energy sec0 node2 node1

eta0, eta1 :: Idx.Eta Node.Int
eta0 = edgeVar Idx.Eta sec0 node0 node1
eta1 = edgeVar Idx.Eta sec0 node1 node2


termFromIndex :: IdxMultiDelta -> SignalTerm
termFromIndex
      (Idx.ExtDelta r2 (Idx.ExtDelta r1 (Idx.ExtDelta r0 Idx.Absolute))) =
   symbol (Idx.Record r2 (Var.index ein)) ~*
   symbol (Idx.Record r1 (Var.index eta0)) ~*
   symbol (Idx.Record r0 (Var.index eta1))



parameterSymbol ::
   (t ~ Utility.VarTerm var Idx.Delta SumProduct.Term Node.Int,
    Eq t, Arith.Sum t, Arith.Constant t,
    Ord (idx Node.Int),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   OuterExtrusion RecMultiDelta t ->
   idx Node.Int -> RecMultiDelta (Maybe t)

parameterSymbol param idx =
   runOuterExtrusion param
      (symbol (Idx.before $ Var.index idx))
      (symbol (Idx.delta $ Var.index idx))

absoluteSymbol ::
   (t ~ Utility.VarTerm var Idx.Delta SumProduct.Term Node.Int,
    Eq t, Arith.Sum t, Arith.Constant t,
    Ord (idx Node.Int),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   idx Node.Int -> RecMultiDelta (Maybe t)

absoluteSymbol idx =
   absoluteRecord (symbol (Idx.before $ Var.index idx))

parameterRecord ::
   (Arith.Sum x) =>
   OuterExtrusion RecMultiDelta x ->
   x -> x -> RecMultiDelta (Maybe x)
parameterRecord = runOuterExtrusion

absoluteRecord ::
   (Arith.Sum x) =>
   x -> RecMultiDelta (Maybe x)
absoluteRecord = runInnerExtrusion absolute


infix 0 ?=


(?=) ::
   (Eq x, Arith.Sum x,
    EqGen.Element idx RecMultiDelta s a v
       ~ EqGen.VariableRecord RecMultiDelta s x,
    Env.AccessMap idx, Ord (idx node), Var.Type idx ~ var) =>
   idx node -> RecMultiDelta (Maybe x) ->
   EqGen.EquationSystem RecMultiDelta node s a v
evar ?= val  =
   Fold.fold $
   liftA2
      (\rec ->
         Fold.foldMap
            (\x -> EqGen.variable (Idx.Record rec evar) =.= EqGen.constant x))
      Record.indices val


givenParameterSymbol ::
   (t ~ Utility.VarTerm var Idx.Delta SumProduct.Term Node.Int,
    Eq t, Arith.Sum t, Arith.Constant t,
    EqGen.Element idx RecMultiDelta s ScalarTerm SignalTerm
       ~ EqGen.VariableRecord RecMultiDelta s t,
    Ord (idx Node.Int),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   OuterExtrusion RecMultiDelta t -> idx Node.Int ->
   EquationSystemSymbolic s
givenParameterSymbol param idx =
   idx ?= parameterSymbol param idx

givenSymbolic :: EquationSystemSymbolic s
givenSymbolic =
   (Idx.DTime Idx.initSection ?= absoluteRecord (Arith.fromInteger 1)) <>
   (Idx.DTime sec0 ?= absoluteRecord (Arith.fromInteger 1)) <>

   givenParameterSymbol param2 ein <>
   givenParameterSymbol param1 eta0 <>
   givenParameterSymbol param0 eta1 <>

   mempty


simplify ::SignalTerm -> SignalTerm
simplify =
   Term.Signal . Op.toNormalTerm .
   NonEmpty.sum . Op.expand .
   Op.fromNormalTerm . Term.getSignal

simplifiedSummands ::
   RecMultiDelta (Result SignalTerm) -> NonEmpty.T [] (Result SignalTerm)
simplifiedSummands =
   fmap (fmap simplify) . Record.summands

mainSymbolic :: IO ()
mainSymbolic = do

   let seqTopo = constructSeqTopo topoLinear [0]
   let (Env.Complete scalarEnv signalEnv) =
          EqGen.solve seqTopo givenSymbolic

   putStrLn $ Format.unUnicode $ formatValue $
      Env.Complete
         (fmap Record.summands scalarEnv)
         (fmap simplifiedSummands signalEnv)

   Draw.sequFlowGraphAbsWithEnv seqTopo $
      Env.Complete
         (fmap (Record.Absolute . Record.summands) scalarEnv)
         (fmap (Record.Absolute . simplifiedSummands) signalEnv)


type
   EquationSystemNumeric s =
      EqGen.EquationSystem RecMultiDelta Node.Int s Double Double

givenParameterNumber ::
   (Ord (idx Node.Int), Env.AccessMap idx,
    Var.Index idx, Var.Type idx ~ Var.Signal) =>
   (OuterExtrusion RecMultiDelta Double) ->
   idx Node.Int -> Double -> Double -> EquationSystemNumeric s
givenParameterNumber param idx before delta =
   idx ?= parameterRecord param before delta


givenNumeric :: EquationSystemNumeric s
givenNumeric =
   (Idx.DTime Idx.initSection ?= absoluteRecord 1) <>
   (Idx.DTime sec0 ?= absoluteRecord 1) <>

   givenParameterNumber param2 ein 4 (-0.6) <>
   givenParameterNumber param1 eta0 0.25 0.1 <>
   givenParameterNumber param0 eta1 0.85 0.05 <>

   mempty



checkDetermined :: Result a -> a
checkDetermined rx =
   case rx of
      Result.Undetermined -> error "undetermined"
      Result.Determined x -> x

mainNumeric :: IO ()
mainNumeric = do

   let seqTopo = constructSeqTopo topoLinear [0]
       Env.Complete _scalarEnv signalEnv = EqGen.solve seqTopo givenNumeric

   case Map.lookup eout (Env.energyMap signalEnv) of
      Nothing -> error "undefined E_2_1"
      Just x -> do
         let assigns =
                map (mapFst termFromIndex) $
                NonEmpty.tail $ Record.assigns x
         Fold.forM_ assigns $ \(term,val) -> do
            putStrLn $ Format.unUnicode $
               Format.assign (formatValue term) (formatValue val)
         Plot.stackIO "Decomposition of total output energy"
            (Idx.delta $ Var.index eout) (map (mapSnd checkDetermined) assigns)


main :: IO ()
main = mainNumeric >> mainSymbolic
