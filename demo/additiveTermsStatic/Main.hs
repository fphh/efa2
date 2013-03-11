{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Example.Utility as Utility
import EFA.Example.Utility
          (symbol, edgeVar, makeEdges, constructSeqTopo)
import EFA.Equation.Arithmetic ((~*))
import EFA.Equation.System ((=.=))
import EFA.Equation.Result (Result)

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Env as Env
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

import qualified Data.Accessor.Basic as Accessor

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty
import Control.Applicative (pure)
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

type IdxMultiDelta = Idx.ExtDelta (Idx.ExtDelta Idx.Delta)
type RecMultiDelta = Record.ExtDelta (Record.ExtDelta Record.Delta)

type
   EquationSystemSymbolic s =
      EqGen.EquationSystem RecMultiDelta Node.Int s ScalarTerm SignalTerm


infixr 9 &

(&) :: Idx.Delta -> a -> Idx.ExtDelta a
(&) = Idx.ExtDelta

absolute, param0, param1, param2 :: IdxMultiDelta
absolute = Idx.Before & Idx.Before & Idx.Before
param0 =   Idx.Before & Idx.Before & Idx.Delta
param1 =   Idx.Before & Idx.Delta  & Idx.Before
param2 =   Idx.Delta  & Idx.Before & Idx.Before


eout, ein :: Idx.Energy Node.Int
ein  = edgeVar Idx.Energy sec0 node0 node1
eout = edgeVar Idx.Energy sec0 node2 node1

eta0, eta1 :: Idx.Eta Node.Int
eta0 = edgeVar Idx.Eta sec0 node0 node1
eta1 = edgeVar Idx.Eta sec0 node1 node2


termFromIndex :: IdxMultiDelta -> SignalTerm
termFromIndex (Idx.ExtDelta r2 (Idx.ExtDelta r1 r0)) =
   symbol (Idx.Record r2 (Var.index ein)) ~*
   symbol (Idx.Record r1 (Var.index eta0)) ~*
   symbol (Idx.Record r0 (Var.index eta1))


{- |
Caution:
This function creates an inconsistent nested Delta record.
The equations before+delta=after are not (always) satisfied.
This is ok for application since we only use the before and delta values
to initialize the equation system.
-}
parameterSymbol ::
   (t ~ Utility.VarTerm var Idx.Delta SumProduct.Term Node.Int,
    Eq t, Arith.Sum t, Arith.Constant t,
    Ord (idx Node.Int),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   IdxMultiDelta -> idx Node.Int -> RecMultiDelta t

parameterSymbol param idx =
   Accessor.set (Record.access param) (symbol (Idx.delta $ Var.index idx)) $
   absoluteSymbol idx

{- | Caution: See 'parameterSymbol' -}
absoluteSymbol ::
   (t ~ Utility.VarTerm var Idx.Delta SumProduct.Term Node.Int,
    Eq t, Arith.Sum t, Arith.Constant t,
    Ord (idx Node.Int),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   idx Node.Int -> RecMultiDelta t

absoluteSymbol idx =
   absoluteRecord (symbol (Idx.before $ Var.index idx))

{- | Caution: See 'parameterSymbol' -}
parameterRecord ::
   (Arith.Constant x) =>
   IdxMultiDelta -> x -> x -> RecMultiDelta x
parameterRecord param x d =
   Accessor.set (Record.access param) d $
   absoluteRecord x

{- | Caution: See 'parameterSymbol' -}
absoluteRecord ::
   (Arith.Constant x) =>
   x -> RecMultiDelta x
absoluteRecord x =
   Accessor.set (Record.access absolute) x $
   pure Arith.zero


equalDelta ::
   Eq x =>
   Record.Delta (EqGen.Expression rec node s a v x) ->
   Record.Delta (EqGen.Expression rec node s a v x) ->
   EqGen.EquationSystem rec node s a v
equalDelta x y =
   (Record.before x =.= Record.before y) <>
   (Record.delta  x =.= Record.delta  y)

equalExtDelta ::
   (f x -> f x -> EqGen.EquationSystem rec node s a v) ->
   Record.ExtDelta f x ->
   Record.ExtDelta f x ->
   EqGen.EquationSystem rec node s a v
equalExtDelta eq x y =
   eq (Record.extBefore x) (Record.extBefore y) <>
   eq (Record.extDelta  x) (Record.extDelta  y)


infix 0 =%=, %=

(=%=) ::
   Eq x =>
   RecMultiDelta (EqGen.Expression rec node s a v x) ->
   RecMultiDelta (EqGen.Expression rec node s a v x) ->
   EqGen.EquationSystem rec node s a v
(=%=) = equalExtDelta (equalExtDelta equalDelta)

(%=) ::
   (Eq x, Arith.Sum x,
    EqGen.Element idx RecMultiDelta s a v
       ~ EqGen.VariableRecord RecMultiDelta s x,
    Env.AccessMap idx, Ord (idx node), Var.Type idx ~ var) =>
   idx node -> RecMultiDelta x ->
   EqGen.EquationSystem RecMultiDelta node s a v
evar %= val  =
   fmap (EqGen.variable . flip Idx.Record evar) Record.indices
   =%=
   fmap EqGen.constant val


givenParameterSymbol ::
   (t ~ Utility.VarTerm var Idx.Delta SumProduct.Term Node.Int,
    Eq t, Arith.Sum t, Arith.Constant t,
    EqGen.Element idx RecMultiDelta s ScalarTerm SignalTerm
       ~ EqGen.VariableRecord RecMultiDelta s t,
    Ord (idx Node.Int),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   IdxMultiDelta -> idx Node.Int ->
   EquationSystemSymbolic s
givenParameterSymbol param idx =
   idx %= parameterSymbol param idx

givenSymbolic :: EquationSystemSymbolic s
givenSymbolic =
   (Idx.DTime Idx.initSection %= absoluteRecord (Arith.fromInteger 1)) <>
   (Idx.DTime sec0 %= absoluteRecord (Arith.fromInteger 1)) <>

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
   IdxMultiDelta -> idx Node.Int -> Double -> Double -> EquationSystemNumeric s
givenParameterNumber param idx before delta =
   idx %= parameterRecord param before delta


givenNumeric :: EquationSystemNumeric s
givenNumeric =
   (Idx.DTime Idx.initSection %= absoluteRecord 1) <>
   (Idx.DTime sec0 %= absoluteRecord 1) <>

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
