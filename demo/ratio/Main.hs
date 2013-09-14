-- | Demonstriert, wie man ein eta als Funktion definiert.

module Main where

import qualified EFA.Application.Topology.LinearOne as LinearOne
import EFA.Application.Topology.LinearOne (Node(Sink, Source))
import EFA.Application.Utility (seqFlowGraphFromStates, checkDetermined)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((.=), (=.=))

import qualified EFA.Equation.Variable as Var
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import Data.Ratio ((%))

import Data.Monoid (mconcat, (<>))


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0


flowGraph :: SeqFlow.Graph Node (Result a) (Result v)
flowGraph = seqFlowGraphFromStates LinearOne.topology [0]

enRange :: [Rational]
enRange = (1%100):[1%2, 1 .. 9]


type Expr s a x = EqSys.ExpressionIgnore Node s a a x

c :: XIdx.Power Node
c = XIdx.power sec0 Source Sink

eta :: XIdx.Eta Node
eta = XIdx.eta sec0 Source Sink


functionEta :: (Fractional x) => Expr s a x -> Expr s a x
functionEta p = 0.2 * p

given :: Rational -> EqSys.EquationSystemIgnore Node s Rational Rational
given p =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (c .= p) :
   []


solve :: Rational -> String
solve p =
   show p ++ "\t" ++
   show (checkDetermined (show eta) $
         Var.checkedLookup "solve" SeqFlow.lookupEta eta $ solveGraph p)

solveGraph ::
   Rational ->
   SeqFlow.Graph Node (Result Rational) (Result Rational)
solveGraph p =
   EqSys.solve flowGraph
      ((EqSys.variable eta =.= functionEta (EqSys.variable c)) <> given p)

main :: IO ()
main = do
   putStrLn $ unlines $ map solve enRange

   Draw.xterm $ Draw.sequFlowGraph Draw.optionsDefault $ solveGraph 0.5
