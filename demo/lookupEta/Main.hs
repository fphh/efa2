
-- | Demonstriert, wie man ein eta mit Hilfe von Lookup-Tables definiert.

module Main where

import qualified EFA.Example.Topology.LinearOne as LinearOne
import EFA.Example.Topology.LinearOne (Node(Sink, Source))

import EFA.Application.Utility (seqFlowGraphFromStates)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import EFA.Flow.Sequence.Absolute ((.=), (=.=))

import qualified EFA.Equation.Variable as Var
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import Data.Monoid (mconcat, (<>))


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0

flowGraph :: SeqFlow.Graph Node (Result a) (Result v)
flowGraph = seqFlowGraphFromStates LinearOne.topology [0]

enRange :: [Double]
enRange = 0.01:[0.5, 1 .. 9]

c :: XIdx.Power Node
c = XIdx.power sec0 Source Sink

eta :: XIdx.Eta Node
eta = XIdx.eta sec0 Source Sink

eval :: [(Double, Double)] -> Double -> Double
eval lt pin =
  case dropWhile ((< pin) . fst) lt of
       [] -> 0
       (_, v):_ -> v


lookupEta ::
   EqSys.ExpressionIgnore Node s a v Double ->
   EqSys.ExpressionIgnore Node s a v Double
lookupEta = EqSys.liftF $ eval table
   where table = zip [0..9] [0, 0.1, 0.3, 0.6, 0.7, 0.65, 0.6, 0.4, 0.35, 0.1]

given :: Double -> EqSys.EquationSystemIgnore Node s Double Double
given p =
   mconcat $

   (XIdx.dTime sec0 .= 1) :
   (c .= p) :
   []


solve :: Double -> String
solve p =
  let env =
         EqSys.solve flowGraph
            ((EqSys.variable eta =.= lookupEta (EqSys.variable c))
               <> given p)
  in  show p ++ " " ++
      Format.unUnicode (formatValue
         (Var.checkedLookup "solve" SeqFlow.lookupEta eta env))

main :: IO ()
main =
   putStrLn $ unlines $ map solve enRange
