-- | Demonstriert, wie man ein eta als Funktion definiert.
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


type Expr s a = EqSys.ExpressionIgnore Node s Double Double a

c :: XIdx.Power Node
c = XIdx.power sec0 Source Sink

eta :: XIdx.Eta Node
eta = XIdx.eta sec0 Source Sink


functionEta :: Expr s Double -> Expr s Double
functionEta = EqSys.liftF $ \p -> 0.3 * sqrt p

given :: Double -> EqSys.EquationSystemIgnore Node s Double Double
given p =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (XIdx.power sec0 Source Sink .= p) :
   []


solve :: Double -> String
solve p =
   let env =
          EqSys.solve flowGraph
             ((EqSys.variable eta =.= functionEta (EqSys.variable c)) <> given p)
   in  show p ++ " " ++
       Format.unUnicode (formatValue
          (Var.checkedLookup "solve" SeqFlow.lookupEta eta env))

main :: IO ()
main =
   putStrLn $ unlines $ map solve enRange
