-- | Demonstriert, wie man ein eta als Funktion definiert.

module Main where

import qualified EFA.Application.Topology.LinearOne as LinearOne
import qualified EFA.Application.Absolute as EqGen
import EFA.Application.Topology.LinearOne (Node(Sink, Source))
import EFA.Application.Absolute ((.=), (=.=))
import EFA.Application.Utility (constructSeqTopo, checkDetermined)

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Equation.Environment as Env
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Map (checkedLookup)

import Data.Ratio ((%))

import Data.Monoid (mconcat, (<>))


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0


seqTopo :: Flow.RangeGraph Node
seqTopo = constructSeqTopo LinearOne.topology [0]

enRange :: [Rational]
enRange = (1%100):[1%2, 1 .. 9]


type Expr s a x = EqGen.Expression Node s a a x

c :: XIdx.Power Node
c = XIdx.power sec0 Source Sink

eta :: XIdx.Eta Node
eta = XIdx.eta sec0 Source Sink


functionEta :: (Fractional x) => Expr s a x -> Expr s a x
functionEta p = 0.2 * p

given :: Rational -> EqGen.EquationSystem Node s Rational Rational
given p =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (c .= p) :
   []


solve :: Rational -> String
solve p =
  show p ++ "\t"
        ++ show (checkDetermined (show eta) $
                   checkedLookup "solve" (Env.etaMap $ Env.signal $ solveEnv p) eta)

solveEnv ::
  Rational ->
  Env.Complete Node (Result Rational) (Result Rational)
solveEnv p =
  EqGen.solve seqTopo
    ((EqGen.variable eta =.= functionEta (EqGen.variable c)) <> given p)

main :: IO ()
main = do
  putStrLn $ unlines $ map solve enRange


  let env = solveEnv 0.5

  Draw.xterm $ Draw.sequFlowGraphAbsWithEnv seqTopo env
