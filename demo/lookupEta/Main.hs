
-- | Demonstriert, wie man ein eta mit Hilfe von Lookup-Tables definiert.

module Main where

import qualified EFA.Application.Topology.LinearOne as LinearOne
import qualified EFA.Application.Absolute as EqSys
import EFA.Application.Topology.LinearOne (Node(Sink, Source))
import EFA.Application.Absolute ((.=), (=.=))
import EFA.Application.Utility (constructSeqTopo)

import qualified EFA.Equation.Environment as Env
import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Map (checkedLookup)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import Data.Monoid (mconcat, (<>))


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0

seqTopo :: Flow.RangeGraph Node
seqTopo = constructSeqTopo LinearOne.topology [0]

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
   EqSys.Expression Node s a v Double ->
   EqSys.Expression Node s a v Double
lookupEta = EqSys.liftF $ eval table
  where table = zip [0..9] [0, 0.1, 0.3, 0.6, 0.7, 0.65, 0.6, 0.4, 0.35, 0.1]

given :: Double -> EqSys.EquationSystem Node s Double Double
given p =
   mconcat $

   (XIdx.dTime sec0 .= 1) :
   (c .= p) :
   []


solve :: Double -> String
solve p =
  let env =
         EqSys.solve seqTopo
            ((EqSys.variable eta =.= lookupEta (EqSys.variable c))
               <> given p)
  in  show p ++ " " ++
      Format.unUnicode (formatValue
         (checkedLookup "solve" (Env.etaMap $ Env.signal env) eta))

main :: IO ()
main =
  putStrLn $ unlines $ map solve enRange
