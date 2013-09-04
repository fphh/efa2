-- | Demonstriert, wie man ein eta als Funktion definiert.
module Main where

import qualified EFA.Application.Topology.LinearOne as LinearOne
import qualified EFA.Application.Absolute as EqGen
import EFA.Application.Topology.LinearOne (Node(Sink, Source))
import EFA.Application.Absolute ((.=), (=.=))
import EFA.Application.Utility (constructSeqTopo)

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Equation.Environment as Env

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


type Expr s a = EqGen.Expression Node s Double Double a

c :: XIdx.Power Node
c = XIdx.power sec0 Source Sink

eta :: XIdx.Eta Node
eta = XIdx.eta sec0 Source Sink


functionEta :: Expr s Double -> Expr s Double
functionEta = EqGen.liftF $ \p -> 0.3 * sqrt p

given :: Double -> EqGen.EquationSystem Node s Double Double
given p =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (XIdx.power sec0 Source Sink .= p) :
   []


solve :: Double -> String
solve p =
  let env =
         EqGen.solve seqTopo
            ((EqGen.variable eta =.= functionEta (EqGen.variable c)) <> given p)
  in  show p ++ " " ++
      Format.unUnicode (formatValue
         (checkedLookup "solve" (Env.etaMap (Env.signal env)) eta))

main :: IO ()
main =
  putStrLn $ unlines $ map solve enRange
