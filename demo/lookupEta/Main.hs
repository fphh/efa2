
-- | Demonstriert, wie man ein eta mit Hilfe von Lookup-Tables definiert.

module Main where

import qualified EFA.Example.Topology.LinearOne as LinearOne
import EFA.Example.Topology.LinearOne (Node(Sink, Source))
import EFA.Application.Utility (quantityTopology)

import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Variable as Var
import qualified EFA.Flow.Topology.Index as XIdx
import EFA.Flow.Topology.Absolute ((.=), (=.=))

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import Data.Monoid (mconcat, (<>))


enRange :: [Double]
enRange = 0.01:[0.5, 1 .. 9]


type Expr s a = EqSys.ExpressionIgnore Node s Double a

c :: XIdx.Power Node
c = XIdx.power Source Sink

eta :: XIdx.Eta Node
eta = XIdx.eta Source Sink

eval :: [(Double, Double)] -> Double -> Double
eval lt pin =
  case dropWhile ((< pin) . fst) lt of
       [] -> 0
       (_, v):_ -> v


lookupEta :: Expr s Double -> Expr s Double
lookupEta = EqSys.liftF $ eval table
   where table = zip [0..9] [0, 0.1, 0.3, 0.6, 0.7, 0.65, 0.6, 0.4, 0.35, 0.1]

given :: Double -> EqSys.EquationSystemIgnore Node s Double
given p =
   mconcat $

   (XIdx.dTime .= 1) :
   (c .= p) :
   []


solve :: Double -> String
solve p =
   let env =
          EqSys.solve
             (quantityTopology LinearOne.topology)
             ((EqSys.variable eta =.= lookupEta (EqSys.variable c)) <> given p)
   in  show p ++ " " ++
       Format.unUnicode (formatValue
          (Var.checkedLookup "solve" FlowTopo.lookupEta eta env))

main :: IO ()
main =
   putStrLn $ unlines $ map solve enRange
