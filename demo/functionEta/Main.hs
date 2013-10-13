-- | Demonstriert, wie man ein eta als Funktion definiert.
module Main where

import qualified EFA.Example.Topology.LinearOne as LinearOne
import EFA.Example.Topology.LinearOne (Node(Sink, Source))
import EFA.Application.Utility (quantityTopology)

import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx
import EFA.Flow.Topology.Absolute ((.=), (=.=))

import qualified EFA.Equation.Variable as Var

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


functionEta :: Expr s Double -> Expr s Double
functionEta = EqSys.liftF $ \p -> 0.3 * sqrt p

given :: Double -> EqSys.EquationSystemIgnore Node s Double
given p =
   mconcat $
   (XIdx.dTime .= 1) :
   (XIdx.power Source Sink .= p) :
   []


solve :: Double -> String
solve p =
   let env =
          EqSys.solve
             (quantityTopology LinearOne.topology)
             ((EqSys.variable eta =.= functionEta (EqSys.variable c)) <> given p)
   in  show p ++ " " ++
       Format.unUnicode (formatValue
          (Var.checkedLookup "solve" FlowTopo.lookupEta eta env))

main :: IO ()
main =
   putStrLn $ unlines $ map solve enRange
