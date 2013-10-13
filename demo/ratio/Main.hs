-- | Demonstriert, wie man ein eta als Funktion definiert.

module Main where

import qualified EFA.Example.Topology.LinearOne as LinearOne
import EFA.Example.Topology.LinearOne (Node(Sink, Source))
import EFA.Application.Utility (quantityTopology, checkDetermined)

import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Variable as Var
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Topology.Absolute ((.=), (=.=))

import EFA.Equation.Result (Result)

import Data.Ratio ((%))

import Data.Monoid (mconcat, (<>))


enRange :: [Rational]
enRange = (1%100):[1%2, 1 .. 9]


type Expr s a x = EqSys.ExpressionIgnore Node s a x

c :: XIdx.Power Node
c = XIdx.power Source Sink

eta :: XIdx.Eta Node
eta = XIdx.eta Source Sink


functionEta :: (Fractional x) => Expr s a x -> Expr s a x
functionEta p = 0.2 * p

given :: Rational -> EqSys.EquationSystemIgnore Node s Rational
given p =
   mconcat $
   (XIdx.dTime .= 1) :
   (c .= p) :
   []


solve :: Rational -> String
solve p =
   show p ++ "\t" ++
   show (checkDetermined (show eta) $
         Var.checkedLookup "solve" FlowTopo.lookupEta eta $ solveGraph p)

solveGraph ::
   Rational -> FlowTopo.Section Node (Result Rational)
solveGraph p =
   EqSys.solve
      (quantityTopology LinearOne.topology)
      ((EqSys.variable eta =.= functionEta (EqSys.variable c)) <> given p)

main :: IO ()
main = do
   putStrLn $ unlines $ map solve enRange

   Draw.xterm $ Draw.flowTopology Draw.optionsDefault $
      FlowTopo.topology $ solveGraph 0.5
