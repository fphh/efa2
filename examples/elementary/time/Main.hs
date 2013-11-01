
module Main where

import EFA.Example.Topology.Tripod.State (flowGraph, sec0, sec1)
import EFA.Example.Topology.Tripod (Node, node0, node1, node2, node3)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((.=), (=.=))

import qualified EFA.Flow.SequenceState.Variable as Var
import EFA.Equation.Arithmetic ((^!))

import EFA.Utility.Async (concurrentlyMany_)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))

import Data.Monoid ((<>))


type Expr s a = EqSys.ExpressionIgnore Node s a a a

n1, n2, n3, n4, n5, n6 :: Expr s Double -> Expr s Double

-- steigend und fallend

n1 = EqSys.liftF $ \x ->  0.95*exp(-0.05*(x-2)^!2)
n2 = EqSys.liftF $ \x -> -0.021 * (x - 12) * x

-- fallend
n3 = EqSys.liftF $ \x -> 1/(0.063*(x+4)^!2)
n4 = EqSys.liftF $ \x -> exp(-0.05*x^!2)
n6 = EqSys.liftF $ \x -> 1-1/(1+1000*exp(-(x-2)))

n7 :: Expr s Double -> Expr s Double -> Expr s Double
n7 =
  EqSys.liftF2 $
    \ p s ->
      let n = 1-1/(1+1000*exp(-(p-2)))
      in  if p/n > s then 0 else n

-- steigend
n5 = EqSys.liftF $ \x -> x/sqrt(1+(x+2)^!2)


n02, n21, n23, n32, p20 :: Idx.Section -> Expr s a
n02 sec = EqSys.variable $ XIdx.eta sec node0 node2
n21 sec = EqSys.variable $ XIdx.eta sec node2 node1
n23 sec = EqSys.variable $ XIdx.eta sec node2 node3
n32 sec = EqSys.variable $ XIdx.eta sec node3 node2
p20 sec = EqSys.variable $ XIdx.power sec node2 node0

esto :: XIdx.StEnergy Node
esto = XIdx.stEnergy XIdx.initSection sec1 node3

ein, eout0, eout1 :: XIdx.Energy Node
ein = XIdx.energy sec0 node0 node2
eout0 = XIdx.energy sec0 node1 node2
eout1 = XIdx.energy sec1 node1 node2


given :: Double -> Double -> EqSys.EquationSystemIgnore Node s Double Double
given _x t =
  (n02 sec0 =.= n5 (p20 sec0))
  <> (n21 sec0 =.= 1)
  <> (n21 sec1 =.= 1)
  <> (n23 sec0 =.= 1)
  <> (n32 sec1 =.= 1)
  <> (EqSys.variable (XIdx.energy sec1 node3 node2)
        =.= EqSys.variable (XIdx.energy sec0 node3 node2))
  <> (EqSys.variable (XIdx.dTime sec0) + EqSys.variable (XIdx.dTime sec1) =.= 12.1)

  <> (XIdx.dTime sec0 .= t)
  <> (XIdx.storage Idx.initial node3 .= 10)

  <> (XIdx.power sec0 node1 node2 .= 10)
  <> (XIdx.power sec1 node1 node2 .= 10)


xrange, trange :: [Double]
xrange = [0.1, 0.15 .. 0.9] ++ [1]
trange = 0.01:[0.5, 1 .. 12]

solve :: Double -> Double -> String
solve x e =
   let solved = EqSys.solve flowGraph (given x e)
       emap idx = Var.checkedLookup "solve" SeqFlow.lookupSignal idx solved
       stemap idx = Var.checkedLookup "solve" SeqFlow.lookupScalar idx solved
       f _es ei eo0 eo1 = (eo0 + eo1) / ei
   in  show x ++ " " ++ show e ++ " " ++
       Format.unUnicode (formatValue
          (f <$> stemap esto
             <*> emap ein
             <*> emap eout0
             <*> emap eout1))


main :: IO ()
main = do
   when False $
      putStrLn $ unlines $ map (\x -> unlines $ map (solve x) trange) xrange

   let solved = EqSys.solve flowGraph (given 0.5 1)

   concurrentlyMany_ [
      Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault solved ]
