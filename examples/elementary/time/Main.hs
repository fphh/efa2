
module Main where

import qualified EFA.Example.Topology.TripodB as Tripod
import EFA.Example.Topology.TripodB (Node, node0, node1, node2, node3)

import EFA.Application.Utility ( seqFlowGraphFromStates, )

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((.=), (=.=))

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Variable as Var
import EFA.Equation.Result (Result)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import Control.Applicative ((<$>), (<*>)) -- (liftA4)

import Data.Monoid ((<>))


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0


flowGraph :: SeqFlow.Graph Node (Result a) (Result v)
flowGraph = seqFlowGraphFromStates Tripod.topology [0, 4]


type Expr s a = EqSys.ExpressionIgnore Node s a a a

n1, n2, n3, n4, n5, n6 :: Expr s Double -> Expr s Double

-- steigend und fallend

n1 = EqSys.liftF $ \x ->  0.95*exp(-0.05*((x-2)*(x-2)))
n2 = EqSys.liftF $ \x -> -0.021 * (x - 12) * x

-- fallend
n3 = EqSys.liftF $ \x -> 1/(0.063*(x+4)*(x+4))
n4 = EqSys.liftF $ \x -> exp(-0.05*x*x)
n6 = EqSys.liftF $ \x -> 1-1/(1+1000*exp(-(x-2)))

n7 :: Expr s Double -> Expr s Double -> Expr s Double
n7 =
  EqSys.liftF2 $
    \ p s ->
      let n = 1-1/(1+1000*exp(-(p-2)))
      in  if p/n > s then 0 else n

-- steigend
n5 = EqSys.liftF $ \x -> x/sqrt(1+(x+2)*(x+2))


n01, n12, n13, n31, p10, p12, p21, p13, p31 ::
   (Eq a, Arith.Sum a) => Idx.Section -> Expr s a
n01 sec = EqSys.variable $ XIdx.eta sec node0 node1
n12 sec = EqSys.variable $ XIdx.eta sec node1 node2
n13 sec = EqSys.variable $ XIdx.eta sec node1 node3
n31 sec = EqSys.variable $ XIdx.eta sec node3 node1
p10 sec = EqSys.variable $ XIdx.power sec node1 node0
p12 sec = EqSys.variable $ XIdx.power sec node1 node2
p21 sec = EqSys.variable $ XIdx.power sec node2 node1
p13 sec = EqSys.variable $ XIdx.power sec node1 node3
p31 sec = EqSys.variable $ XIdx.power sec node3 node1

--esto :: Expr s Double
esto :: XIdx.StEnergy Node
esto = XIdx.stEnergy XIdx.initSection sec1 node3

ein, eout0, eout1 :: XIdx.Energy Node
ein = XIdx.energy sec0 node0 node1
eout0 = XIdx.energy sec0 node2 node1
eout1 = XIdx.energy sec1 node2 node1


sto0, sto1 :: XIdx.Storage Node
sto0 = XIdx.storage (Idx.afterSection sec0) node3
sto1 = XIdx.storage (Idx.afterSection sec1) node3


given :: Double -> Double -> EqSys.EquationSystemIgnore Node s Double Double
given _x t =
  (n01 sec0 =.= n5 (p10 sec0))
  <> (n12 sec0 =.= 1) -- n5 (p21 sec0))
  <> (n12 sec1 =.= 1) -- n5 (p12 sec1))
  <> (n13 sec0 =.= 1) -- n1 (p12 sec0))
  <> (n31 sec1 =.= 1) -- n1 (p31 sec1))
  <> (EqSys.variable (XIdx.energy sec1 node3 node1)
        =.= EqSys.variable (XIdx.energy sec0 node3 node1))
  <> (EqSys.variable (XIdx.dTime sec0) + EqSys.variable (XIdx.dTime sec1) =.= 12.1)

  <> (XIdx.dTime sec0 .= t)
  <> (XIdx.storage Idx.initial node3 .= 10)

  -- <> (edgeVar EqSys.xfactor sec0 node1 node2 .= x)
  <> (XIdx.power sec0 node2 node1 .= 10)
  <> (XIdx.power sec1 node2 node1 .= 10)

{-
t0range, t1range, xrange, erange :: [Double]
t0range = [0.1, 0.2 .. 2]
t1range = [0.1, 0.2 .. 2.2]
erange = 0.01:[1, 1.5 .. 11] ++ [11.9]
-}


xrange, trange :: [Double]
xrange = [0.1, 0.15 .. 0.9] ++ [1]
trange = 0.01:[0.5, 1 .. 12] -- ++ [11.9]

solve :: Double -> Double -> String
solve x e =
   let solved = EqSys.solve flowGraph (given x e)
       emap idx = Var.checkedLookup "solve" SeqFlow.lookupEnergy idx solved
       stemap idx = Var.checkedLookup "solve" SeqFlow.lookupStEnergy idx solved
 --      smap idx = Var.checkedLookup "solve" SeqFlow.lookupStorage idx solved
       f _es ei eo0 eo1 = (eo0 + eo1) / ei -- (es + ei)
   in  show x ++ " " ++ show e ++ " " ++
 --        Format.unUnicode (formatValue (smap sto0))
 --        Format.unUnicode (formatValue
 {-
             (liftA2 (/) (smap sto0) (smap sto1)))
 -}
       Format.unUnicode (formatValue
          (f <$> stemap esto
             <*> emap ein
             <*> emap eout0
             <*> emap eout1))


main :: IO ()
main = do
   -- putStrLn $ unlines $ map (\x -> unlines $ map (solve x) trange) xrange

   let solved = EqSys.solve flowGraph (given 0.5 1)
   -- let solved' = EqSys.solve (given 0.9 undefined) flowGraph

   concurrentlyMany_ [
      Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault solved ]
      -- Draw.seqFlowGraph Draw.optionsDefault "" flowGraph solved' ]
