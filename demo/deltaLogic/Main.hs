{-# LANGUAGE Rank2Types #-}
module Main where

import qualified EFA.Example.Topology.LinearOne as LinearOne
import EFA.Example.Topology.LinearOne (Node(Source, Sink))

import EFA.Application.Utility (quantityTopology)

import qualified EFA.Flow.Topology.Symbolic as Symbolic
import qualified EFA.Flow.Topology.AssignMap as AssignMap
import qualified EFA.Flow.Topology.EquationSystem as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx
import EFA.Flow.Topology.Symbolic ((=<>))
import EFA.Flow.Topology.EquationSystem ((=%=))

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import EFA.Equation.Arithmetic ((~*))

import qualified EFA.Report.Format as Format

import qualified System.IO as IO
import Data.Monoid (mempty, (<>))


{-
Use SumProduct.Term here since it simplifies automatically.
-}
given, sys ::
   Symbolic.EquationSystem Symbolic.Ignore
      Record.Delta Node s SumProduct.Term
given =
   RecIdx.before XIdx.dTime =<>

   RecIdx.before (XIdx.power Source Sink) =<>
   RecIdx.before (XIdx.eta Source Sink) =<>

   RecIdx.after (XIdx.eta Source Sink) =<>

   RecIdx.delta (XIdx.power Source Sink) =<>

   mempty

sys =
   EqSys.variableRecord (XIdx.power Sink Source) =%=
      EqSys.variableRecord (XIdx.eta Source Sink) ~*
      EqSys.variableRecord (XIdx.power Source Sink)

run ::
   (forall s.
    Symbolic.EquationSystem Symbolic.Ignore
       Record.Delta Node s SumProduct.Term) ->
   IO ()
run x =
   putStrLn $ Format.unUnicode $ Format.lines $
   AssignMap.format $ FlowTopo.toAssignMap $
   EqSys.solve (quantityTopology LinearOne.topology) x


main :: IO ()
main = do
   IO.hSetEncoding IO.stdout IO.utf8

   run given
   run (given <> sys)
   run (sys <> given)
