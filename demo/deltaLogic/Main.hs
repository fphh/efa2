module Main where

import qualified EFA.Application.Index as XIdx
import EFA.Application.Utility (SymbolicEquationSystem, Ignore, (=<>))

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Record as Record
import EFA.Equation.System ((=%=))
import EFA.Equation.Arithmetic ((~*))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified System.IO as IO
import Data.Monoid (mempty, (<>))


sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1 :: Node.Int
node0 :~ node1 :~ _ = Stream.enumFrom minBound


{-
Use SumProduct.Term here since it simplifies automatically.
-}
given, sys ::
   SymbolicEquationSystem Ignore Record.Delta Node.Int s SumProduct.Term
given =
   Idx.before (XIdx.power sec0 node0 node1) =<>
   Idx.before (XIdx.eta sec0 node0 node1) =<>

   Idx.after (XIdx.eta sec0 node0 node1) =<>

   Idx.delta (XIdx.power sec0 node0 node1) =<>

   mempty

sys =
   (EqGen.variableRecord (XIdx.power sec0 node1 node0) =%=
      EqGen.variableRecord (XIdx.eta sec0 node0 node1) ~*
      EqGen.variableRecord (XIdx.power sec0 node0 node1))


main :: IO ()
main = do
   IO.hSetEncoding IO.stdout IO.utf8
   putStr $ Format.unUnicode $ formatValue $ EqGen.solveSimple given
   putStrLn ""
   putStr $ Format.unUnicode $ formatValue $ EqGen.solveSimple (given <> sys)
   putStrLn ""
   putStr $ Format.unUnicode $ formatValue $ EqGen.solveSimple (sys <> given)
