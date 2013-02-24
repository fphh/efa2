module Main where

import EFA.Example.Utility (Term, edgeVar, (=<>))

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Env as Env
import EFA.Equation.System ((=%=))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import Data.Monoid (mempty, (<>))


sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1 :: Node.Int
node0 :~ node1 :~ _ = Stream.enumFrom $ Node.Int 0


{-
Use SumProduct.Term here since it simplifies automatically.
-}
given, sys ::
   EqGen.EquationSystem Env.Delta Node.Int s
      (Term SumProduct.Term Idx.Delta Node.Int)
given =
   Idx.before (edgeVar Idx.Power sec0 node0 node1) =<>
   Idx.before (edgeVar Idx.Eta sec0 node0 node1) =<>

   Idx.after (edgeVar Idx.Eta sec0 node0 node1) =<>

   Idx.delta (edgeVar Idx.Power sec0 node0 node1) =<>

   mempty

sys =
   (edgeVar EqGen.power sec0 node1 node0 =%=
      edgeVar EqGen.eta sec0 node0 node1 *
      edgeVar EqGen.power sec0 node0 node1)


main :: IO ()
main = do
   putStr $ Format.unUnicode $ formatValue $ EqGen.solveSimple given
   putStrLn ""
   putStr $ Format.unUnicode $ formatValue $ EqGen.solveSimple (given <> sys)
   putStrLn ""
   putStr $ Format.unUnicode $ formatValue $ EqGen.solveSimple (sys <> given)
