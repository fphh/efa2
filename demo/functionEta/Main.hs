-- | Demonstriert, wie man ein eta als Funktion definiert.
module Main where

import qualified EFA.Equation.Environment as Env
import qualified EFA.Example.Absolute as EqGen
import EFA.Example.Absolute ((.=))
import EFA.Equation.System ((=.=))
import EFA.Example.Utility
  (constructSeqTopo, edgeVar, makeEdges)

import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility (checkedLookup)
import EFA.Graph (mkGraph)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import Data.Monoid (mconcat, (<>))


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0

data Node = Sink | Source deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


linearOne :: TD.Topology Node
linearOne = mkGraph nodes (makeEdges edges)
  where nodes = [(Sink, TD.AlwaysSink), (Source, TD.AlwaysSource)]
        edges = [(Source, Sink)]

seqTopo :: Flow.RangeGraph Node
seqTopo = constructSeqTopo linearOne [0]

enRange :: [Double]
enRange = 0.01:[0.5, 1 .. 9]


type Expr s a = EqGen.Expression Node s Double Double a

c :: Idx.Power Node
c = edgeVar Idx.Power sec0 Source Sink

eta :: Idx.Eta Node
eta = edgeVar Idx.Eta sec0 Source Sink


functionEta :: Expr s Double -> Expr s Double
functionEta = EqGen.liftF $ \p -> 0.3 * sqrt p

given :: Double -> EqGen.EquationSystem Node s Double Double
given p =
   mconcat $
   (Idx.DTime sec0 .= 1) :
   (edgeVar Idx.Power sec0 Source Sink .= p) :
   []


solve :: Double -> String
solve p =
  let env =
         EqGen.solve seqTopo
            ((EqGen.variable eta =.= functionEta (EqGen.variable c)) <> given p)
  in  show p ++ " " ++
      Format.unUnicode (formatValue
         (checkedLookup (Env.etaMap (Env.signal env)) eta))

main :: IO ()
main =
  putStrLn $ unlines $ map solve enRange
