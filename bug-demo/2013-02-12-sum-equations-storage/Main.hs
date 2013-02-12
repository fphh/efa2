
-- | Woher bekommt e10 in der Sektion 1 seinen Wert?
-- Änderungen in Equation.System.makeNodeEquations
--
-- Jetzt
--
-- mwhen (label /= TD.Storage) (varsumin =.= varsumout)
-- 
-- statt:
--
-- (varsumin =.= varsumout) -- <- buggy

module Main where

import Data.Monoid ((<>))

import EFA.Example.Utility
  ( edgeVar, makeEdges, constructSeqTopo )
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Equation.System as EqGen
import EFA.Equation.System ((=.=))
import qualified EFA.Graph as Gr

sec0, sec1 :: Idx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ Idx.Section 0


data Nodes = N0 | N1 deriving (Show, Eq, Ord)

instance Node.Show Nodes where
         show = Prelude.show

topoDreibein :: TD.Topology Nodes
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [ (N0, TD.Storage),
               (N1, TD.Storage) ]
        es = [(N0, N1)]

seqTopo :: TD.SequFlowGraph Nodes
seqTopo = constructSeqTopo topoDreibein [0, 1]

p10 :: Idx.Section -> EqGen.ExprWithVars Nodes s Double
p10 sec = edgeVar EqGen.power sec N1 N0

stoinit :: EqGen.ExprWithVars Nodes s Double
stoinit = EqGen.storage (Idx.SecNode Idx.initSection N1)

t0, t1, tinit :: EqGen.ExprWithVars nty s Double
tinit = EqGen.dtime Idx.initSection
t0 = EqGen.dtime  sec0
t1 = EqGen.dtime sec1

given :: EqGen.EquationSystem Nodes s Double
given =
  (tinit =.= 1)
  <> (t0 =.= 1)
  <> (t1 =.= 1)
  <> (stoinit =.= 5) -- kann man auch mal auskommentieren -> interessanter effekt
  <> (p10 sec0 =.= 3)

main :: IO ()
main =
  let env = EqGen.solve given seqTopo
  in  Draw.sequFlowGraphAbsWithEnv seqTopo env
