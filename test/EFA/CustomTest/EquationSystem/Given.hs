
module EFA.CustomTest.EquationSystem.Given where


import EFA.Example.Absolute ( (.=) )
import qualified EFA.Example.Absolute as EqGen

import EFA.Graph.Topology.Index
  ( Section(..), InSection(..), Power(..), ForNode(..),
    Boundary(..), StorageEdge(..), Storage(..), StX(..),
    Direction(..), Sum(..), StructureEdge(..), X(..),
    Energy(..), Eta(..), MaxEnergy(..), StEnergy(..),
    DTime(..), StSum(..) )

import EFA.Utility.Stream (Stream((:~)))
import qualified EFA.Utility.Stream as Stream
import qualified EFA.Example.Index as XIdx
import qualified EFA.Graph.Topology.Index as Idx

import EFA.Example.Utility ( makeEdges, constructSeqTopo )


import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import EFA.Equation.Result (Result(..))

import qualified Data.Map as M

import Data.Ratio ((%))
import Data.Monoid (mconcat)


sec0, sec1, sec2, sec3, sec4 :: Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Section 0

node0, node1, node2, node3 :: Node.Int
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom minBound

topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.fromList ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.storage)]
        es = [(node0, node2), (node1, node2), (node2, node3)]


seqTopo :: Flow.RangeGraph Node.Int
seqTopo = constructSeqTopo topoDreibein [1, 0, 1]


-- Hilfsfunktion, um das testGiven-Gleichungssystem zu bauen.
-- Man übergibt ein gelöstes Env.
-- Es muessen noch die richtigen Werte eingetragen werden.

toTestGiven ::
  Env.Complete Node.Int
    (Record.Absolute (Result Rational))
    (Record.Absolute (Result Rational)) ->
  String
toTestGiven (Env.Complete scal sig) =
  "testGiven :: EqGen.EquationSystem Node s Rational Rational\n" ++
  "testGiven = mconcat $\n" ++
  concat [
    g $ Env.powerMap sig,
    g $ Env.energyMap sig,
    g $ Env.etaMap sig,
    g $ Env.dtimeMap sig,
    g $ Env.xMap sig,
    g $ Env.sumMap sig,
    g $ Env.maxEnergyMap scal,
    g $ Env.storageMap scal,
    g $ Env.stEnergyMap scal,
    g $ Env.stXMap scal,
    g $ Env.stSumMap scal,
    "  []" ]
  where g :: (Show k, Show a) => M.Map k (Record.Absolute (Result a)) -> String
        g = M.foldWithKey f ""
        f idx v acc = "  ((" ++ show idx ++ ") .= "
                        ++ showValue v ++ ") :\n" ++ acc
        showValue (Record.Absolute (Determined x)) = show x
        showValue (Record.Absolute Undetermined) = "?"


-- Nicht alle Werte sind von originalGiven berechenbar.
testEnv ::
  Env.Complete Node.Int
    (Record.Absolute (Result Rational))
    (Record.Absolute (Result Rational))
testEnv = Env.Complete scal sigNew
  where Env.Complete scal sig = EqGen.solveWithoutTopology testGiven
        sigNew = sig {
          Env.energyMap =
            M.insert (InSection sec1
                     (Energy (StructureEdge node1 node2)))
                     (Record.Absolute Undetermined) (Env.energyMap sig),
          Env.powerMap =
            M.insert (InSection sec1
                     (Power (StructureEdge node1 node2)))
                     (Record.Absolute Undetermined) (Env.powerMap sig),
          Env.etaMap =
            M.insert (InSection sec1
                     (Eta (StructureEdge node2 node1)))
                     (Record.Absolute Undetermined) (Env.etaMap sig),
          Env.sumMap =
            M.insert (InSection sec1 (Sum In node1))
                     (Record.Absolute Undetermined) (Env.sumMap sig) }


originalGiven :: EqGen.EquationSystem Node.Int s Rational Rational
originalGiven =
   mconcat $

   (XIdx.dTime sec0 .= 1 % 1) :
   (XIdx.dTime sec1 .= 2 % 1) :
   (XIdx.dTime sec2 .= 1 % 1) :

   (XIdx.storage (Idx.afterSection sec2) node3 .= 10 % 1) :

   (XIdx.x sec0 node2 node3 .= 8 % 25) :

   (XIdx.power sec0 node2 node3 .= 4 % 1) :
   (XIdx.power sec1 node3 node2 .= 5 % 1) :
   (XIdx.power sec2 node3 node2 .= 6 % 1) :

   (XIdx.eta sec0 node3 node2 .= 1 % 4) :
   (XIdx.eta sec0 node2 node1 .= 1 % 2) :
   (XIdx.eta sec0 node0 node2 .= 3 % 4) :

   (XIdx.eta sec1 node0 node2 .= 2 % 5) :
   (XIdx.eta sec1 node2 node3 .= 3 % 5) :

   (XIdx.eta sec2 node0 node2 .= 7 % 10) :
   (XIdx.eta sec2 node3 node2 .= 9 % 10) :
   (XIdx.eta sec2 node2 node1 .= 1 % 1) :

   (XIdx.x sec1 node2 node3 .= 2 % 5) :

   (XIdx.x sec2 node2 node0 .= 3 % 10) :

   []




testGiven :: EqGen.EquationSystem Node.Int s Rational Rational
testGiven = mconcat $
  ((InSection (Section 0) (Power (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 34 % 3) :
  ((InSection (Section 0) (Power (StructureEdge (Node.Int 1) (Node.Int 2)))) .= 25 % 4) :
  ((InSection (Section 0) (Power (StructureEdge (Node.Int 2) (Node.Int 0)))) .= 17 % 2) :
  ((InSection (Section 0) (Power (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 25 % 2) :
  ((InSection (Section 0) (Power (StructureEdge (Node.Int 2) (Node.Int 3)))) .= 4 % 1) :
  ((InSection (Section 0) (Power (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 16 % 1) :
  ((InSection (Section 1) (Power (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 625 % 12) :

--  ((InSection (Section 1) (Power (StructureEdge (Node.Int 1) (Node.Int 2)))) .= ?) :

  ((InSection (Section 1) (Power (StructureEdge (Node.Int 2) (Node.Int 0)))) .= 125 % 6) :
  ((InSection (Section 1) (Power (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 25 % 2) :
  ((InSection (Section 1) (Power (StructureEdge (Node.Int 2) (Node.Int 3)))) .= 25 % 3) :
  ((InSection (Section 1) (Power (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 5 % 1) :
  ((InSection (Section 2) (Power (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 81 % 50) :
  ((InSection (Section 2) (Power (StructureEdge (Node.Int 1) (Node.Int 2)))) .= 54 % 7) :
  ((InSection (Section 2) (Power (StructureEdge (Node.Int 2) (Node.Int 0)))) .= 81 % 35) :
  ((InSection (Section 2) (Power (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 54 % 7) :
  ((InSection (Section 2) (Power (StructureEdge (Node.Int 2) (Node.Int 3)))) .= 27 % 5) :
  ((InSection (Section 2) (Power (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 6 % 1) :
  ((InSection (Section 0) (Energy (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 34 % 3) :
  ((InSection (Section 0) (Energy (StructureEdge (Node.Int 1) (Node.Int 2)))) .= 25 % 4) :
  ((InSection (Section 0) (Energy (StructureEdge (Node.Int 2) (Node.Int 0)))) .= 17 % 2) :
  ((InSection (Section 0) (Energy (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 25 % 2) :
  ((InSection (Section 0) (Energy (StructureEdge (Node.Int 2) (Node.Int 3)))) .= 4 % 1) :
  ((InSection (Section 0) (Energy (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 16 % 1) :
  ((InSection (Section 1) (Energy (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 625 % 6) :

--  ((InSection (Section 1) (Energy (StructureEdge (Node.Int 1) (Node.Int 2)))) .= ?) :

  ((InSection (Section 1) (Energy (StructureEdge (Node.Int 2) (Node.Int 0)))) .= 125 % 3) :
  ((InSection (Section 1) (Energy (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 25 % 1) :
  ((InSection (Section 1) (Energy (StructureEdge (Node.Int 2) (Node.Int 3)))) .= 50 % 3) :
  ((InSection (Section 1) (Energy (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 10 % 1) :
  ((InSection (Section 2) (Energy (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 81 % 50) :
  ((InSection (Section 2) (Energy (StructureEdge (Node.Int 1) (Node.Int 2)))) .= 54 % 7) :
  ((InSection (Section 2) (Energy (StructureEdge (Node.Int 2) (Node.Int 0)))) .= 81 % 35) :
  ((InSection (Section 2) (Energy (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 54 % 7) :
  ((InSection (Section 2) (Energy (StructureEdge (Node.Int 2) (Node.Int 3)))) .= 27 % 5) :
  ((InSection (Section 2) (Energy (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 6 % 1) :
  ((InSection (Section 0) (Eta (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 3 % 4) :
  ((InSection (Section 0) (Eta (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 1 % 2) :
  ((InSection (Section 0) (Eta (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 1 % 4) :
  ((InSection (Section 1) (Eta (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 2 % 5) :

--  ((InSection (Section 1) (Eta (StructureEdge (Node.Int 2) (Node.Int 1)))) .= ?) :

  ((InSection (Section 1) (Eta (StructureEdge (Node.Int 2) (Node.Int 3)))) .= 3 % 5) :
  ((InSection (Section 2) (Eta (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 7 % 10) :
  ((InSection (Section 2) (Eta (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 1 % 1) :
  ((InSection (Section 2) (Eta (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 9 % 10) :
  ((InSection (Section 0) DTime) .= 1 % 1) :
  ((InSection (Section 1) DTime) .= 2 % 1) :
  ((InSection (Section 2) DTime) .= 1 % 1) :
  ((InSection (Section 0) (X (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 1 % 1) :
  ((InSection (Section 0) (X (StructureEdge (Node.Int 1) (Node.Int 2)))) .= 1 % 1) :
  ((InSection (Section 0) (X (StructureEdge (Node.Int 2) (Node.Int 0)))) .= 17 % 25) :
  ((InSection (Section 0) (X (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 1 % 1) :
  ((InSection (Section 0) (X (StructureEdge (Node.Int 2) (Node.Int 3)))) .= 8 % 25) :
  ((InSection (Section 0) (X (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 1 % 1) :
  ((InSection (Section 1) (X (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 1 % 1) :
  ((InSection (Section 1) (X (StructureEdge (Node.Int 1) (Node.Int 2)))) .= 1 % 1) :
  ((InSection (Section 1) (X (StructureEdge (Node.Int 2) (Node.Int 0)))) .= 1 % 1) :
  ((InSection (Section 1) (X (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 3 % 5) :
  ((InSection (Section 1) (X (StructureEdge (Node.Int 2) (Node.Int 3)))) .= 2 % 5) :
  ((InSection (Section 1) (X (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 1 % 1) :
  ((InSection (Section 2) (X (StructureEdge (Node.Int 0) (Node.Int 2)))) .= 1 % 1) :
  ((InSection (Section 2) (X (StructureEdge (Node.Int 1) (Node.Int 2)))) .= 1 % 1) :
  ((InSection (Section 2) (X (StructureEdge (Node.Int 2) (Node.Int 0)))) .= 3 % 10) :
  ((InSection (Section 2) (X (StructureEdge (Node.Int 2) (Node.Int 1)))) .= 1 % 1) :
  ((InSection (Section 2) (X (StructureEdge (Node.Int 2) (Node.Int 3)))) .= 7 % 10) :
  ((InSection (Section 2) (X (StructureEdge (Node.Int 3) (Node.Int 2)))) .= 1 % 1) :
  ((InSection (Section 0) (Sum In (Node.Int 1))) .= 25 % 4) :
  ((InSection (Section 0) (Sum In (Node.Int 2))) .= 25 % 2) :
  ((InSection (Section 0) (Sum Out (Node.Int 0))) .= 34 % 3) :
  ((InSection (Section 0) (Sum Out (Node.Int 2))) .= 25 % 2) :
  ((InSection (Section 0) (Sum Out (Node.Int 3))) .= 16 % 1) :

--  ((InSection (Section 1) (Sum In (Node.Int 1))) .= ?) :

  ((InSection (Section 1) (Sum In (Node.Int 2))) .= 125 % 3) :
  ((InSection (Section 1) (Sum In (Node.Int 3))) .= 10 % 1) :
  ((InSection (Section 1) (Sum Out (Node.Int 0))) .= 625 % 6) :
  ((InSection (Section 1) (Sum Out (Node.Int 2))) .= 125 % 3) :
  ((InSection (Section 2) (Sum In (Node.Int 1))) .= 54 % 7) :
  ((InSection (Section 2) (Sum In (Node.Int 2))) .= 54 % 7) :
  ((InSection (Section 2) (Sum Out (Node.Int 0))) .= 162 % 49) :
  ((InSection (Section 2) (Sum Out (Node.Int 2))) .= 54 % 7) :
  ((InSection (Section 2) (Sum Out (Node.Int 3))) .= 6 % 1) :
  ((ForNode (MaxEnergy (StorageEdge Initial (AfterSection (Section 0)))) (Node.Int 3)) .= 22 % 1) :
  ((ForNode (MaxEnergy (StorageEdge Initial (AfterSection (Section 2)))) (Node.Int 3)) .= 6 % 1) :
  ((ForNode (MaxEnergy (StorageEdge (AfterSection (Section 1)) (AfterSection (Section 2)))) (Node.Int 3)) .= 10 % 1) :
  ((ForNode (Storage Initial) (Node.Int 3)) .= 22 % 1) :
  ((ForNode (Storage (AfterSection (Section 0))) (Node.Int 3)) .= 6 % 1) :
  ((ForNode (Storage (AfterSection (Section 1))) (Node.Int 3)) .= 16 % 1) :
  ((ForNode (Storage (AfterSection (Section 2))) (Node.Int 3)) .= 10 % 1) :
  ((ForNode (StEnergy (StorageEdge Initial (AfterSection (Section 0)))) (Node.Int 3)) .= 16 % 1) :
  ((ForNode (StEnergy (StorageEdge Initial (AfterSection (Section 2)))) (Node.Int 3)) .= 9 % 4) :
  ((ForNode (StEnergy (StorageEdge (AfterSection (Section 0)) Initial)) (Node.Int 3)) .= 16 % 1) :
  ((ForNode (StEnergy (StorageEdge (AfterSection (Section 1)) (AfterSection (Section 2)))) (Node.Int 3)) .= 15 % 4) :
  ((ForNode (StEnergy (StorageEdge (AfterSection (Section 2)) Initial)) (Node.Int 3)) .= 9 % 4) :
  ((ForNode (StEnergy (StorageEdge (AfterSection (Section 2)) (AfterSection (Section 1)))) (Node.Int 3)) .= 15 % 4) :
  ((ForNode (StX (StorageEdge Initial (AfterSection (Section 0)))) (Node.Int 3)) .= 64 % 73) :
  ((ForNode (StX (StorageEdge Initial (AfterSection (Section 2)))) (Node.Int 3)) .= 9 % 73) :
  ((ForNode (StX (StorageEdge (AfterSection (Section 0)) Initial)) (Node.Int 3)) .= 1 % 1) :
  ((ForNode (StX (StorageEdge (AfterSection (Section 1)) (AfterSection (Section 2)))) (Node.Int 3)) .= 1 % 1) :
  ((ForNode (StX (StorageEdge (AfterSection (Section 2)) Initial)) (Node.Int 3)) .= 3 % 8) :
  ((ForNode (StX (StorageEdge (AfterSection (Section 2)) (AfterSection (Section 1)))) (Node.Int 3)) .= 5 % 8) :
  ((ForNode (StSum In Initial) (Node.Int 3)) .= 22 % 1) :
  ((ForNode (StSum In (AfterSection (Section 1))) (Node.Int 3)) .= 10 % 1) :
  ((ForNode (StSum Out (AfterSection (Section 0))) (Node.Int 3)) .= 16 % 1) :
  ((ForNode (StSum Out (AfterSection (Section 2))) (Node.Int 3)) .= 6 % 1) :
  []
