
module EFA.CustomTest.Given where


import qualified Data.Map as M


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

import EFA.Equation.Record (Absolute(..))

import EFA.Equation.Environment (Signal(..), Complete(..))
import EFA.Equation.Result (Result(..))

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

{-
-- Hilfsfunktion, um das testGiven-Gleichungssystem zu bauen.
-- Man übergibt ein gelöstes Env.
-- Es muessen noch die richtigen Werte eingetragen werden.

toTestGiven ::
  Complete Node.Int
    (Absolute (Result Double)) 
    (Absolute (Result Double)) -> 
  String
toTestGiven (Complete scal sig) =
  "testGiven :: EqGen.EquationSystem Node s Double Double\n" ++
  "testGiven = mconcat $\n" ++
  concat [
    g $ powerMap sig,
    g $ energyMap sig,
    g $ etaMap sig,
    g $ dtimeMap sig,
    g $ xMap sig,
    g $ sumMap sig,
    g $ maxEnergyMap scal,
    g $ storageMap scal,
    g $ stEnergyMap scal,
    g $ stXMap scal,
    g $ stSumMap scal,
    "  []" ]
  where g :: (Show k, Show a) => M.Map k (Absolute (Result a)) -> String
        g = M.foldWithKey f ""
        f idx v acc = "  ((" ++ show idx ++ ") .= " ++ showValue v ++ ") :\n" ++ acc
        showValue (Absolute (Determined x)) = show x
        showValue (Absolute Undetermined) = "?"
-}


-- Nicht alle Werte sind von originalGiven berechenbar.
testEnv ::
  Complete Node.Int
    (Absolute (Result Double)) 
    (Absolute (Result Double))
testEnv = Complete scal sigNew
  where Complete scal sig = EqGen.solveWithoutTopology testGiven
        sigNew = sig {
          energyMap = 
            M.insert (InSection sec1
                     (Energy (StructureEdge node1 node2)))
                     (Absolute Undetermined) (energyMap sig),
          powerMap =
            M.insert (InSection sec1
                     (Power (StructureEdge node1 node2)))
                     (Absolute Undetermined) (powerMap sig),
          etaMap =
            M.insert (InSection sec1
                     (Eta (StructureEdge node2 node1)))
                     (Absolute Undetermined) (etaMap sig),
          sumMap =
            M.insert (InSection sec1 (Sum In node1))
                     (Absolute Undetermined) (sumMap sig) }



originalGiven :: EqGen.EquationSystem Node.Int s Double Double
originalGiven =
   mconcat $

   (XIdx.dTime sec0 .= 1) :
   (XIdx.dTime sec1 .= 2) :
   (XIdx.dTime sec2 .= 1) :

   (XIdx.storage (Idx.afterSection sec2) node3 .= 10.0) :
   (XIdx.power sec0 node2 node3 .= 4.0) :

   (XIdx.x sec0 node2 node3 .= 0.32) :

   (XIdx.power sec1 node3 node2 .= 5) :
   (XIdx.power sec2 node3 node2 .= 6) :
   (XIdx.power sec3 node3 node2 .= 7) :
   (XIdx.power sec4 node3 node2 .= 8) :

   (XIdx.eta sec0 node3 node2 .= 0.25) :
   (XIdx.eta sec0 node2 node1 .= 0.5) :
   (XIdx.eta sec0 node0 node2 .= 0.75) :

   (XIdx.eta sec1 node0 node2 .= 0.4) :
   (XIdx.eta sec1 node2 node3 .= 0.6) :

   (XIdx.eta sec2 node0 node2 .= 0.7) :
   (XIdx.eta sec2 node3 node2 .= 0.9) :
   (XIdx.eta sec2 node2 node1 .= 1.0) :

   (XIdx.x sec1 node2 node3 .= 0.4) :
   (XIdx.x sec2 node2 node0 .= 0.3) :

   []





testGiven :: EqGen.EquationSystem Node.Int s Double Double
testGiven = mconcat $
  ((InSection sec0 (Power (StructureEdge node0 node2))) .= 11.333333333333334) :
  ((InSection sec0 (Power (StructureEdge node1 node2))) .= 6.25) :
  ((InSection sec0 (Power (StructureEdge node2 node0))) .= 8.5) :
  ((InSection sec0 (Power (StructureEdge node2 node1))) .= 12.5) :
  ((InSection sec0 (Power (StructureEdge node2 node3))) .= 4.0) :
  ((InSection sec0 (Power (StructureEdge node3 node2))) .= 16.0) :
  ((InSection sec1 (Power (StructureEdge node0 node2))) .= 52.08333333333333) :

--  ((InSection sec1 (Power (StructureEdge node1 node2))) .= ?) :

  ((InSection sec1 (Power (StructureEdge node2 node0))) .= 20.833333333333332) :
  ((InSection sec1 (Power (StructureEdge node2 node1))) .= 12.499999999999998) :
  ((InSection sec1 (Power (StructureEdge node2 node3))) .= 8.333333333333334) :
  ((InSection sec1 (Power (StructureEdge node3 node2))) .= 5.0) :
  ((InSection sec2 (Power (StructureEdge node0 node2))) .= 3.3061224489795924) :
  ((InSection sec2 (Power (StructureEdge node1 node2))) .= 7.714285714285715) :
  ((InSection sec2 (Power (StructureEdge node2 node0))) .= 2.3142857142857145) :
  ((InSection sec2 (Power (StructureEdge node2 node1))) .= 7.714285714285715) :
  ((InSection sec2 (Power (StructureEdge node2 node3))) .= 5.4) :
  ((InSection sec2 (Power (StructureEdge node3 node2))) .= 6.0) :
  ((InSection (Section 3) (Power (StructureEdge node3 node2))) .= 7.0) :
  ((InSection (Section 4) (Power (StructureEdge node3 node2))) .= 8.0) :
  ((InSection sec0 (Energy (StructureEdge node0 node2))) .= 11.333333333333334) :
  ((InSection sec0 (Energy (StructureEdge node1 node2))) .= 6.25) :
  ((InSection sec0 (Energy (StructureEdge node2 node0))) .= 8.5) :
  ((InSection sec0 (Energy (StructureEdge node2 node1))) .= 12.5) :
  ((InSection sec0 (Energy (StructureEdge node2 node3))) .= 4.0) :
  ((InSection sec0 (Energy (StructureEdge node3 node2))) .= 16.0) :
  ((InSection sec1 (Energy (StructureEdge node0 node2))) .= 104.16666666666666) :

--  ((InSection sec1 (Energy (StructureEdge node1 node2))) .= ?) :

  ((InSection sec1 (Energy (StructureEdge node2 node0))) .= 41.666666666666664) :
  ((InSection sec1 (Energy (StructureEdge node2 node1))) .= 24.999999999999996) :
  ((InSection sec1 (Energy (StructureEdge node2 node3))) .= 16.666666666666668) :
  ((InSection sec1 (Energy (StructureEdge node3 node2))) .= 10.0) :
  ((InSection sec2 (Energy (StructureEdge node0 node2))) .= 3.3061224489795924) :
  ((InSection sec2 (Energy (StructureEdge node1 node2))) .= 7.714285714285715) :
  ((InSection sec2 (Energy (StructureEdge node2 node0))) .= 2.3142857142857145) :
  ((InSection sec2 (Energy (StructureEdge node2 node1))) .= 7.714285714285715) :
  ((InSection sec2 (Energy (StructureEdge node2 node3))) .= 5.4) :
  ((InSection sec2 (Energy (StructureEdge node3 node2))) .= 6.0) :
  ((InSection sec0 (Eta (StructureEdge node0 node2))) .= 0.75) :
  ((InSection sec0 (Eta (StructureEdge node2 node1))) .= 0.5) :
  ((InSection sec0 (Eta (StructureEdge node3 node2))) .= 0.25) :
  ((InSection sec1 (Eta (StructureEdge node0 node2))) .= 0.4) :

--  ((InSection sec1 (Eta (StructureEdge node2 node1))) .= ?) :

  ((InSection sec1 (Eta (StructureEdge node2 node3))) .= 0.6) :
  ((InSection sec2 (Eta (StructureEdge node0 node2))) .= 0.7) :
  ((InSection sec2 (Eta (StructureEdge node2 node1))) .= 1.0) :
  ((InSection sec2 (Eta (StructureEdge node3 node2))) .= 0.9) :
  ((InSection sec0 DTime) .= 1.0) :
  ((InSection sec1 DTime) .= 2.0) :
  ((InSection sec2 DTime) .= 1.0) :
  ((InSection sec0 (X (StructureEdge node0 node2))) .= 1.0) :
  ((InSection sec0 (X (StructureEdge node1 node2))) .= 1.0) :
  ((InSection sec0 (X (StructureEdge node2 node0))) .= 0.68) :
  ((InSection sec0 (X (StructureEdge node2 node1))) .= 1.0) :
  ((InSection sec0 (X (StructureEdge node2 node3))) .= 0.32) :
  ((InSection sec0 (X (StructureEdge node3 node2))) .= 1.0) :
  ((InSection sec1 (X (StructureEdge node0 node2))) .= 1.0) :
  ((InSection sec1 (X (StructureEdge node1 node2))) .= 1.0) :
  ((InSection sec1 (X (StructureEdge node2 node0))) .= 1.0) :
  ((InSection sec1 (X (StructureEdge node2 node1))) .= 0.6) :
  ((InSection sec1 (X (StructureEdge node2 node3))) .= 0.4000000000000001) :
  ((InSection sec1 (X (StructureEdge node3 node2))) .= 1.0) :
  ((InSection sec2 (X (StructureEdge node0 node2))) .= 1.0) :
  ((InSection sec2 (X (StructureEdge node1 node2))) .= 1.0) :
  ((InSection sec2 (X (StructureEdge node2 node0))) .= 0.3) :
  ((InSection sec2 (X (StructureEdge node2 node1))) .= 1.0) :
  ((InSection sec2 (X (StructureEdge node2 node3))) .= 0.7) :
  ((InSection sec2 (X (StructureEdge node3 node2))) .= 1.0) :
  ((InSection sec0 (Sum In node1)) .= 6.25) :
  ((InSection sec0 (Sum In node2)) .= 12.5) :
  ((InSection sec0 (Sum Out node0)) .= 11.333333333333334) :
  ((InSection sec0 (Sum Out node2)) .= 12.5) :
  ((InSection sec0 (Sum Out node3)) .= 16.0) :

--  ((InSection sec1 (Sum In node1)) .= ?) :

  ((InSection sec1 (Sum In node2)) .= 41.666666666666664) :
  ((InSection sec1 (Sum In node3)) .= 10.0) :
  ((InSection sec1 (Sum Out node0)) .= 104.16666666666666) :
  ((InSection sec1 (Sum Out node2)) .= 41.666666666666664) :
  ((InSection sec2 (Sum In node1)) .= 7.714285714285715) :
  ((InSection sec2 (Sum In node2)) .= 7.714285714285715) :
  ((InSection sec2 (Sum Out node0)) .= 3.3061224489795924) :
  ((InSection sec2 (Sum Out node2)) .= 7.714285714285715) :
  ((InSection sec2 (Sum Out node3)) .= 6.0) :

  ((ForNode (MaxEnergy (StorageEdge Initial (AfterSection sec0))) node3) .= 22.0) :
  ((ForNode (MaxEnergy (StorageEdge Initial (AfterSection sec2))) node3) .= 6.0) :
  ((ForNode (MaxEnergy (StorageEdge (AfterSection sec1) (AfterSection sec2))) node3) .= 10.0) :

  ((ForNode (Storage Initial) node3) .= 22.0) :
  ((ForNode (Storage (AfterSection sec0)) node3) .= 6.0) :
  ((ForNode (Storage (AfterSection sec1)) node3) .= 16.0) :
  ((ForNode (Storage (AfterSection sec2)) node3) .= 10.0) :

  ((ForNode (StEnergy (StorageEdge Initial (AfterSection sec0))) node3) .= 16.0) :
  ((ForNode (StEnergy (StorageEdge Initial (AfterSection sec2))) node3) .= 2.25) :
  ((ForNode (StEnergy (StorageEdge (AfterSection sec0) Initial)) node3) .= 16.0) :
  ((ForNode (StEnergy (StorageEdge (AfterSection sec1) (AfterSection sec2))) node3) .= 3.75) :
  ((ForNode (StEnergy (StorageEdge (AfterSection sec2) Initial)) node3) .= 2.25) :
  ((ForNode (StEnergy (StorageEdge (AfterSection sec2) (AfterSection sec1))) node3) .= 3.75) :

  ((ForNode (StX (StorageEdge Initial (AfterSection sec0))) node3) .= 0.8767123287671232) :
  ((ForNode (StX (StorageEdge Initial (AfterSection sec2))) node3) .= 0.1232876712328767) :
  ((ForNode (StX (StorageEdge (AfterSection sec0) Initial)) node3) .= 1.0) :
  ((ForNode (StX (StorageEdge (AfterSection sec1) (AfterSection sec2))) node3) .= 1.0) :
  ((ForNode (StX (StorageEdge (AfterSection sec2) Initial)) node3) .= 0.375) :
  ((ForNode (StX (StorageEdge (AfterSection sec2) (AfterSection sec1))) node3) .= 0.625) :

  ((ForNode (StSum In Initial) node3) .= 22.0) :
  ((ForNode (StSum In (AfterSection sec1)) node3) .= 10.0) :
  ((ForNode (StSum Out (AfterSection sec0)) node3) .= 16.0) :
  ((ForNode (StSum Out (AfterSection sec2)) node3) .= 6.0) :
  []
