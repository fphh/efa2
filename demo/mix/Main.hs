module Main where

import EFA.Application.Utility ( topologyFromEdges )

import qualified EFA.Flow.Topology.EquationSystem as EqSys
import qualified EFA.Flow.Topology.AssignMap as AssignMap
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Topology.EquationSystem ((.=))

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import EFA.Equation.Result(Result)

import qualified EFA.Report.Format as Format

import qualified EFA.Utility.FixedLength as FL

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty

import Data.Monoid (Monoid, mconcat)


data Node = Source0 | Source1 | Crossing | Sink0 | Sink1
   deriving (Show, Eq, Ord, Enum)

source0, source1, crossing, sink0, sink1 :: Node
source0  = Source0
source1  = Source1
sink0    = Sink0
sink1    = Sink1
crossing = Crossing

instance Node.C Node where
   display Source0  = Format.literal "Quelle0"
   display Source1  = Format.literal "Quelle1"
   display Sink0    = Format.literal "Senke0"
   display Sink1    = Format.literal "Senke1"
   display Crossing = Format.literal "Kreuzung"

   subscript = Format.integer . fromIntegral . fromEnum
   dotId = Node.dotIdDefault

   typ Source0  = Node.Source
   typ Source1  = Node.Source
   typ Sink0    = Node.Sink
   typ Sink1    = Node.Sink
   typ Crossing = Node.Crossing


topology :: Topo.Topology Node
topology =
   topologyFromEdges
      [(source0, crossing), (source1, crossing),
       (crossing, sink0), (crossing, sink1)]


type Mix = Record.Mix (NonEmpty.T Empty.T)

sourceMixSystem ::
   EqSys.EquationSystem Verify.Ignore Mix Node s Double
sourceMixSystem =
   mconcat $

   (RecIdx.mixSum XIdx.dTime .= 0.5) :
   (RecIdx.mixComponent FL.i0 (XIdx.power source0 crossing) .= 4) :
   (RecIdx.mixComponent FL.i1 (XIdx.power source0 crossing) .= 0) :
   (RecIdx.mixComponent FL.i0 (XIdx.power source1 crossing) .= 0) :
   (RecIdx.mixComponent FL.i1 (XIdx.power source1 crossing) .= 3) :

   (RecIdx.mixSum (XIdx.power crossing sink0) .= 0.9) :

   (RecIdx.mixSum (XIdx.eta source0 crossing) .= 0.25) :
   (RecIdx.mixSum (XIdx.eta source1 crossing) .= 0.5) :
   (RecIdx.mixSum (XIdx.eta crossing sink0) .= 0.75) :
   (RecIdx.mixSum (XIdx.eta crossing sink1) .= 0.8) :

   []

sourceMixSolution :: FlowTopo.Section Node (Mix (Result Double))
sourceMixSolution =
   EqSys.solveOpts EqSys.optionsSourceMix
      (FlowTopo.sectionFromPlain $ Topo.flowFromPlain topology)
      sourceMixSystem


sinkMixSystem ::
   EqSys.EquationSystem Verify.Ignore Mix Node s Double
sinkMixSystem =
   mconcat $

   (RecIdx.mixSum XIdx.dTime .= 0.5) :
   (RecIdx.mixComponent FL.i0 (XIdx.power source0 crossing) .= 4) :
   (RecIdx.mixComponent FL.i1 (XIdx.power source0 crossing) .= 3) :
   (RecIdx.mixSum (XIdx.power source1 crossing) .= 5) :

   (RecIdx.mixComponent FL.i1 (XIdx.power crossing sink0) .= 0) :
   (RecIdx.mixComponent FL.i0 (XIdx.power crossing sink1) .= 0) :

   (RecIdx.mixSum (XIdx.eta source0 crossing) .= 0.25) :
   (RecIdx.mixSum (XIdx.eta source1 crossing) .= 0.5) :
   (RecIdx.mixSum (XIdx.eta crossing sink0) .= 0.75) :
   (RecIdx.mixSum (XIdx.eta crossing sink1) .= 0.8) :
   []

sinkMixSolution :: FlowTopo.Section Node (Mix (Result Double))
sinkMixSolution =
   EqSys.solveOpts EqSys.optionsSinkMix
      (FlowTopo.sectionFromPlain $ Topo.flowFromPlain topology)
      sinkMixSystem


main :: IO ()
main = do
   mapM_ (putStrLn . Format.unUnicode) $
      AssignMap.format $ FlowTopo.toAssignMap sourceMixSolution
   Draw.xterm $ Draw.flowTopology Draw.optionsDefault $
      FlowTopo.topology sourceMixSolution

   Draw.xterm $ Draw.flowTopology Draw.optionsDefault $
      FlowTopo.topology sinkMixSolution
