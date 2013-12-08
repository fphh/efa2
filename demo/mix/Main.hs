module Main where

import EFA.Application.Utility
          (topologyFromEdges, seqFlowGraphFromStates, dirEdge)

import qualified EFA.Flow.State.EquationSystem as StateEqSys
import qualified EFA.Flow.State.Quantity as StateFlow

import qualified EFA.Flow.Sequence.EquationSystem as SeqEqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as SeqIdx

import qualified EFA.Flow.Cumulated.EquationSystem as CumEqSys
import qualified EFA.Flow.Cumulated.Quantity as CumFlow

import qualified EFA.Flow.Topology.EquationSystem as EqSys
import qualified EFA.Flow.Topology.AssignMap as AssignMap
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.Part.Index as PartIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Topology.EquationSystem ((.=))

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import EFA.Equation.Result (Result)
import EFA.Equation.Unknown (unknown)
import EFA.Equation.Arithmetic ((~+))

import qualified EFA.Report.Format as Format

import qualified EFA.Utility.FixedLength as FL
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Async (concurrentlyMany_)

import Control.Applicative (liftA2, pure)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import Data.NonEmpty ((!:))
import Data.Monoid (Monoid, mconcat, mempty)


data Node = Source0 | Source1 | Crossing | Storage | Sink
   deriving (Show, Eq, Ord, Enum)

source0, source1, crossing, storage, sink :: Node
source0  = Source0
source1  = Source1
storage  = Storage
sink     = Sink
crossing = Crossing

instance Node.C Node where
   display Source0  = Format.literal "Quelle0"
   display Source1  = Format.literal "Quelle1"
   display Storage  = Format.literal "Speicher"
   display Sink     = Format.literal "Senke"
   display Crossing = Format.literal "Kreuzung"

   subscript = Format.integer . fromIntegral . fromEnum
   dotId = Node.dotIdDefault

   typ Source0  = Node.Source
   typ Source1  = Node.Source
   typ Storage  = Node.Storage
   typ Sink     = Node.Sink
   typ Crossing = Node.Crossing


topology :: Topo.Topology Node
topology =
   topologyFromEdges
      [(source0, crossing), (source1, crossing),
       (crossing, storage), (crossing, sink)]


type Mix = Record.Mix (NonEmpty.T Empty.T)

sourceMixSystem ::
   EqSys.EquationSystem Verify.Ignore Mix Node s Double
sourceMixSystem =
   mconcat $

   (RecIdx.mixTotal XIdx.dTime .= 0.5) :
   (RecIdx.mixComponent FL.i0 (XIdx.power source0 crossing) .= 4) :
   (RecIdx.mixComponent FL.i1 (XIdx.power source0 crossing) .= 0) :
   (RecIdx.mixComponent FL.i0 (XIdx.power source1 crossing) .= 0) :
   (RecIdx.mixComponent FL.i1 (XIdx.power source1 crossing) .= 3) :

   (RecIdx.mixTotal (XIdx.power crossing storage) .= 0.9) :

   (RecIdx.mixTotal (XIdx.eta source0 crossing) .= 0.25) :
   (RecIdx.mixTotal (XIdx.eta source1 crossing) .= 0.5) :
   (RecIdx.mixTotal (XIdx.eta crossing storage) .= 0.75) :
   (RecIdx.mixTotal (XIdx.eta crossing sink) .= 0.8) :

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

   (RecIdx.mixTotal XIdx.dTime .= 0.5) :
   (RecIdx.mixComponent FL.i0 (XIdx.power source0 crossing) .= 4) :
   (RecIdx.mixComponent FL.i1 (XIdx.power source0 crossing) .= 3) :
   (RecIdx.mixTotal (XIdx.power source1 crossing) .= 5) :

   (RecIdx.mixComponent FL.i1 (XIdx.power crossing storage) .= 0) :
   (RecIdx.mixComponent FL.i0 (XIdx.power crossing sink) .= 0) :

   (RecIdx.mixTotal (XIdx.eta source0 crossing) .= 0.3) :
   (RecIdx.mixTotal (XIdx.eta source1 crossing) .= 0.6) :
   (RecIdx.mixTotal (XIdx.eta crossing storage) .= 0.75) :
   (RecIdx.mixTotal (XIdx.eta crossing sink) .= 0.8) :
   []

sinkMixSolution :: FlowTopo.Section Node (Mix (Result Double))
sinkMixSolution =
   EqSys.solveOpts EqSys.optionsSinkMix
      (FlowTopo.sectionFromPlain $ Topo.flowFromPlain topology)
      sinkMixSystem


cumulatedSolution :: FlowTopo.Section Node (Mix (Result Double))
cumulatedSolution =
   FlowTopoPlain.mapEdge (fmap StateFlow.flowResultFromCumResult) $
   FlowTopoPlain.checkedZipWith "cumulatedSolution"
      (liftA2 (liftA2 (~+)))
      (const $ const $ pure unknown)
      (liftA2 (liftA2 (liftA2 (liftA2 (~+)))))
      (FlowTopoPlain.mapEdge (fmap StateFlow.cumFromFlow) sourceMixSolution)
      (FlowTopoPlain.mapEdge (fmap StateFlow.cumFromFlow) sinkMixSolution)



seqFlowGraph :: SeqFlow.Graph Node (Mix (Result a)) (Mix (Result v))
seqFlowGraph =
   let state0 =
          [dirEdge source0 crossing, dirEdge source1 crossing,
           dirEdge crossing storage, dirEdge crossing sink]
       state1 =
          [dirEdge source0 crossing, dirEdge source1 crossing,
           dirEdge storage crossing]

   in  seqFlowGraphFromStates topology [state0, state1, state0]


sec0, sec1, sec2 :: PartIdx.Section
sec0 :~ sec1 :~ sec2 :~ _ = Stream.enumFrom $ PartIdx.section0

seqSourceMixSystem ::
   SeqEqSys.EquationSystem Verify.Ignore Mix Node s Double Double
seqSourceMixSystem =
   mconcat $

   (RecIdx.mixComponent FL.i0 (SeqIdx.stOutSum SeqIdx.initSection storage) SeqEqSys..= 0.1) :
   (RecIdx.mixComponent FL.i1 (SeqIdx.stOutSum SeqIdx.initSection storage) SeqEqSys..= 0.2) :

   SeqEqSys.fromSectionSystem sec0 sourceMixSystem :

   (RecIdx.mixTotal (SeqIdx.stEnergy sec0 sec1 storage) SeqEqSys..= 0.2) :

   (SeqEqSys.fromSectionSystem sec1 $ mconcat $
      (RecIdx.mixTotal XIdx.dTime .= 0.3) :
      (RecIdx.mixComponent FL.i0 (XIdx.power source0 crossing) .= 5) :
      (RecIdx.mixComponent FL.i1 (XIdx.power source0 crossing) .= 0) :
      (RecIdx.mixComponent FL.i0 (XIdx.power source1 crossing) .= 0) :
      (RecIdx.mixComponent FL.i1 (XIdx.power source1 crossing) .= 7) :
      (RecIdx.mixTotal (XIdx.eta source0 crossing) .= 0.3) :
      (RecIdx.mixTotal (XIdx.eta source1 crossing) .= 0.4) :
      (RecIdx.mixTotal (XIdx.eta storage crossing) .= 0.9) :
      (RecIdx.mixTotal (XIdx.eta crossing sink) .= 0.7) :
      []) :

   (SeqEqSys.fromSectionSystem sec2 $ mconcat $
      (RecIdx.mixTotal XIdx.dTime .= 0.6) :
      (RecIdx.mixComponent FL.i0 (XIdx.power source0 crossing) .= 4) :
      (RecIdx.mixComponent FL.i1 (XIdx.power source0 crossing) .= 0) :
      (RecIdx.mixComponent FL.i0 (XIdx.power source1 crossing) .= 0) :
      (RecIdx.mixComponent FL.i1 (XIdx.power source1 crossing) .= 4) :
      (RecIdx.mixTotal (XIdx.eta source0 crossing) .= 0.7) :
      (RecIdx.mixTotal (XIdx.eta source1 crossing) .= 0.2) :
      (RecIdx.mixTotal (XIdx.eta crossing storage) .= 0.1) :
      (RecIdx.mixTotal (XIdx.eta crossing sink) .= 0.6) :
      (RecIdx.mixTotal (XIdx.x crossing storage) .= 0.5) :
      []) :

   []

seqSourceMixSolution ::
   SeqFlow.Graph Node (Mix (Result Double)) (Mix (Result Double))
seqSourceMixSolution =
   SeqEqSys.solveOpts
      (SeqEqSys.equalStInOutSums SeqEqSys.optionsSourceMix)
      seqFlowGraph seqSourceMixSystem

stateSourceMixSolution ::
   StateFlow.Graph Node (Mix (Result Double)) (Mix (Result Double))
stateSourceMixSolution =
   StateEqSys.solve
      (StateFlow.graphFromCumResult $
       StateFlow.fromSequenceFlowRecordResult False seqSourceMixSolution)
      mempty

seqCumulatedSolution ::
   CumFlow.Graph Node (Mix (Result Double))
seqCumulatedSolution =
   Graph.mapEdge CumFlow.flowResultFromCumResult $
   CumFlow.fromSequenceFlowRecordResult $
   SeqFlow.sequence seqSourceMixSolution


type MultiMix = Record.ExtMix (NonEmpty.T Empty.T) Mix

idxMixTotal :: RecIdx.Mix pos
idxMixTotal = RecIdx.MixTotal

idxMix0 :: RecIdx.Mix (FL.WrapPos (FL.GE1 list))
idxMix0 = RecIdx.MixComponent FL.i0

idxMix1 :: RecIdx.Mix (FL.WrapPos (FL.GE2 list))
idxMix1 = RecIdx.MixComponent FL.i1

idxMultiMix ::
   RecIdx.Mix pos0 -> RecIdx.Mix pos1 ->
   idx -> RecIdx.Record (RecIdx.ExtMix pos0 (RecIdx.Mix pos1)) idx
idxMultiMix a b =
   RecIdx.Record (RecIdx.ExtMix a b)

idxMultiMixTotal ::
   idx -> RecIdx.Record (RecIdx.ExtMix pos0 (RecIdx.Mix pos1)) idx
idxMultiMixTotal = idxMultiMix idxMixTotal idxMixTotal

multiMixSystem ::
   EqSys.EquationSystem Verify.Ignore MultiMix Node s Double
multiMixSystem =
   mconcat $

   (idxMultiMixTotal XIdx.dTime .= 0.5) :

   (idxMultiMix idxMix0 idxMixTotal (XIdx.power crossing source0) .= 4) :
   (idxMultiMix idxMix1 idxMix0   (XIdx.power crossing source0) .= 0) :
   (idxMultiMix idxMix1 idxMix1   (XIdx.power crossing source0) .= 0) :

   (idxMultiMix idxMix1 idxMixTotal (XIdx.power crossing source1) .= 3) :
   (idxMultiMix idxMix0 idxMix0   (XIdx.power crossing source1) .= 0) :
   (idxMultiMix idxMix0 idxMix1   (XIdx.power crossing source1) .= 0) :

   (idxMultiMix idxMixTotal idxMix0 (XIdx.power sink crossing) .= 5) :
   (idxMultiMix idxMix0   idxMix1 (XIdx.power sink crossing) .= 0) :

   (idxMultiMixTotal (XIdx.eta source0 crossing) .= 0.25) :
   (idxMultiMixTotal (XIdx.eta source1 crossing) .= 0.5) :
   (idxMultiMixTotal (XIdx.eta crossing storage) .= 0.75) :
   (idxMultiMixTotal (XIdx.eta crossing sink) .= 0.8) :
   []

multiMixSolution :: FlowTopo.Section Node (MultiMix (Result Double))
multiMixSolution =
   EqSys.solveOpts
      (EqSys.optionsMix (EqSys.Source !: EqSys.Sink !: Empty.Cons))
      (FlowTopo.sectionFromPlain $ Topo.flowFromPlain topology)
      multiMixSystem


main :: IO ()
main = do
   mapM_ (putStrLn . Format.unUnicode) $
      AssignMap.format $ FlowTopo.toAssignMap sourceMixSolution

   concurrentlyMany_ $
      (map (Draw.xterm . Draw.flowSection Draw.optionsDefault) $
          sourceMixSolution :
          sinkMixSolution :
          cumulatedSolution :
          EqSys.solve cumulatedSolution mempty :
          [])
      ++
      [Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault seqSourceMixSolution]
      ++
      [Draw.xterm $ Draw.stateFlowGraph Draw.optionsDefault stateSourceMixSolution]
      ++
      [Draw.xterm $ Draw.cumulatedFlow seqCumulatedSolution]
      ++
      [Draw.xterm $ Draw.cumulatedFlow $
       CumEqSys.solve seqCumulatedSolution mempty]
      ++
      [Draw.xterm $ Draw.flowSection Draw.optionsDefault multiMixSolution]
