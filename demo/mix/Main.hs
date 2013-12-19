{-# LANGUAGE TypeFamilies #-}
module Main where

import EFA.Example.Topology.FourLeaves
          (Node, source0, source1, crossing, storage, sink, topology)
import EFA.Example.Record.SourceSinkMix
          (MultiMix, idxMixTotal, idxMix0, idxMix1,
           idxMultiMix, idxMultiMixTotal)

import EFA.Application.Utility (seqFlowGraphFromStates, dirEdge)

import qualified EFA.Flow.State.EquationSystem as StateEqSys
import qualified EFA.Flow.State.Quantity as StateFlow

import qualified EFA.Flow.Sequence.EquationSystem as SeqEqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as SeqIdx

import qualified EFA.Flow.Cumulated.EquationSystem as CumEqSys
import qualified EFA.Flow.Cumulated.Quantity as CumFlow

import qualified EFA.Flow.Storage.EquationSystem as StorageEqSys

import qualified EFA.Flow.Topology.EquationSystem as EqSys
import qualified EFA.Flow.Topology.AssignMap as AssignMap
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Flow.Part.Index as PartIdx
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)
import EFA.Equation.Unknown (unknown)
import EFA.Equation.Arithmetic ((~+))

import qualified EFA.Report.Format as Format

import qualified EFA.Utility.FixedLength as FL
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Async (concurrentlyMany_)

import Control.Applicative (liftA2, pure)

import Data.Monoid (Monoid, mconcat, mempty)


infix 0 .=

(.=) ::
   (Arith.Constant x, Verify.LocalVar mode x, FlowTopo.Lookup idx,
    EqSys.Record rec, Record.ToIndex rec ~ recIdx) =>
   RecIdx.Record recIdx (idx Node) -> Rational ->
   EqSys.EquationSystem mode rec Node s x
evar .= val  =
   evar EqSys..= Arith.fromRational val


type SourceMix = Record.SourceMix FL.N1

sourceMixSystem0, sourceMixSystem1, sourceMixSystem2 ::
   EqSys.EquationSystem Verify.Ignore SourceMix Node s Double
sourceMixSystem0 =
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

sourceMixSystem1 =
   mconcat $

   (RecIdx.mixTotal XIdx.dTime .= 0.3) :
   (RecIdx.mixComponent FL.i0 (XIdx.power source0 crossing) .= 5) :
   (RecIdx.mixComponent FL.i1 (XIdx.power source0 crossing) .= 0) :
   (RecIdx.mixComponent FL.i0 (XIdx.power source1 crossing) .= 0) :
   (RecIdx.mixComponent FL.i1 (XIdx.power source1 crossing) .= 7) :
   (RecIdx.mixTotal (XIdx.eta source0 crossing) .= 0.3) :
   (RecIdx.mixTotal (XIdx.eta source1 crossing) .= 0.4) :
   (RecIdx.mixTotal (XIdx.eta storage crossing) .= 0.9) :
   (RecIdx.mixTotal (XIdx.eta crossing sink) .= 0.7) :
   []

sourceMixSystem2 =
   mconcat $

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
   []


sourceMixSolution0 :: FlowTopo.Section Node (SourceMix (Result Double))
sourceMixSolution0 =
   EqSys.solveOpts
      (EqSys.realMix EqSys.optionsDefault)
      (FlowTopo.sectionFromPlain $ Topo.flowFromPlain topology)
      sourceMixSystem0

sourceMixSolution2 :: FlowTopo.Section Node (SourceMix (Result Double))
sourceMixSolution2 =
   EqSys.solveOpts
      (EqSys.realMix EqSys.optionsDefault)
      (FlowTopo.sectionFromPlain $ Topo.flowFromPlain topology)
      sourceMixSystem2


type SinkMix = Record.SinkMix FL.N1

sinkMixSystem ::
   EqSys.EquationSystem Verify.Ignore SinkMix Node s Double
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

sinkMixSolution :: FlowTopo.Section Node (SinkMix (Result Double))
sinkMixSolution =
   EqSys.solveOpts
      (EqSys.realMix EqSys.optionsDefault)
      (FlowTopo.sectionFromPlain $ Topo.flowFromPlain topology)
      sinkMixSystem


cumulatedSolution :: FlowTopo.Section Node (SourceMix (Result Double))
cumulatedSolution =
   FlowTopoPlain.mapEdge (fmap StateFlow.flowResultFromCumResult) $
   FlowTopoPlain.checkedZipWith "cumulatedSolution"
      (liftA2 (liftA2 (~+)))
      (const $ const $ pure unknown)
      (liftA2 (liftA2 (liftA2 (liftA2 (~+)))))
      (FlowTopoPlain.mapEdge (fmap StateFlow.cumFromFlow) sourceMixSolution0)
      (FlowTopoPlain.mapEdge (fmap StateFlow.cumFromFlow) sourceMixSolution2)




seqFlowGraph :: SeqFlow.Graph Node (SourceMix (Result a)) (SourceMix (Result v))
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
   SeqEqSys.EquationSystem Verify.Ignore SourceMix Node s Double Double
seqSourceMixSystem =
   mconcat $

   (RecIdx.mixComponent FL.i0 (SeqIdx.stOutSum SeqIdx.initSection storage) SeqEqSys..= 0.1) :
   (RecIdx.mixComponent FL.i1 (SeqIdx.stOutSum SeqIdx.initSection storage) SeqEqSys..= 0.2) :

   SeqEqSys.fromSectionSystem sec0 sourceMixSystem0 :
   SeqEqSys.fromSectionSystem sec1 sourceMixSystem1 :
   SeqEqSys.fromSectionSystem sec2 sourceMixSystem2 :

   (RecIdx.mixTotal (SeqIdx.stEnergy sec0 sec1 storage) SeqEqSys..= 0.2) :

   []

seqSourceMixSolution ::
   SeqFlow.Graph Node (SourceMix (Result Double)) (SourceMix (Result Double))
seqSourceMixSolution =
   SeqEqSys.solveOpts
      (SeqEqSys.optionsBase SeqEqSys.equalStInOutSums StorageEqSys.classOne)
      seqFlowGraph seqSourceMixSystem

stateSourceMixSolution ::
   StateFlow.Graph Node (SourceMix (Result Double)) (SourceMix (Result Double))
stateSourceMixSolution =
   StateEqSys.solve
      (StateFlow.graphFromCumResult $
       StateFlow.fromSequenceFlowRecordResult False seqSourceMixSolution)
      mempty

seqCumulatedSolution ::
   CumFlow.Graph Node (SourceMix (Result Double))
seqCumulatedSolution =
   Graph.mapEdge CumFlow.flowResultFromCumResult $
   CumFlow.fromSequenceFlowRecordResult $
   SeqFlow.sequence seqSourceMixSolution


multiMixSystem ::
   (Arith.Constant a) =>
   EqSys.EquationSystem Verify.Ignore MultiMix Node s a
multiMixSystem =
   mconcat $

   (idxMultiMixTotal XIdx.dTime .= 0.5) :

   (idxMultiMixTotal (XIdx.power source0 crossing) .= 4) :
   (idxMultiMix idxMix1 idxMixTotal (XIdx.power source0 crossing) .= 0) :

   (idxMultiMixTotal (XIdx.power source1 crossing) .= 3) :
   (idxMultiMix idxMix0 idxMixTotal (XIdx.power source1 crossing) .= 0) :

   (idxMultiMixTotal (XIdx.power sink crossing) .= 0.4) :
   (idxMultiMix idxMixTotal idxMix1 (XIdx.power sink crossing) .= 0) :

   (idxMultiMix idxMixTotal idxMix0 (XIdx.power storage crossing) .= 0) :

   (idxMultiMixTotal (XIdx.eta source0 crossing) .= 0.25) :
   (idxMultiMixTotal (XIdx.eta source1 crossing) .= 0.5) :
   (idxMultiMixTotal (XIdx.eta crossing storage) .= 0.75) :
   (idxMultiMixTotal (XIdx.eta crossing sink) .= 0.8) :
   []


multiMixSolution :: FlowTopo.Section Node (MultiMix (Result Double))
multiMixSolution =
   EqSys.solveOpts
      (EqSys.realMix EqSys.optionsDefault)
      (FlowTopo.sectionFromPlain $ Topo.flowFromPlain topology)
      multiMixSystem

multiMixSolutionRatio :: FlowTopo.Section Node (MultiMix (Result Rational))
multiMixSolutionRatio =
   EqSys.solveOpts
      (EqSys.realMix EqSys.optionsDefault)
      (FlowTopo.sectionFromPlain $ Topo.flowFromPlain topology)
      multiMixSystem


main :: IO ()
main = do
   mapM_ (putStrLn . Format.unUnicode) $
      AssignMap.format $ FlowTopo.toAssignMap multiMixSolution

   mapM_ (putStrLn . Format.unUnicode) $
      AssignMap.format $ FlowTopo.toAssignMap multiMixSolutionRatio

   concurrentlyMany_ $
      (map
          (\(title, graph) ->
             Draw.xterm $ Draw.title title $
             Draw.flowSection Draw.optionsDefault graph) $
          ("source mix", sourceMixSolution0) :
          ("added source mixes", cumulatedSolution) :
          ("complete added source mixes",
           EqSys.solve cumulatedSolution mempty) :
          [])
      ++
      [Draw.xterm $ Draw.title "sink mix" $
       Draw.flowSection Draw.optionsDefault sinkMixSolution]
      ++
      [Draw.xterm $ Draw.title "sequence flow mix" $
       Draw.seqFlowGraph Draw.optionsDefault seqSourceMixSolution]
      ++
      [Draw.xterm $ Draw.title "state flow mix" $
       Draw.stateFlowGraph Draw.optionsDefault stateSourceMixSolution]
      ++
      [Draw.xterm $ Draw.title "cumulated flow mix" $
       Draw.cumulatedFlow seqCumulatedSolution]
      ++
      [Draw.xterm $ Draw.title "complete cumulated flow mix" $
       Draw.cumulatedFlow $ CumEqSys.solve seqCumulatedSolution mempty]
      ++
      [Draw.xterm $
       Draw.title "combined source and sink mix - non-uniform eta caused by eps/eps problem" $
       Draw.flowSection Draw.optionsDefault multiMixSolution]
      ++
      [Draw.xterm $
       Draw.title "combined source and sink mix - no eps/eps problem" $
       Draw.flowSection Draw.optionsDefault multiMixSolutionRatio]
