{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module EFA.Test.Mix.Given where

import EFA.Application.Utility (topologyFromEdges)

import qualified EFA.Flow.Topology.EquationSystem as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Symbolic as Symbol
import qualified EFA.Flow.Topology.Index as XIdx

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Pair as Pair
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified EFA.Report.Format as Format

import EFA.Symbolic.SumProduct ( Term )

import qualified EFA.Utility.FixedLength as FL
import EFA.Utility.FixedLength ((!:))

import qualified Control.Monad.Exception.Synchronous as ME

import Data.Tuple.HT (mapFst)
import Data.Monoid (Monoid, mconcat)
import Data.Foldable (foldMap)



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


type Mix = Record.Mix FL.N1

type MultiMix = Record.ExtMix FL.N1 Mix

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

   (idxMultiMixTotal (XIdx.power source0 crossing) .= 4) :
   (idxMultiMix idxMix1 idxMix0 (XIdx.power source0 crossing) .= 0) :
   (idxMultiMix idxMix1 idxMix1 (XIdx.power source0 crossing) .= 0) :

   (idxMultiMixTotal (XIdx.power source1 crossing) .= 3) :
   (idxMultiMix idxMix0 idxMix0 (XIdx.power source1 crossing) .= 0) :
   (idxMultiMix idxMix0 idxMix1 (XIdx.power source1 crossing) .= 0) :

   (idxMultiMixTotal (XIdx.power sink crossing) .= 0.4) :
   (idxMultiMix idxMix0 idxMix1 (XIdx.power sink crossing) .= 0) :
   (idxMultiMix idxMix1 idxMix1 (XIdx.power sink crossing) .= 0) :

   (idxMultiMix idxMix0 idxMix0 (XIdx.power storage crossing) .= 0) :
   (idxMultiMix idxMix1 idxMix0 (XIdx.power storage crossing) .= 0) :

   (idxMultiMixTotal (XIdx.eta source0 crossing) .= 0.25) :
   (idxMultiMixTotal (XIdx.eta source1 crossing) .= 0.5) :
   (idxMultiMixTotal (XIdx.eta crossing storage) .= 0.75) :
   (idxMultiMixTotal (XIdx.eta crossing sink) .= 0.8) :
   []

multiMixOptions ::
   (Verify.LocalVar mode a, Arith.Sum a) =>
   EqSys.Options mode MultiMix s a
multiMixOptions =
   EqSys.mix (EqSys.Source !: EqSys.Sink !: FL.end) $
   EqSys.optionsDefault

multiMixSolution :: FlowTopo.Section Node (MultiMix (Result Double))
multiMixSolution =
   EqSys.solveOpts multiMixOptions flowGraph multiMixSystem


type ResultGraph a = FlowTopo.Section Node (MultiMix (Result a))

flowGraph :: ResultGraph a
flowGraph = FlowTopo.sectionFromPlain $ Topo.flowFromPlain topology


fullGraph, solvedGraph ::
   (ME.Exceptional
      (Verify.Exception Format.Unicode)
      (ResultGraph Rational),
    Verify.Assigns Format.Unicode)
fullGraph =
   mapFst (fmap numericGraph) $
   EqSys.solveTrackedOpts multiMixOptions flowGraph fullGiven

solvedGraph =
   mapFst (fmap numericGraph) $
   EqSys.solveTrackedOpts multiMixOptions flowGraph partialGiven

numericGraph ::
   ResultGraph (Pair.T at an) ->
   ResultGraph an
numericGraph =
   FlowTopo.mapSection (fmap $ fmap Pair.second)


infix 0 .=

(.=) ::
   (Arith.Constant x, Verify.LocalVar mode x, FlowTopo.Lookup idx) =>
   RecIdx.Record MixRecIdx (idx Node) -> Rational ->
   EqSys.EquationSystem mode MultiMix Node s x
evar .= val  =
   evar EqSys..= Arith.fromRational val


type MixRecIdx = RecIdx.ExtMix (FL.WrapPos FL.N2) (RecIdx.Mix (FL.WrapPos FL.N2))

type Tracked = Pair.T (Symbol.Term Term MixRecIdx Node) Rational

type EquationSystem s =
        EqSys.EquationSystem (Verify.Track Format.Unicode) MultiMix Node s Tracked

data
   Equation mode a =
      Equation {
         getEquation :: forall s.
            EqSys.EquationSystem mode MultiMix Node s a
      }

partialGiven :: EquationSystem s
partialGiven = foldMap getEquation partialEquations

partialEquations ::
   (Verify.LocalVar mode a, Arith.Constant a) =>
   [Equation mode a]
partialEquations =
   Equation (idxMultiMixTotal XIdx.dTime .= 0.5) :

   Equation (idxMultiMixTotal (XIdx.power source0 crossing) .= 4) :
   Equation (idxMultiMix idxMix1 idxMix0 (XIdx.power source0 crossing) .= 0) :
   Equation (idxMultiMix idxMix1 idxMix1 (XIdx.power source0 crossing) .= 0) :

   Equation (idxMultiMixTotal (XIdx.power source1 crossing) .= 3) :
   Equation (idxMultiMix idxMix0 idxMix0 (XIdx.power source1 crossing) .= 0) :
   Equation (idxMultiMix idxMix0 idxMix1 (XIdx.power source1 crossing) .= 0) :

   Equation (idxMultiMixTotal (XIdx.power sink crossing) .= 0.4) :
   Equation (idxMultiMix idxMix0 idxMix1 (XIdx.power sink crossing) .= 0) :

   Equation (idxMultiMix idxMix0 idxMix0 (XIdx.power storage crossing) .= 0) :

   Equation (idxMultiMixTotal (XIdx.eta source0 crossing) .= 0.25) :
   Equation (idxMultiMixTotal (XIdx.eta source1 crossing) .= 0.5) :
   Equation (idxMultiMixTotal (XIdx.eta crossing storage) .= 0.75) :
   Equation (idxMultiMixTotal (XIdx.eta crossing sink) .= 0.8) :
   []


fullGiven :: EquationSystem s
fullGiven = mconcat $
   (idxMultiMixTotal XIdx.dTime .= 0.5) :

   (idxMultiMixTotal (XIdx.power source0 crossing) .= 4) :
   (idxMultiMix idxMix1 idxMix0 (XIdx.power source0 crossing) .= 0) :
   (idxMultiMix idxMix1 idxMix1 (XIdx.power source0 crossing) .= 0) :

   (idxMultiMixTotal (XIdx.power source1 crossing) .= 3) :
   (idxMultiMix idxMix0 idxMix0 (XIdx.power source1 crossing) .= 0) :
   (idxMultiMix idxMix0 idxMix1 (XIdx.power source1 crossing) .= 0) :

   (idxMultiMixTotal (XIdx.power sink crossing) .= 0.4) :
   (idxMultiMix idxMix0 idxMix1 (XIdx.power sink crossing) .= 0) :
   (idxMultiMix idxMix1 idxMix1 (XIdx.power sink crossing) .= 0) :

   (idxMultiMix idxMix0 idxMix0 (XIdx.power storage crossing) .= 0) :
   (idxMultiMix idxMix1 idxMix0 (XIdx.power storage crossing) .= 0) :

   (idxMultiMixTotal (XIdx.eta source0 crossing) .= 0.25) :
   (idxMultiMixTotal (XIdx.eta source1 crossing) .= 0.5) :
   (idxMultiMixTotal (XIdx.eta crossing storage) .= 0.75) :
   (idxMultiMixTotal (XIdx.eta crossing sink) .= 0.8) :
   []
