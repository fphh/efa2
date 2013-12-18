{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module EFA.Test.Mix.Given where

import EFA.Example.Topology.FourLeaves
          (Node, source0, source1, crossing, storage, sink, topology)
import EFA.Example.Record.SourceSinkMix
          (MultiMix, idxMixTotal, idxMix0, idxMix1,
           idxMultiMix, idxMultiMixTotal)

import qualified EFA.Flow.Topology.EquationSystem as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Symbolic as Symbol
import qualified EFA.Flow.Topology.Index as XIdx

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


multiMixOptions ::
   (Verify.LocalVar mode a, Arith.Sum a) =>
   EqSys.Options mode MultiMix s a
multiMixOptions =
   EqSys.realMix EqSys.optionsDefault


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


type MixRecIdx =
        RecIdx.ExtSourceMix (FL.WrapPos FL.N2)
           (RecIdx.SinkMix (FL.WrapPos FL.N2))

type Tracked = Pair.T (Symbol.Term Term MixRecIdx Node) Rational

type EquationSystem s =
        EqSys.EquationSystem (Verify.Track Format.Unicode) MultiMix Node s Tracked

data
   Equation mode a =
      Equation {
         getEquation :: forall s.
            EqSys.EquationSystem mode MultiMix Node s a
      }


infix 0 .=

(.=) ::
   (Arith.Constant x, Verify.LocalVar mode x, FlowTopo.Lookup idx) =>
   RecIdx.Record MixRecIdx (idx Node) -> Rational ->
   Equation mode x
var .= val  =
   Equation (var EqSys..= Arith.fromRational val)


partialGiven :: EquationSystem s
partialGiven = foldMap getEquation partialEquations

partialEquations ::
   (Verify.LocalVar mode a, Arith.Constant a) =>
   [Equation mode a]
partialEquations =
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


infixl 1 #

(#) :: a -> (a,a) -> Record.Mix dir FL.N1 a
(#) total (x,y) = Record.Mix total $ x !: y !: FL.end


infix 0 %=

(%=) ::
   (Arith.Constant x, Verify.LocalVar mode x, FlowTopo.Lookup idx) =>
   idx Node -> Record.SourceMix FL.N1 (Record.SinkMix FL.N1 Rational) ->
   EqSys.EquationSystem mode MultiMix Node s x
evar %= val  =
   evar EqSys.%= fmap Arith.fromRational (Record.ExtMix val)


fullGiven :: EquationSystem s
fullGiven = mconcat $
   (XIdx.dTime %= 1/2 # (1/2,1/2) # (1/2 # (1/2,1/2),1/2 # (1/2,1/2))) :
   (XIdx.energy source0 crossing %= 2 # (2/5,8/5) # (2 # (2/5,8/5),0 # (0,0))) :
   (XIdx.energy source1 crossing %= 3/2 # (3/10,6/5) # (0 # (0,0),3/2 # (3/10,6/5))) :
   (XIdx.energy crossing source0 %= 1/2 # (1/10,2/5) # (1/2 # (1/10,2/5),0 # (0,0))) :
   (XIdx.energy crossing source1 %= 3/4 # (3/20,3/5) # (0 # (0,0),3/4 # (3/20,3/5))) :
   (XIdx.energy crossing storage %= 1 # (0,1) # (2/5 # (0,2/5),3/5 # (0,3/5))) :
   (XIdx.energy crossing sink %= 1/4 # (1/4,0) # (1/10 # (1/10,0),3/20 # (3/20,0))) :
   (XIdx.energy storage crossing %= 3/4 # (0,3/4) # (3/10 # (0,3/10),9/20 # (0,9/20))) :
   (XIdx.energy sink crossing %= 1/5 # (1/5,0) # (2/25 # (2/25,0),3/25 # (3/25,0))) :
   (XIdx.power source0 crossing %= 4 # (4/5,16/5) # (4 # (4/5,16/5),0 # (0,0))) :
   (XIdx.power source1 crossing %= 3 # (3/5,12/5) # (0 # (0,0),3 # (3/5,12/5))) :
   (XIdx.power crossing source0 %= 1 # (1/5,4/5) # (1 # (1/5,4/5),0 # (0,0))) :
   (XIdx.power crossing source1 %= 3/2 # (3/10,6/5) # (0 # (0,0),3/2 # (3/10,6/5))) :
   (XIdx.power crossing storage %= 2 # (0,2) # (4/5 # (0,4/5),6/5 # (0,6/5))) :
   (XIdx.power crossing sink %= 1/2 # (1/2,0) # (1/5 # (1/5,0),3/10 # (3/10,0))) :
   (XIdx.power storage crossing %= 3/2 # (0,3/2) # (3/5 # (0,3/5),9/10 # (0,9/10))) :
   (XIdx.power sink crossing %= 2/5 # (2/5,0) # (4/25 # (4/25,0),6/25 # (6/25,0))) :
   (XIdx.eta source0 crossing %= 1/4 # (1/4,1/4) # (1/4 # (1/4,1/4),1/4 # (1/4,1/4))) :
   (XIdx.eta source1 crossing %= 1/2 # (1/2,1/2) # (1/2 # (1/2,1/2),1/2 # (1/2,1/2))) :
   (XIdx.eta crossing storage %= 3/4 # (3/4,3/4) # (3/4 # (3/4,3/4),3/4 # (3/4,3/4))) :
   (XIdx.eta crossing sink %= 4/5 # (4/5,4/5) # (4/5 # (4/5,4/5),4/5 # (4/5,4/5))) :
   (XIdx.x source0 crossing %= 1 # (1,1) # (1 # (1,1),1 # (1,1))) :
   (XIdx.x source1 crossing %= 1 # (1,1) # (1 # (1,1),1 # (1,1))) :
   (XIdx.x crossing source0 %= 2/5 # (2/5,2/5) # (1 # (1,1),0 # (0,0))) :
   (XIdx.x crossing source1 %= 3/5 # (3/5,3/5) # (0 # (0,0),1 # (1,1))) :
   (XIdx.x crossing storage %= 4/5 # (0,1) # (4/5 # (0,1),4/5 # (0,1))) :
   (XIdx.x crossing sink %= 1/5 # (1,0) # (1/5 # (1,0),1/5 # (1,0))) :
   (XIdx.x storage crossing %= 1 # (1,1) # (1 # (1,1),1 # (1,1))) :
   (XIdx.x sink crossing %= 1 # (1,1) # (1 # (1,1),1 # (1,1))) :
   (XIdx.inSum crossing %= 5/4 # (1/4,1) # (1/2 # (1/10,2/5),3/4 # (3/20,3/5))) :
   (XIdx.inSum storage %= 3/4 # (0,3/4) # (3/10 # (0,3/10),9/20 # (0,9/20))) :
   (XIdx.inSum sink %= 1/5 # (1/5,0) # (2/25 # (2/25,0),3/25 # (3/25,0))) :
   (XIdx.outSum source0 %= 2 # (2/5,8/5) # (2 # (2/5,8/5),0 # (0,0))) :
   (XIdx.outSum source1 %= 3/2 # (3/10,6/5) # (0 # (0,0),3/2 # (3/10,6/5))) :
   (XIdx.outSum crossing %= 5/4 # (1/4,1) # (1/2 # (1/10,2/5),3/4 # (3/20,3/5))) :
   []
