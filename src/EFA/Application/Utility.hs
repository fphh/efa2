{-# LANGUAGE TypeFamilies #-}
module EFA.Application.Utility where

import qualified EFA.Flow.Sequence.Quantity as SeqFlowQuant
import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.Sequence as SeqFlow
import qualified EFA.Flow.State.Index as StateIdx

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology as FlowTopoPlain

import qualified EFA.Graph.StateFlow.Environment as StateEnv

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Data as Data

import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Determined, Undetermined))
import EFA.Equation.System ((.=))

import EFA.Report.FormatValue (FormatValue)

import qualified EFA.Utility.Map as MapU
import EFA.Utility.Map (checkedLookup)
import EFA.Utility (myShowList)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Foldable (foldMap)


topologyFromEdges ::
   Node.C node => [(node, node)] -> Topo.Topology node
topologyFromEdges es =
   Graph.fromMap
      (MapU.fromSet Node.typ $ foldMap (\(x,y) -> Set.fromList [x,y]) es)
      (Map.fromList $ map (\(a, b) -> (Graph.DirEdge a b, ())) es)


-- @HT neue Utility Funktionen für Topologie-Definition, bitte prüfen
type EdgeLabel = String
type PPosLabel = String

type LabeledEdgeList node = [(node, node, EdgeLabel, PPosLabel, PPosLabel)]
type PPosLabelMap node = Map (SeqIdx.PPos node) String

-- | Generate Topology from labeled edge List
makeTopology ::
   (Node.C node) =>
   LabeledEdgeList node -> Topo.Topology node
makeTopology =
   topologyFromEdges . map (\(n1,n2,_,_,_) -> (n1,n2))


-- | Edge Label map used for displaying topology with labeled edges
makeEdgeNameMap :: (Ord node) => LabeledEdgeList node -> Map (node, node) String
makeEdgeNameMap edgeList = Map.fromList $ map f edgeList
  where f (x, y, lab, _, _) = ((x, y), lab)

-- | Generate Label Map for Power Positions
makePPosLabelMap :: (Ord node) => LabeledEdgeList node -> PPosLabelMap node
makePPosLabelMap edgeList = Map.fromList $ concatMap f edgeList
  where f (n1,n2,_,l1,l2) = [(SeqIdx.ppos n1 n2, l1),
                             (SeqIdx.ppos n2 n1, l2)]


{- |
Construct solvable topology from topology with default directions.
-}
quantityTopology ::
   (Ord node, SeqFlowQuant.Unknown v) =>
   Topo.Topology node ->
   FlowTopo.Section node v
quantityTopology topo =
   FlowTopo.sectionFromPlain $
   FlowTopoPlain.Section () $
   Topo.classifyStorages $
   let flowTopo = Graph.mapEdgesMaybe (Just . Graph.EDirEdge) topo
   in  if StateAnalysis.admissibleTopology flowTopo
         then flowTopo
         else error "quantityTopology: topology has forbidden default edges"


{- |
Construct sequence flow graph with a single section
containing the topology with default directions.
-}
seqFlowGraphFromTopology ::
   (Ord node, SeqFlowQuant.Unknown a, SeqFlowQuant.Unknown v) =>
   Topo.Topology node ->
   SeqFlowQuant.Graph node a v
seqFlowGraphFromTopology topo =
   SeqFlowQuant.graphFromPlain $
   SeqFlow.sequenceGraph $
   Sequ.fromList $ (:[]) $
   let flowTopo = Graph.mapEdgesMaybe (Just . Graph.EDirEdge) topo
   in  if StateAnalysis.admissibleTopology flowTopo
         then flowTopo
         else error "seqFlowGraphFromTopology: topology has forbidden default edges"


dirEdge :: node -> node -> Graph.EitherEdge node
dirEdge x y = Graph.EDirEdge $ Graph.DirEdge x y

undirEdge :: (Ord node) => node -> node -> Graph.EitherEdge node
undirEdge x y = Graph.EUnDirEdge $ Graph.UnDirEdge x y

identifyFlowState ::
   (Ord node) =>
   Topo.Topology node -> [Graph.EitherEdge node] -> Topo.FlowTopology node
identifyFlowState topo givenEdges =
   case StateAnalysis.identify topo givenEdges of
      [] -> error "identifyFlowState: impossible given edges"
      [flowTopo] -> flowTopo
      _ -> error "identifyFlowState: ambiguous given edges"

identifyFlowStates ::
   (Ord node) =>
   Topo.Topology node -> [Graph.EitherEdge node] -> [Topo.FlowTopology node]
identifyFlowStates topo givenEdges = StateAnalysis.identify topo givenEdges


seqFlowGraphFromStates ::
   (Ord node, SeqFlowQuant.Unknown a, SeqFlowQuant.Unknown v) =>
   Topo.Topology node ->
   [[Graph.EitherEdge node]] ->
   SeqFlowQuant.Graph node a v
seqFlowGraphFromStates topo =
   SeqFlowQuant.graphFromPlain .
   SeqFlow.sequenceGraph .
   fmap (identifyFlowState topo) .
   Sequ.fromList


checkDetermined :: String -> Result a -> a
checkDetermined name rx =
   case rx of
      Undetermined -> error $ "undetermined " ++ name
      Determined x -> x


{-
Two restricted variants of (.=).
I added them in the hope for simpler type signatures.
The type signatures are still complex, unfortunately.
The symbols are not used, too.
-}
infix 0 #=, ~=

-- | @(.=)@ restricted to signals
(~=) ::
  (Verify.GlobalVar mode v recIdx var node, Arith.Sum v,
   var ~ Var.InSectionSignal, var ~ Var.Type idx,
   recIdx ~ EqRecord.ToIndex rec, EqGen.Record rec,
   EqEnv.AccessMap idx, EqEnv.Environment idx ~ EqEnv.Signal,
   Ord (idx node), FormatValue (idx node)) =>
  Idx.Record recIdx (idx node) -> v ->
  EqGen.EquationSystem mode rec node s a v
(~=)  =  (.=)

-- | @(.=)@ restricted to scalars
(#=) ::
  (Verify.GlobalVar mode a recIdx var node, Arith.Sum a,
   var ~ Var.ForNodeSectionScalar, var ~ Var.Type idx,
   recIdx ~ EqRecord.ToIndex rec, EqGen.Record rec,
   EqEnv.AccessMap idx, EqEnv.Environment idx ~ EqEnv.Scalar,
   Ord (idx node), FormatValue (idx node)) =>
  Idx.Record recIdx (idx node) -> a ->
  EqGen.EquationSystem mode rec node s a v
(#=)  =  (.=)

-- | Unpack scalar result values in env from Data constructor
envGetData ::
  (Ord node) =>
  EqEnv.Complete node (Result (Data.Data va a)) (Result (Data.Data vv v)) ->
  EqEnv.Complete node (Result (Data.Apply va a)) (Result (Data.Apply vv v))
envGetData =
  EqEnv.completeFMap (fmap Data.getData) (fmap Data.getData)


--  @HH mit @HT klären ob lookup funktionen notwenig und sinnvoll, PG

lookupAbsEnergy ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (Result t) ->
  SeqIdx.Energy node -> Result t
lookupAbsEnergy caller env n =
  checkedLookup caller (EqEnv.energyMap $ EqEnv.signal env) n

lookupAbsPower ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (Result t) ->
  SeqIdx.Power node -> Result t
lookupAbsPower caller env n =
  checkedLookup caller (EqEnv.powerMap $ EqEnv.signal env) n

lookupAbsEta ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (Result t) ->
  SeqIdx.Eta node -> Result t
lookupAbsEta caller env n =
  checkedLookup caller (EqEnv.etaMap $ EqEnv.signal env) n

lookupDetPower ::(Ord node, Show d, Show node) =>
  SeqIdx.Power node -> EqEnv.Complete node b (Result d) -> d
lookupDetPower idx =
  checkDetermined ("lookupDetPower (2): " ++ show idx) .
  flip (lookupAbsPower ("lookupDetPower (1): " ++ show idx)) idx

lookupDetEnergy ::(Ord node, Show d, Show node) =>
  SeqIdx.Energy node -> EqEnv.Complete node b (Result d) -> d
lookupDetEnergy idx =
  checkDetermined ("lookupDetEnergy (2): " ++ show idx) .
  flip (lookupAbsEnergy ("lookupDetEnergy (1): " ++ show idx)) idx


--  @HT schon wieder code-Duplication wegen Stateenv - was können wir da machen ?

lookupAbsEnergyState ::
  (Ord node, Show node, Show t) =>
  String ->
  StateEnv.Complete node b (Result t) ->
  StateIdx.Energy node -> Result t
lookupAbsEnergyState caller env n =
  checkedLookup caller (StateEnv.energyMap $ StateEnv.signal env) n

lookupAbsPowerState ::
  (Ord node, Show node, Show t) =>
  String ->
  StateEnv.Complete node b (Result t) ->
  StateIdx.Power node -> Result t
lookupAbsPowerState caller env n =
  checkedLookup caller (StateEnv.powerMap $ StateEnv.signal env) n

lookupDetPowerState ::
  (Ord node, Show d, Show node) =>
  StateIdx.Power node -> StateEnv.Complete node b (Result d) -> d
lookupDetPowerState idx =
  checkDetermined ("lookupDetPowerState (2): " ++ show idx) .
  flip (lookupAbsPowerState ("lookupDetPowerState (1): " ++ show idx)) idx

lookupDetEnergyState ::
  (Ord node, Show d, Show node, Show b) =>
  StateIdx.Energy node -> StateEnv.Complete node b (Result d) -> d
lookupDetEnergyState idx env =
  checkDetermined ("lookupDetEnergyState (2):\n" ++ show idx ++ "\n" ++ show env) $
  flip (lookupAbsEnergyState ("lookupDetEnergyState (1):\n" ++ show idx ++ "\n" ++ show env)) idx env

lookupEnergyStateMaybe ::
  (Ord node, Show d, Show node, Show b) =>
  StateIdx.Energy node -> StateEnv.Complete node b (Result d) -> Maybe d
lookupEnergyStateMaybe idx env =
  fmap toDet
  $ Map.lookup idx (StateEnv.energyMap $ StateEnv.signal env)
  where toDet (Determined x) = x
        toDet _ =
          error $ "lookupEnergyStateMaybe undetermined value at "
                  ++ show idx ++ "\nin\n"
                  ++ myShowList (Map.keys $ StateEnv.energyMap $ StateEnv.signal env)
