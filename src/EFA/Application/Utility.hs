{-# LANGUAGE TypeFamilies #-}
module EFA.Application.Utility where

import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Graph.StateFlow.Environment as StateEnv
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr

import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Data as Data

import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.System ((.=))
import EFA.Equation.Result (Result(..))

import EFA.Report.FormatValue (FormatValue)

import EFA.Utility.Map (checkedLookup)
import EFA.Utility (myShowList)

import qualified Data.Map as Map
import Data.Map (Map)

{-
makeNode :: Int -> Idx.Node
makeNode = Idx.Node

makeNodes :: [(Int, TD.NodeType)] -> [Gr.LNode Idx.Node TD.NodeType]
makeNodes ns = map f ns
  where f (n, ty) = (makeNode n, ty)
-}

makeEdges :: [(node, node)] -> [Gr.LEdge Gr.DirEdge node ()]
makeEdges = map (\(a, b) -> (Gr.DirEdge a b, ()))

{-
makeSimpleEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node ()]
makeSimpleEdges es = map f es
  where f (a, b) = (Gr.Edge (Idx.Node a) (Idx.Node b), ())
-}

-- @HT neue Utility Funktionen für Topologie-Definition, bitte prüfen
type EdgeLabel = String
type PPosLabel = String

type LabeledEdgeList node = [(node, node, EdgeLabel, PPosLabel, PPosLabel)]
type PPosLableMap node = Map (SeqIdx.PPos node) String

-- | Generate Topology with simple edge List
makeTopologySimple ::  (Ord node) => [(node,TD.NodeType ())] ->  [(node,node)] -> TD.Topology node
makeTopologySimple ns es = Gr.fromList ns (makeEdges es)

-- | Generate Topology from labeled edge List
makeTopology ::  (Ord node) => [(node,TD.NodeType ())] ->  LabeledEdgeList node -> TD.Topology node
makeTopology ns es = Gr.fromList ns (makeEdges $ map f es)
  where  f (n1,n2,_,_,_) = (n1,n2)


-- | Edge Label map used for displaying topology with labeled edges
makeEdgeNameMap :: (Ord node) => LabeledEdgeList node -> Map (node, node) String
makeEdgeNameMap edgeList = Map.fromList $ map f edgeList
  where f (x, y, lab, _, _) = ((x, y), lab)

-- | Generate Label Map for Power Positions
makePPosLabelMap :: (Ord node) => LabeledEdgeList node -> PPosLableMap node
makePPosLabelMap edgeList = Map.fromList $ concat $ map f edgeList
  where f (n1,n2,_,l1,l2) = [(SeqIdx.ppos n1 n2, l1),
                             (SeqIdx.ppos n2 n1, l2)]

constructSeqTopo ::
   (Ord node, Show node) =>
   TD.Topology node -> [Int] -> Flow.RangeGraph node
constructSeqTopo topo =
  Flow.sequenceGraph .
  fmap (StateAnalysis.bruteForce topo !!) .
  SD.fromList


select :: [topo] -> [Int] -> SD.SequData topo
select ts = SD.fromList . map (ts !!)


checkDetermined :: String -> Result a -> a
checkDetermined name rx =
   case rx of
      Result.Undetermined -> error $ "undetermined " ++ name
      Result.Determined x -> x


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
          error $ "lookupDetEnergyState undetermined value at "
                  ++ show idx ++ "\nin\n"
                  ++ myShowList (Map.keys $ StateEnv.energyMap $ StateEnv.signal env)
