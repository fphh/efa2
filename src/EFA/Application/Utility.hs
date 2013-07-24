{-# LANGUAGE TypeFamilies #-}
module EFA.Application.Utility (
   module EFA.Application.Utility,
   (.=), (%=), (=%%=),
   Verify.Ignore,
   ) where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
--import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr

--import qualified EFA.Graph.Topology.Index as TIdx

import qualified EFA.Signal.Data as Data
--import EFA.Signal.Data (Data(..))

import qualified EFA.Equation.Record as EqRecord
--import qualified EFA.Equation.Arithmetic as EqArith
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Signal.SequenceData as SD
import EFA.Symbolic.Variable (VarTerm, ScalarTerm, SignalTerm, Symbol, varSymbol)
import EFA.Equation.System ((.=), (%=), (=%%=))

import EFA.Equation.Result (Result)
import EFA.Utility (Pointed)

import qualified EFA.Application.Index as XIdx
-- import EFA.Application.Absolute ( (.=), (=.=) )

import EFA.Report.FormatValue (FormatValue)

import Data.Monoid ((<>))
import Data.Map (Map)
import qualified Data.Map as Map
--import qualified Data.Foldable as Fold



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

-- @ HT neue Utility Funktionen für Topologie-Definition, bitte prüfen
type EdgeLabel = String
type PPosLabel = String

type LabeledEdgeList node = [(node, node, EdgeLabel, PPosLabel, PPosLabel)]
type PPosLableMap node = Map (XIdx.PPos node) String

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
  where f (n1,n2,_,l1,l2) = [(XIdx.ppos n1 n2, l1),
                             (XIdx.ppos n2 n1, l2)]

constructSeqTopo ::
   (Ord node, Show node) =>
   TD.Topology node -> [Int] -> Flow.RangeGraph node
constructSeqTopo topo =
  Flow.mkSequenceTopology .
  fmap (StateAnalysis.bruteForce topo !!) .
  SD.fromList


select :: [topo] -> [Int] -> SD.SequData topo
select ts = SD.fromList . map (ts !!)


checkDetermined :: String -> Result a -> a
checkDetermined name rx =
   case rx of
      Result.Undetermined -> error $ "undetermined " ++ name
      Result.Determined x -> x


type
   SymbolicEquationSystem mode rec node s term =
      EqGen.EquationSystem mode rec node s
         (ScalarTerm (EqRecord.ToIndex rec) term node)
         (SignalTerm (EqRecord.ToIndex rec) term node)


givenSymbol ::
  {-
  The Eq constraint is requested by unique-logic but not really needed.
  It is not easily possible to compare the terms for equal meaning
  and it is better not to compare them at all.
  We should remove the Eq constraint as soon as unique-logic allows it.
  -}
  (Verify.GlobalVar mode t recIdx var node,
   t ~ VarTerm var recIdx term node,
   Eq t, Arith.Sum t,
   t ~ Env.Element idx (ScalarTerm recIdx term node) (SignalTerm recIdx term node),
   EqGen.Record rec, recIdx ~ EqRecord.ToIndex rec,
   Ord (idx node), FormatValue (idx node), Pointed term,
   Var.Type idx ~ var, Symbol var, Env.AccessMap idx) =>
  Idx.Record recIdx (idx node) ->
  SymbolicEquationSystem mode rec node s term
givenSymbol idx =
   idx .= varSymbol idx


infixr 6 =<>

(=<>) ::
  (Verify.GlobalVar mode t recIdx var node,
   t ~ VarTerm var recIdx term node,
   Eq t, Arith.Sum t,
   t ~ Env.Element idx (ScalarTerm recIdx term node) (SignalTerm recIdx term node),
   EqGen.Record rec, recIdx ~ EqRecord.ToIndex rec,
   Ord (idx node), FormatValue (idx node), Pointed term,
   Var.Type idx ~ var, Symbol var, Env.AccessMap idx) =>
  Idx.Record recIdx (idx node) ->
  SymbolicEquationSystem mode rec node s term ->
  SymbolicEquationSystem mode rec node s term
idx =<> eqsys = givenSymbol idx <> eqsys


infix 0 #=, ~=

-- | @(.=)@ restricted to signals
(~=) ::
  (Verify.GlobalVar mode v recIdx var node, Arith.Sum v,
   var ~ Var.InSectionSignal, var ~ Var.Type idx,
   recIdx ~ EqRecord.ToIndex rec, EqGen.Record rec,
   Env.AccessMap idx, Env.Environment idx ~ Env.Signal,
   Ord (idx node), FormatValue (idx node)) =>
  Idx.Record recIdx (idx node) -> v ->
  EqGen.EquationSystem mode rec node s a v
(~=)  =  (.=)

-- | @(.=)@ restricted to scalars
(#=) ::
  (Verify.GlobalVar mode a recIdx var node, Arith.Sum a,
   var ~ Var.ForNodeScalar, var ~ Var.Type idx,
   recIdx ~ EqRecord.ToIndex rec, EqGen.Record rec,
   Env.AccessMap idx, Env.Environment idx ~ Env.Scalar,
   Ord (idx node), FormatValue (idx node)) =>
  Idx.Record recIdx (idx node) -> a ->
  EqGen.EquationSystem mode rec node s a v
(#=)  =  (.=)

-- | Unpack scalar result values in env from Data constructor
envGetData ::
  (Ord node) =>
  Env.Complete node (Result (Data.Data va a)) (Result (Data.Data vv v)) ->
  Env.Complete node (Result (Data.Apply va a)) (Result (Data.Apply vv v))
envGetData =
  Env.completeFMap (fmap Data.getData) (fmap Data.getData)



