{-# LANGUAGE TypeFamilies #-}
module EFA.Example.Utility (
   module EFA.Example.Utility,
   (.=), (%=),
   ) where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr

import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Signal.SequenceData as SD
import EFA.Symbolic.Variable (VarTerm, ScalarTerm, SignalTerm, Symbol, varSymbol)
import EFA.Equation.System ((.=), (%=))
import EFA.Equation.Result (Result)
import EFA.Utility (Pointed)

import Data.Monoid ((<>))



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
   SymbolicEquationSystem rec node s term =
      EqGen.EquationSystem rec node s
         (ScalarTerm (EqRecord.ToIndex rec) term node)
         (SignalTerm (EqRecord.ToIndex rec) term node)


givenSymbol ::
  {-
  The Eq constraint is requested by unique-logic but not really needed.
  It is not easily possible to compare the terms for equal meaning
  and it is better not to compare them at all.
  We should remove the Eq constraint as soon as unique-logic allows it.
  -}
  (t ~ VarTerm var recIdx term node,
   Eq t, Arith.Sum t,
   t ~ Env.Element idx (ScalarTerm recIdx term node) (SignalTerm recIdx term node),
   EqGen.Record rec, recIdx ~ EqRecord.ToIndex rec,
   Ord (idx node), Pointed term,
   Var.Type idx ~ var, Symbol var, Env.AccessMap idx) =>
  Idx.Record recIdx (idx node) ->
  SymbolicEquationSystem rec node s term
givenSymbol idx =
   idx .= varSymbol idx


infixr 6 =<>

(=<>) ::
  (t ~ VarTerm var recIdx term node,
   Eq t, Arith.Sum t,
   t ~ Env.Element idx (ScalarTerm recIdx term node) (SignalTerm recIdx term node),
   EqGen.Record rec, recIdx ~ EqRecord.ToIndex rec,
   Ord (idx node), Pointed term,
   Var.Type idx ~ var, Symbol var, Env.AccessMap idx) =>
  Idx.Record recIdx (idx node) ->
  SymbolicEquationSystem rec node s term ->
  SymbolicEquationSystem rec node s term
idx =<> eqsys = givenSymbol idx <> eqsys


infix 0 #=, ~=

-- | @(.=)@ restricted to signals
(~=) ::
  (Eq v, Arith.Sum v, EqGen.Record rec,
   Env.AccessMap idx, Env.Environment idx ~ Env.Signal, Ord (idx node)) =>
  EqRecord.Indexed rec (idx node) -> v ->
  EqGen.EquationSystem rec node s a v
(~=)  =  (.=)

-- | @(.=)@ restricted to scalars
(#=) ::
  (Eq a, Arith.Sum a, EqGen.Record rec,
   Env.AccessMap idx, Env.Environment idx ~ Env.Scalar, Ord (idx node)) =>
  EqRecord.Indexed rec (idx node) -> a ->
  EqGen.EquationSystem rec node s a v
(#=)  =  (.=)
