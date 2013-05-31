{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import qualified EFA.Symbolic.Mixed as Term
import qualified EFA.Signal.SequenceData as SD
import EFA.Equation.System ((.=), (%=))
import EFA.Equation.Result (Result)
import EFA.Utility (Pointed, point)

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



checkDetermined :: String -> Result a -> a
checkDetermined name rx =
   case rx of
      Result.Undetermined -> error $ "undetermined " ++ name
      Result.Determined x -> x



type
   SignalTerm rec term node =
      Term.Signal term
         (EqRecord.Indexed rec (Var.ForNodeScalar node))
         (EqRecord.Indexed rec (Var.InSectionSignal node))

type
   ScalarTerm rec term node =
      Term.Scalar term
         (EqRecord.Indexed rec (Var.ForNodeScalar node))
         (EqRecord.Indexed rec (Var.InSectionSignal node))

type
   ScalarAtom rec term node =
      Term.ScalarAtom term
         (EqRecord.Indexed rec (Var.ForNodeScalar node))
         (EqRecord.Indexed rec (Var.InSectionSignal node))

type
   SymbolicEquationSystem rec node s term =
      EqGen.EquationSystem rec node s
         (ScalarTerm rec term node)
         (SignalTerm rec term node)


type
   VarTerm var recIdx term node =
      Term var term
         (Idx.Record recIdx (Var.ForNodeScalar node))
         (Idx.Record recIdx (Var.InSectionSignal node))

class (var ~ Variable (Term var)) => Symbol var where
   type Term var :: (* -> *) -> * -> * -> *
   type Variable term :: * -> *
   symbol ::
      Pointed term =>
      Idx.Record recIdx (var node) ->
      VarTerm var recIdx term node

instance Symbol (Idx.InSection Var.Signal) where
   type Term (Idx.InSection Var.Signal) = Term.Signal
   type Variable Term.Signal = Idx.InSection Var.Signal
   symbol = Term.Signal . point

instance Symbol (Idx.ForNode Var.Scalar) where
   type Term (Idx.ForNode Var.Scalar) = Term.Scalar
   type Variable Term.Scalar = Idx.ForNode Var.Scalar
   symbol = Term.Scalar . point . Term.ScalarVariable


givenSymbol ::
  {-
  The Eq constraint is requested by unique-logic but not really needed.
  It is not easily possible to compare the terms for equal meaning
  and it is better not to compare them at all.
  We should remove the Eq constraint as soon as unique-logic allows it.
  -}
  (t ~ VarTerm var (EqRecord.ToIndex rec) term node,
   Eq t, Arith.Sum t,
   t ~ Env.Element idx (ScalarTerm rec term node) (SignalTerm rec term node),
   EqGen.Record rec,
   Ord (idx node), Pointed term,
   Var.Type idx ~ var, Symbol var, Env.AccessMap idx) =>
  EqRecord.Indexed rec (idx node) ->
  SymbolicEquationSystem rec node s term
givenSymbol idx =
   idx .= symbol (fmap Var.index idx)


infixr 6 =<>

(=<>) ::
  (t ~ VarTerm var (EqRecord.ToIndex rec) term node,
   Eq t, Arith.Sum t,
   t ~ Env.Element idx (ScalarTerm rec term node) (SignalTerm rec term node),
   EqGen.Record rec,
   Ord (idx node), Pointed term,
   Var.Type idx ~ var, Symbol var, Env.AccessMap idx) =>
  EqRecord.Indexed rec (idx node) ->
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
