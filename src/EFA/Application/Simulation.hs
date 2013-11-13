{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module EFA.Application.Simulation where

import EFA.Application.Utility (quantityTopology)

import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx
import EFA.Flow.Topology.Absolute ( (.=), (=.=) )

import qualified EFA.Flow.Absolute as EqAbs

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(Data), Nil,(:>))

import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Map (Map)
import Data.Monoid((<>))


type EtaAssignMap node = Map (XIdx.Position node) (String, String)

solve :: (Node.C node,
          Eq (v a), Show (v a),
          Ord a,
          Show a,
          Arith.Constant a,
          SV.Zipper v,
          SV.Walker v,
          SV.Storage v a,
          SV.Singleton v,
          SV.Len (v a),
          SV.FromList v) =>
         Topo.Topology node ->
         EtaAssignMap node ->
         Map String (a -> a) ->
         Record.PowerRecord node v a ->
         FlowTopo.Section node (Result (Data (v :> Nil) a))
solve topology etaAssign etaFunc powerRecord =
   EqSys.solve (quantityTopology topology) $
   givenSimulate etaAssign etaFunc powerRecord


givenSimulate ::
   (Node.C node, Show a, Ord a, Arith.Constant a,
    SV.Zipper v, SV.Walker v,
    SV.Len (v a), SV.FromList v, SV.Storage v a) =>
   EtaAssignMap node ->
   Map String (a -> a) ->
   Record.PowerRecord node v a ->
   (forall s. EqSys.EquationSystemIgnore node s (Data (v :> Nil) a))

givenSimulate etaAssign etaFunc (Record.Record t xs) =
   (XIdx.dTime .=
     (Data $ SV.fromList $ replicate (Sig.len t) $ Arith.one))
   <> EqSys.withExpressionGraph (makeEtaFuncGiven etaAssign etaFunc)
   <> Fold.fold (Map.mapWithKey f xs)
   where
     f ppos p  =  XIdx.powerFromPosition ppos .= Sig.unpack p


-- | Generate given equations using efficiency curves or functions for a specified section
makeEtaFuncGiven ::
   (Node.C node, Show a, Ord a, Arith.Constant a,
    Data.ZipWith c, Data.Storage c a) =>
   EtaAssignMap node ->
   Map String (a -> a) ->
   FlowTopo.Section node (EqAbs.ExpressionIgnore vars s (Data c a)) ->
   EqAbs.VariableSystemIgnore vars s
makeEtaFuncGiven etaAssign etaFunc topo =
   Fold.fold $
   Map.mapWithKey
      (\se (strP, strN) ->
         Fold.foldMap
            (\(eta, power) ->
               eta
               =.=
               EqAbs.liftF
                  (Data.map (absEtaFunction strP strN etaFunc))
                  power)
            (FlowTopo.lookupAutoDirSection
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerOut flow))
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerIn  flow))
               id se topo))
      etaAssign

absEtaFunction ::
   (Ord k, Show k, Ord a, Show a, Arith.Constant a, Arith.Product b) =>
   k -> k -> Map k (a -> b) -> a -> b
absEtaFunction strP strN etaFunc =
   let fpos = check strP id  $ Map.lookup strP etaFunc
       fneg = check strN rev $ Map.lookup strN etaFunc
       rev h = Arith.recip . h . Arith.negate
       check str =
          maybe (\x -> error ("not defined: " ++ show str ++ " for " ++ show x))
   in  \x -> if x >= Arith.zero then fpos x else fneg x
