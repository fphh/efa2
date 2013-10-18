{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module EFA.Application.Simulation where

import EFA.Application.Utility (quantityTopology)

import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx

import EFA.Flow.Topology.Absolute ( (.=), (=.=) )

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(Data), Nil,(:>))

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Map (Map)
import Data.Monoid((<>))


type EtaAssignMap node = Map (Idx.StructureEdge node) (String, String)

solve :: (Node.C node,
          Eq (v a), Show (v a),
          Fractional a,
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
   let qtopo = quantityTopology topology
   in  EqSys.solve qtopo $
       givenSimulate qtopo etaAssign etaFunc powerRecord

{-
solve2 :: (Node.C node,
          Eq (v a),
          Fractional a,
          Ord a,
          Show a,
          EqArith.Constant a,
          SV.Zipper v,
          SV.Walker v,
          SV.Storage v a,
          SV.Singleton v,
          SV.Len (v a),
          SV.FromList v,
          Base.BSum a) =>
         Topo.Topology node ->
         EtaAssignMap node ->
         Map String (a -> a) ->
         Record.PowerRecord node v a ->
         FlowTopo.Section node (Result (Data Nil a))
solve2 topology etaAssign etaFunc powerRecord =
    EqSys.solveSimple $
    givenSimulate stateFlowGraph etaAssign etaFunc powerRecord
  where
    -- | Build Sequenceflow graph for simulation
    stateFlowGraph =
      FlowTopoPlain.stateGraphActualStorageEdges $
      Sequ.fromList [Topo.flowFromPlain topology]
-}




givenSimulate ::
   (Node.C node, Show a, Ord a, Arith.Constant a,
    SV.Zipper v, SV.Walker v,
    SV.Len (v a), SV.FromList v, SV.Storage v a) =>
   FlowTopo.Section node (Result v0) ->
   EtaAssignMap node ->
   Map String (a -> a) ->
   Record.PowerRecord node v a ->
   (forall s. EqSys.EquationSystemIgnore node s (Data (v :> Nil) a))

givenSimulate topo etaAssign etaFunc (Record.Record t xs) =
   (XIdx.dTime .=
     (Data $ SV.fromList $ replicate (Sig.len t) $ Arith.one))
   <> makeEtaFuncGiven topo etaAssign etaFunc
   <> Fold.fold (Map.mapWithKey f xs)
   where
     f ppos p  =  XIdx.powerFromPPos ppos .= Sig.unpack p


-- | Generate given equations using efficiency curves or functions for a specified section
makeEtaFuncGiven ::
   (Node.C node, Show a, Ord a, Arith.Constant a,
    Data.ZipWith c, Data.Storage c a) =>
   FlowTopo.Section node (Result v0) ->
   EtaAssignMap node ->
   Map String (a -> a) ->
   EqSys.EquationSystemIgnore node s (Data c a)
makeEtaFuncGiven topo etaAssign etaFunc =
   Fold.fold $
   Map.mapWithKey
      (\se (strP, strN) ->
         EqSys.variable (etaFromEdge topo se)
         =.=
         EqSys.liftF
            (Data.map (absEtaFunction strP strN etaFunc))
            (EqSys.variable $ Idx.Power se))
      etaAssign

etaFromEdge ::
   Node.C node =>
   FlowTopo.Section node v -> Idx.StructureEdge node -> Idx.Eta node
etaFromEdge topo se =
   let etaF = Idx.Eta se
       etaB = Idx.Eta $ Idx.flip se
   in  checkFoundPair etaF etaB
          (FlowTopo.lookupEta etaF topo, FlowTopo.lookupEta etaB topo)

checkFoundPair ::
   FormatValue a =>
   a -> a -> (Maybe b, Maybe b) -> a
checkFoundPair etaF etaB ee =
   case ee of
      (Just _, Nothing) -> etaF
      (Nothing, Just _) -> etaB
      (Nothing, Nothing) ->
         error $ "found neither " ++ Format.unUnicode (formatValue etaF) ++
                 " nor " ++ Format.unUnicode (formatValue etaB)
      (Just _,  Just _) ->
         error $ "found both " ++ Format.unUnicode (formatValue etaF) ++
                 " and " ++ Format.unUnicode (formatValue etaB)


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

{-
-- | Generate given equations using efficiency curves or functions for a specified section
makeEtaFuncGiven ::
   (Fractional a, Ord a, Show a, Arith.Sum a,
    Data.Apply c a ~ v, Eq v, Data.ZipWith c, Data.Storage c a, Node.C node) =>
   (Idx.Section -> EtaAssignMap node) ->
   Idx.Section ->
   Map String (a -> a) ->
   EqSys.EquationSystem node s x (Data c a)
makeEtaFuncGiven etaAssign sec etaFunc = Fold.fold $ Map.mapWithKey f (etaAssign sec)
  where f n (strP, strN, g) =
          EqSys.variable n =.= EqSys.liftF (Data.map ef) (EqSys.variable $ g n)
          where ef x = if x >= 0 then fpos x else fneg x
                fpos = maybe (err strP) id (Map.lookup strP etaFunc)
                fneg = maybe (err strN) (\h -> recip . h . negate)
                                        (Map.lookup strN etaFunc)
                err str x = error ("not defined: " ++ show str ++ " for " ++ show x)

-}
