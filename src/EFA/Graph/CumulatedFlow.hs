{-# LANGUAGE TypeFamilies #-}
module EFA.Graph.CumulatedFlow where

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import EFA.Equation.Arithmetic ((~+))
import EFA.Equation.Result (Result(Undetermined, Determined))

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Applicative (liftA2)
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)
import Data.Tuple.HT (mapPair)



data RelativeDir = WithTopoDir
                 | AgainstTopoDir deriving (Eq, Show)

type EnergyMap node a = Map (Idx.Energy node) a


getRelativeDir ::
  (Ord x) =>
  Topo.Topology x -> Graph.DirEdge x -> RelativeDir
getRelativeDir g e =
  case Graph.edgeLabels g of
    es ->
      if Map.member e es
         then WithTopoDir
         else if Map.member (Graph.reverseEdge e) es
                 then AgainstTopoDir
                 else error "getTopologyDir: edge not found"

--relativeDirToFlowDir ::


cumulatedEnergyFlow ::
  (Arith.Integrate v, Arith.Sum a, Arith.Scalar v ~ a, Arith.Constant a,
   Ord node) =>
  Topo.Topology node ->
  Topo.DirSeqFlowGraph node ->
  Env.Complete node (Result a) (Result v) ->
  ( EnergyMap node (Result a), EnergyMap node (Result a) )
cumulatedEnergyFlow topo seqTopo env =
   mapPair (cum, cum) $ unzip $ mapMaybe f $ Graph.edges seqTopo
  where cum = Map.unionsWith (liftA2 (~+))
        em = Env.energyMap $ Env.signal env
        f e =
          case Topo.edgeType e of
             Topo.StructureEdge (Idx.InPart sec de) ->
                let se = Topo.structureEdgeFromDirEdge de
                    idx1 = Idx.Energy se
                    idx2 = Idx.Energy $ Idx.flip se

                    transfer idx =
                       Map.singleton idx $
                       maybe Undetermined (fmap Arith.integrate) $
                       Map.lookup (Idx.InPart sec idx) em

                    insert =
                       transfer idx1 <> transfer idx2

                    zero = Determined Arith.zero
                    insertzero =
                       Map.singleton idx1 zero <>
                       Map.singleton idx2 zero

                in  Just $
                    case getRelativeDir topo de of
                       WithTopoDir -> (insert, insertzero)
                       AgainstTopoDir -> (insertzero, insert)
             _ -> Nothing


cumulate ::
  (Arith.Integrate v, Arith.Sum a, Arith.Scalar v ~ a, Arith.Constant a,
   Ord node) =>
  Topo.Topology node ->
  (ranges, Topo.SeqFlowGraph node) ->
  Env.Complete node (Result a) (Result v) ->
  ( ( Topo.Topology node, EnergyMap node (Result a) ),
    ( Topo.Topology node, EnergyMap node (Result a) ) )
cumulate topo (_rngs, seqTopo) env =
  ( (topo, withDirEnv), (Graph.reverse topo, againstDirEnv) )
  where (withDirEnv, againstDirEnv) =
           cumulatedEnergyFlow topo (Topo.dirFromFlowGraph seqTopo) env
