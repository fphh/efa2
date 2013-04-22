{-# LANGUAGE TypeFamilies #-}
module EFA.Graph.CumulatedFlow where

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Record as Rec
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr

import EFA.Equation.Arithmetic ((~+))
import EFA.Equation.Result (Result(Undetermined, Determined))

import qualified Data.Map as M

import Control.Applicative (liftA2)
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)
import Data.Tuple.HT (mapPair)



data RelativeDir = WithTopoDir
                 | AgainstTopoDir deriving (Eq, Show)

type EnergyMap node a = M.Map (Idx.Energy node) a


getRelativeDir ::
  (Ord x) =>
  TD.Topology x -> Gr.DirEdge x -> RelativeDir
getRelativeDir g e =
  case Gr.edgeLabels g of
    es ->
      if M.member e es
         then WithTopoDir
         else if M.member (Gr.reverseEdge e) es
                 then AgainstTopoDir
                 else error "getTopologyDir: edge not found"

--relativeDirToFlowDir :: 


cumulatedEnergyFlow ::
  (Arith.Integrate v, Arith.Sum a, Arith.Scalar v ~ a, Arith.Constant a,
   Ord node) =>
  TD.Topology node ->
  TD.DirSequFlowGraph node ->
  Env.Complete node (Rec.Absolute (Result a)) (Rec.Absolute (Result v)) ->
  ( EnergyMap node (Rec.Absolute (Result a)),
    EnergyMap node (Rec.Absolute (Result a)) )
cumulatedEnergyFlow topo seqTopo env =
   mapPair (cum, cum) $ unzip $ mapMaybe f $ Gr.edges seqTopo
  where cum = M.unionsWith (liftA2 (liftA2 (~+)))
        em = Env.energyMap $ Env.signal env
        f e =
          case TD.edgeType e of
             TD.StructureEdge (Idx.InSection sec (Gr.DirEdge n n')) ->
                let idx1 = Idx.Energy (Idx.StructureEdge n n')
                    idx2 = Idx.Energy (Idx.StructureEdge n' n)

                    transfer idx =
                       M.singleton idx $
                       maybe
                          (Rec.Absolute Undetermined)
                          (fmap $ fmap Arith.integrate) $
                       M.lookup (Idx.InSection sec idx) em

                    insert =
                       transfer idx1 <> transfer idx2

                    zero = Rec.Absolute $ Determined Arith.zero
                    insertzero =
                       M.singleton idx1 zero <>
                       M.singleton idx2 zero

                in  Just $
                    case getRelativeDir topo $ Gr.DirEdge n n' of
                       WithTopoDir -> (insert, insertzero)
                       AgainstTopoDir -> (insertzero, insert)
             _ -> Nothing


cumulate ::
  (Arith.Integrate v, Arith.Sum a, Arith.Scalar v ~ a, Arith.Constant a,
   Ord node) =>
  TD.Topology node ->
  (ranges, TD.SequFlowGraph node) ->
  Env.Complete node (Rec.Absolute (Result a)) (Rec.Absolute (Result v)) ->
  ( ( TD.Topology node, EnergyMap node (Rec.Absolute (Result a)) ),
    ( TD.Topology node, EnergyMap node (Rec.Absolute (Result a)) ) )
cumulate topo (_rngs, seqTopo) env =
  ( (topo, withDirEnv), (Gr.reverse topo, againstDirEnv) )
  where (withDirEnv, againstDirEnv) =
           cumulatedEnergyFlow topo (TD.dirFromSequFlowGraph seqTopo) env
