module EFA.Graph.CumulatedFlow where

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as Rec
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr

import EFA.Equation.Result (Result(..))

import qualified Data.Map as M

import Control.Applicative (liftA2)
import Data.Monoid (Monoid, mempty, (<>))
import Data.Maybe (mapMaybe)
import Data.Tuple.HT (mapPair)



data RelativeDir = WithTopoDir
                 | AgainstTopoDir deriving (Eq, Show)


getRelativeDir ::
  (Ord x) =>
  TD.Topology x -> Gr.Edge x -> RelativeDir
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
  (Num a, Ord node, Show node) =>
  TD.Topology node ->
  TD.DirSequFlowGraph node ->
  Env.Complete node b (Rec.Absolute (Result a)) ->
  ( Env.EnergyMap node (Rec.Absolute (Result a)),
    Env.EnergyMap node (Rec.Absolute (Result a)) )
cumulatedEnergyFlow topo seqTopo env =
   mapPair (cum, cum) $ unzip $ mapMaybe f $ Gr.labEdges seqTopo
  where cum = M.unionsWith (liftA2 (liftA2 (+)))
        em = Env.energyMap $ Env.signal env
        f (e, ()) =
          case TD.edgeType e of
             TD.StructureEdge idx@(Idx.InSection _sec (Idx.StructureEdge n n')) ->
                let e1 = Idx.liftInSection Idx.Energy idx
                    idx1 = Idx.InSection (Idx.Section 0) $ Idx.Energy (Idx.StructureEdge n n')

                    e2 = Idx.liftInSection Idx.Energy (Idx.flip idx)
                    idx2 = Idx.InSection (Idx.Section 0) $ Idx.Energy (Idx.StructureEdge n' n)

                    insert =
                       (M.singleton idx1 $ toDet $ M.lookup e1 em) <>
                       (M.singleton idx2 $ toDet $ M.lookup e2 em)
                    toDet = maybe (Rec.Absolute Undetermined) id

                    insertzero =
                       M.singleton idx1 zero <>
                       M.singleton idx2 zero
                    zero = Rec.Absolute (Determined 0)

                in  Just $
                    case getRelativeDir topo $ Gr.Edge n n' of
                       WithTopoDir -> (insert, insertzero)
                       AgainstTopoDir -> (insertzero, insert)
             _ -> Nothing


cumulatedEnv ::
  (Ord node) =>
  TD.Topology node ->
  Env.EnergyMap node (Rec.Absolute (Result a)) ->
  ( TD.SequFlowGraph node,
    Env.Complete node (Rec.Absolute (Result b)) (Rec.Absolute (Result a)))
cumulatedEnv topo enEnv = (ctopo, env)
  where env =
           Env.Complete
              (mempty {
                 Env.storageMap =
                   fmap (const $ Rec.Absolute Undetermined) $
                   M.mapKeys (Idx.ForNode $ Idx.Storage bnd) $
                   M.filter TD.isStorage $ Gr.nodeLabels topo })
              (mempty {
                 Env.energyMap = enEnv,
                 Env.dtimeMap =
                   M.singleton
                     (Idx.InSection sec Idx.DTime)
                     (Rec.Absolute Undetermined)
               })

        sec = Idx.Section 0
        bnd = Idx.AfterSection sec
        ctopo =
           TD.fromTopology $
           TD.classifyStorages $
           Gr.emap (const TD.Dir) topo


cumulate ::
  (Num a, Ord node, Show node) =>
  TD.Topology node ->
  (t1, TD.SequFlowGraph node) ->
  Env.Complete node b (Rec.Absolute (Result a)) ->
  ( ( (t1, TD.SequFlowGraph node),
       Env.Complete node (Rec.Absolute (Result a1)) (Rec.Absolute (Result a))),
    ( (t1, TD.SequFlowGraph node),
      Env.Complete node (Rec.Absolute (Result a2)) (Rec.Absolute (Result a))))
cumulate topo (rngs, seqTopo) env =
  ( ((rngs, ctopo), withDirEnv), ((rngs, crevTopo), againstDirEnv) )
  where revTopo = Gr.reverse topo
        (withDir, againstDir) =
           cumulatedEnergyFlow topo (TD.dirFromSequFlowGraph seqTopo) env
        (ctopo, withDirEnv) = cumulatedEnv topo withDir
        (crevTopo, againstDirEnv) = cumulatedEnv revTopo againstDir
