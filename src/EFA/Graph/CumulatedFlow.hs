-- {-# LANGUAGE FlexibleContexts #-}


module EFA.Graph.CumulatedFlow where

import qualified Data.Map as M

import Data.Monoid (mempty, Monoid, (<>))
import Control.Applicative (liftA2)

import qualified EFA.Equation.Environment as Env
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import qualified EFA.Equation.Record as Rec

import EFA.Equation.Result (Result(..))


filt :: 
  (Ord node, Eq node, Show node) =>
  Gr.Graph (Idx.BndNode node) nl el ->
  Gr.Graph (Idx.BndNode node) nl el
filt = Gr.lefilter (TD.isStructureEdge . fst)


data RelativeDir = WithTopoDir
                 | AgainstTopoDir deriving (Eq, Show)


getRelativeDir ::
  (Ord x) =>
  TD.Topology x -> Gr.Edge x -> RelativeDir
getRelativeDir (Gr.Graph _ es) e =
  if M.member e es
     then WithTopoDir
     else if M.member (Gr.reverseEdge e) es
             then AgainstTopoDir
             else error "getTopologyDir: edge not found"

--relativeDirToFlowDir :: 


-- Are edges in SequFlowGraph always Dir?
cumulatedEnergyFlow ::
  (Num a, Ord node, Show node) =>
  TD.Topology node ->
  Gr.Graph (Idx.BndNode node) t TD.FlowDirection ->
  Env.Complete node b (Rec.Absolute (Result a)) ->
  ( M.Map (Idx.Energy node) (Rec.Absolute (Result a)),
    M.Map (Idx.Energy node) (Rec.Absolute (Result a)) )
cumulatedEnergyFlow topo seqTopo env =
  (M.mapKeys toEn $ M.map toDet m1, M.mapKeys toEn $ M.map toDet m2)
  where em = Env.energyMap $ Env.signal env
        Gr.Graph _ es = filt seqTopo
        (m1, m2) = M.foldWithKey f (M.empty, M.empty) es
        f (Gr.Edge (Idx.BndNode (Idx.AfterSection sec) n)
                   (Idx.BndNode _ n')) TD.Dir (withDir, againstDir) =
          case getRelativeDir topo e of
               WithTopoDir -> (insert withDir, insertzero againstDir)
               AgainstTopoDir -> (insertzero withDir, insert againstDir)
          where e  = Gr.Edge n n'

                e1 = Idx.Energy (Idx.StructureEdge sec n n')
                idx1 = (n, n')

                e2 = Idx.Energy (Idx.StructureEdge sec n' n)
                idx2 = (n', n)

                insert = M.insertWith (.+) idx1 (M.lookup e1 em)
                         . M.insertWith (.+) idx2 (M.lookup e2 em)

                insertzero = M.insertWith (.+) idx1 zero
                             . M.insertWith (.+) idx2 zero
                zero = Just (Rec.Absolute (Determined 0))

                (.+) = liftA2 (liftA2 (liftA2 (+)))

        f _ _ _ = error "not a Dir edge!"

        toDet Nothing = Rec.Absolute Undetermined
        toDet (Just a) = a

        toEn (a, b) =
          Idx.Energy (Idx.StructureEdge (Idx.Section 0) a b)

reverseGraph :: (Ord a) => Gr.Graph a b c -> Gr.Graph a b c
reverseGraph (Gr.Graph ns es) = Gr.Graph ns' es'
  where ns' = fmap (\(ins, n, outs) -> (outs, n, ins)) ns
        es' = M.mapKeys (\(Gr.Edge x y) -> (Gr.Edge y x)) es


getStorages :: (Ord node) => TD.Topology node -> [node]
getStorages = M.keys . M.filter p . Gr.nodes
  where p (_, TD.Storage _, _) = True
        p _ = False

cumulatedEnv :: 
  (Ord node) =>
  TD.Topology node ->
  Env.EnergyMap node (Rec.Absolute (Result a))
  -> ( TD.SequFlowGraph node,
       Env.Complete node (Rec.Absolute (Result a1)) (Rec.Absolute (Result a)))
cumulatedEnv topo enEnv = (ctopo, env)
  where env1 =
          mempty {
            Env.signal = mempty { 
            Env.energyMap = enEnv,
            Env.dtimeMap =
              M.fromList [ (Idx.DTime (Idx.Section 0),
                            Rec.Absolute Undetermined) ] }}
        env2 =
          mempty {  -- why cant we put it directly in env1 ? -> type problem
            Env.scalar = mempty {
            Env.storageMap = M.fromList sm }}

        sec = Idx.AfterSection (Idx.Section 0)
        sm = map f (getStorages topo)
        f s = ( Idx.Storage (Idx.BndNode sec s),
                Rec.Absolute Undetermined )
        env = env1 <> env2
        ctopo = TD.fromTopology $
                  TD.classifyStorages $
                  Gr.emap (const TD.Dir) topo


cumulate ::
  (Num a, Ord node, Show node) =>
  TD.Topology node ->
  (t1, Gr.Graph (Idx.BndNode node) t TD.FlowDirection) ->
  Env.Complete node b (Rec.Absolute (Result a)) ->
  ( ( (t1, TD.SequFlowGraph node),
       Env.Complete node (Rec.Absolute (Result a1)) (Rec.Absolute (Result a))),
    ( (t1, TD.SequFlowGraph node),
      Env.Complete node (Rec.Absolute (Result a2)) (Rec.Absolute (Result a))))
cumulate topo (rngs, seqTopo) env =
  ( ((rngs, ctopo), withDirEnv), ((rngs, crevTopo), againstDirEnv) )
  where revTopo = reverseGraph topo
        (withDir, againstDir) = cumulatedEnergyFlow topo seqTopo env
        (ctopo, withDirEnv) = cumulatedEnv topo withDir
        (crevTopo, againstDirEnv) = cumulatedEnv revTopo againstDir
