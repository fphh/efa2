{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.SequenceState.Quantity where

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Part.Map as PartMap

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph

import qualified EFA.Utility.Map as MapU

import qualified Data.Map as Map; import Data.Map (Map)


data Signal = Signal
data Scalar = Scalar

class Type typ where
   type ChooseElement typ a v :: *
   switchPart :: f Scalar -> f Signal -> f typ

instance Type Scalar where
   type ChooseElement Scalar a v = a
   switchPart x _ = x

instance Type Signal where
   type ChooseElement Signal a v = v
   switchPart _ x = x


type Element idx a v = ChooseElement (TypeOf idx) a v

type family TypeOf (idx :: * -> *) :: *
type instance TypeOf (Idx.InPart part idx) = Signal
type instance TypeOf (Idx.ForStorage idx) = Scalar



storageSequences ::
   (Ord part, Node.C node) =>
   [(part, FlowTopo.Topology node v)] ->
   Map node (Map part (FlowTopo.Sums v))
storageSequences =
   Map.unionsWith (Map.unionWith (error "duplicate section for node"))
   .
   map (\(s, topo) -> fmap (Map.singleton s) $ storageNodeLabels topo)


storageSums ::
   (Ord part, Node.C node) =>
   Map node (Storage.Graph part a carryLabel) ->
   Map part (FlowTopo.Section node v) ->
   Map node (Map part (FlowTopo.Sums (a,v)))
storageSums storages sections =
   MapU.checkedZipWith "storageSums.nodes"
      (MapU.checkedZipWith "storageSums.parts"
          (\carrySum -> fmap ((,) carrySum)))
      (fmap (PartMap.parts . Storage.nodes) storages)
      (MapU.flip $ fmap (storageNodeLabels . FlowTopo.topology) sections)

storageNodeLabels ::
   (Node.C node) =>
   Graph.Graph node Graph.EitherEdge nl el -> Map node nl
storageNodeLabels =
   Map.filterWithKey (const . Node.isStorage . Node.typ) . Graph.nodeLabels
