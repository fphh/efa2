{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.SequenceState.Quantity where

import qualified EFA.Flow.Topology.Quantity as FlowTopo

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph

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
   [(part, Graph.Graph node Graph.EitherEdge (FlowTopo.Sums v) edgeLabel)] ->
   Map node (Map part (FlowTopo.Sums v))
storageSequences =
   Map.unionsWith (Map.unionWith (error "duplicate section for node"))
   .
   map
      (\(s, topo) ->
         fmap (Map.singleton s) $
         Map.filterWithKey (const . Node.isStorage . Node.typ) $
         Graph.nodeLabels topo)
