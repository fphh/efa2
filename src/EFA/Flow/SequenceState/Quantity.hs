{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.SequenceState.Quantity where

import qualified EFA.Graph.Topology.Index as Idx


data Signal = Signal
data Scalar = Scalar

class AccessPart env where
   type PartElement env a v :: *
   switchPart :: f Scalar -> f Signal -> f env

instance AccessPart Scalar where
   type PartElement Scalar a v = a
   switchPart x _ = x

instance AccessPart Signal where
   type PartElement Signal a v = v
   switchPart _ x = x


type Element idx a v = PartElement (Environment idx) a v

type family Environment (idx :: * -> *) :: *
type instance Environment (Idx.InPart part idx) = Signal
type instance Environment (Idx.ForNode idx) = Scalar
