{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.SequenceState.Quantity where

import qualified EFA.Graph.Topology.Index as Idx


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
type instance TypeOf (Idx.ForNode idx) = Scalar
