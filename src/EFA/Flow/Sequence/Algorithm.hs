{-# LANGUAGE FlexibleContexts #-}

module EFA.Flow.Sequence.Algorithm where

import qualified EFA.Flow.Topology.Variable as TopoVar

import qualified EFA.Flow.Sequence.Absolute as SeqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqQty

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Graph.Topology.Node as Node


import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Undetermined))

import Data.Monoid (mempty)

-- | Accumulate energy per section and resolve for eta and x-factors
accumulate ::
  ( Arith.ZeroTestable (Arith.Scalar (Arith.Scalar a)),
    Arith.ZeroTestable (Arith.Scalar a),
    Arith.Product (Arith.Scalar a), Arith.Integrate (Arith.Scalar a),
    Arith.Integrate a, Arith.Constant (Arith.Scalar (Arith.Scalar a)),
    Node.C node) =>
  SeqQty.Graph node (Result (Arith.Scalar (Arith.Scalar a))) (Result a) ->
  SeqQty.Graph node (Result (Arith.Scalar (Arith.Scalar a))) (Result (Arith.Scalar a))
accumulate sfg =
  SeqAbs.solve (SeqQty.mapGraphWithVar (const id) integr sfg) mempty
  where integr (Idx.InPart _ var) v = 
          fmap Arith.integrate $
            case var of 
                 TopoVar.Energy _ -> v
                 TopoVar.DTime _ -> v
                 _ -> Undetermined
