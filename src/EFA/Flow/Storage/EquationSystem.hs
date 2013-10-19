{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Storage.EquationSystem where

import qualified EFA.Flow.Storage.Quantity as Quant
import qualified EFA.Flow.EquationSystem as EqSys
import EFA.Flow.EquationSystem (System)

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.SystemRecord (Expr, Record)
import EFA.Equation.Arithmetic (Constant)

import qualified Data.NonEmpty as NonEmpty
import Data.Foldable (foldMap)


fromInStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ Expr mode rec s x, Quant.Carry carry) =>
   rx -> [carry rx] ->
   System mode s
fromInStorages stoutsum outs =
   splitFactors stoutsum Quant.carryEnergy Quant.carryXOut outs

fromOutStorages ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ Expr mode rec s x, Quant.Carry carry) =>
   rx -> [carry rx] ->
   System mode s
fromOutStorages stinsum ins =
   splitFactors stinsum Quant.carryEnergy Quant.carryXIn ins

splitFactors ::
   (Verify.LocalVar mode x, Constant x, Record rec,
    rx ~ Expr mode rec s x) =>
   rx ->
   (carry rx -> rx) ->
   (carry rx -> rx) ->
   [carry rx] ->
   System mode s
splitFactors varsum energy xfactor =
   foldMap (EqSys.splitFactors varsum energy Arith.one xfactor)
   .
   NonEmpty.fetch
