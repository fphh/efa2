{-# LANGUAGE TemplateHaskell #-}

module EFA.Test.Sequence where

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Chop as Chop
import qualified EFA.Signal.Signal as Signal
import EFA.Signal.Record (PowerRecord, Record(Record))

import qualified EFA.Graph.Topology.Node as Node

import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT

-- import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)

type Node = Node.Int


failing_prop_genSequ :: PowerRecord Node [] Double -> Bool
failing_prop_genSequ prec =
   Chop.approxSequPwrRecord (1e-8)
      (Chop.genSequ prec)
--      (Chop.chopAtZeroCrossingsPowerRecord prec)
      (Chop.chopAtZeroCrossingsPowerRecord False prec)


prop_chopMatchingCutsApprox ::
   Bool -> PowerRecord Node [] Double -> Bool
prop_chopMatchingCutsApprox duplicates prec =
   eqAdjacent
      (\(Record xt xm)
        (Record yt ym) ->
          fmap snd (Signal.viewR xt) == fmap fst (Signal.viewL yt)
          &&
          fmap (fmap snd . Signal.viewR) xm == fmap (fmap fst . Signal.viewL) ym)
      (Chop.chopAtZeroCrossingsPowerRecord duplicates prec
         :: Sequ.List (PowerRecord Node [] Double))


prop_chopProjectiveApprox ::
   PowerRecord Node [] Double -> Bool
prop_chopProjectiveApprox prec =
   let secs :: Sequ.List (PowerRecord Node [] Double)
       secs = Chop.chopAtZeroCrossingsPowerRecord False prec
   in  Chop.approxSequPwrRecord (1e-8)
          secs
          (Chop.chopAtZeroCrossingsPowerRecord False $
           Chop.concatPowerRecords secs)

prop_chopMatchingCutsExact ::
   Bool -> PowerRecord Node [] Rational -> Bool
prop_chopMatchingCutsExact duplicates prec =
   eqAdjacent
      (\(Record xt xm)
        (Record yt ym) ->
          fmap snd (Signal.viewR xt) == fmap fst (Signal.viewL yt)
          &&
          fmap (fmap snd . Signal.viewR) xm == fmap (fmap fst . Signal.viewL) ym)
      (Chop.chopAtZeroCrossingsPowerRecord duplicates prec
         :: Sequ.List (PowerRecord Node [] Rational))

prop_chopProjectiveExact ::
   PowerRecord Node [] Rational -> Bool
prop_chopProjectiveExact prec =
   let secs :: Sequ.List (PowerRecord Node [] Rational)
       secs = Chop.chopAtZeroCrossingsPowerRecord False prec
   in  secs
       ==
       (Chop.chopAtZeroCrossingsPowerRecord False $
        Chop.concatPowerRecords secs)


eqAdjacent :: (a -> a -> Bool) -> Sequ.List a -> Bool
eqAdjacent f =
   and . ListHT.mapAdjacent f . Fold.toList

runTests :: IO Bool
runTests = $quickCheckAll
