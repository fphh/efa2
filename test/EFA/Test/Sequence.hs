{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EFA.Test.Sequence where

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Chop as Chop
import qualified EFA.Signal.Signal as S
import EFA.Signal.Record (PowerRecord, Record(Record))
import EFA.Signal.Base (Val)

import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT

-- import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)


failing_prop_genSequ :: (Ord node) => PowerRecord node [] Val -> Bool
failing_prop_genSequ prec =
   Chop.approxSequPwrRecord (1e-8)
      (Chop.genSequ prec)
--      (Chop.chopAtZeroCrossingsPowerRecord prec)
      (Chop.chopAtZeroCrossingsPowerRecord prec)


prop_chopMatchingCutsApprox ::
  forall node. (Ord node) =>
  PowerRecord node [] Val -> Bool
prop_chopMatchingCutsApprox prec =
   case Chop.chopAtZeroCrossingsPowerRecord prec :: Sequ.List (PowerRecord node [] Val) of
       xs ->
          eqAdjacent
             (\(Record xt xm)
               (Record yt ym) ->
                 fmap snd (S.viewR xt) == fmap fst (S.viewL yt)
                 &&
                 fmap (fmap snd . S.viewR) xm == fmap (fmap fst . S.viewL) ym)
             xs


prop_chopProjectiveApprox ::
  forall node. (Ord node) => PowerRecord node [] Val -> Bool
prop_chopProjectiveApprox prec =
   let secs :: Sequ.List (PowerRecord node [] Val)
       secs = Chop.chopAtZeroCrossingsPowerRecord prec
   in  Chop.approxSequPwrRecord (1e-8)
          secs
          (Chop.chopAtZeroCrossingsPowerRecord $
           Chop.concatPowerRecords secs)

prop_chopMatchingCutsExact ::
  forall node. (Ord node) => PowerRecord node [] Rational -> Bool
prop_chopMatchingCutsExact prec =
   case Chop.chopAtZeroCrossingsPowerRecord prec
         :: Sequ.List (PowerRecord node [] Rational) of
      xs ->
         eqAdjacent
            (\(Record xt xm)
              (Record yt ym) ->
                fmap snd (S.viewR xt) == fmap fst (S.viewL yt)
                &&
                fmap (fmap snd . S.viewR) xm == fmap (fmap fst . S.viewL) ym)
            xs

prop_chopProjectiveExact ::
  forall node. (Ord node) => PowerRecord node [] Rational -> Bool
prop_chopProjectiveExact prec =
   let secs :: Sequ.List (PowerRecord node [] Rational)
       secs = Chop.chopAtZeroCrossingsPowerRecord prec
   in  secs
       ==
       (Chop.chopAtZeroCrossingsPowerRecord $
        Chop.concatPowerRecords secs)

eqAdjacent :: (a -> a -> Bool) -> Sequ.List a -> Bool
eqAdjacent f =
   and . ListHT.mapAdjacent f . Fold.toList

runTests :: IO Bool
runTests = $quickCheckAll
