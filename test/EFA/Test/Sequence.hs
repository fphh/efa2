{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EFA.Test.Sequence where

import EFA.Signal.SequenceData (SequData(..))
import qualified EFA.Signal.Sequence as Seq
-- import qualified EFA.Signal.SequenceData as SeqData

import qualified EFA.Signal.Signal as S
import EFA.Signal.Record (PowerRecord, Record (..))
import EFA.Signal.Base (Val)

import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT

-- import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)


failing_prop_genSequ :: (Ord node) => PowerRecord node [] Val -> Bool
failing_prop_genSequ prec =
   Seq.approxSequPwrRecord (1e-8)
      (snd (Seq.genSequ prec))
--      (Seq.chopAtZeroCrossingsPowerRecord prec)
      (Seq.chopAtZeroCrossingsPowerRecord prec)


prop_chopMatchingCutsApprox ::
  forall node. (Ord node) =>
  PowerRecord node [] Val -> Bool
prop_chopMatchingCutsApprox prec =
   case Seq.chopAtZeroCrossingsPowerRecord prec :: SequData (PowerRecord node [] Val) of
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
   let secs :: SequData (PowerRecord node [] Val)
       secs = Seq.chopAtZeroCrossingsPowerRecord prec
   in  Seq.approxSequPwrRecord (1e-8)
          secs
          (Seq.chopAtZeroCrossingsPowerRecord $
           Seq.concatPowerRecords secs)

prop_chopMatchingCutsExact ::
  forall node. (Ord node) => PowerRecord node [] Rational -> Bool
prop_chopMatchingCutsExact prec =
   case Seq.chopAtZeroCrossingsPowerRecord prec
         :: SequData (PowerRecord node [] Rational) of
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
   let secs :: SequData (PowerRecord node [] Rational)
       secs = Seq.chopAtZeroCrossingsPowerRecord prec
   in  secs
       ==
       (Seq.chopAtZeroCrossingsPowerRecord $
        Seq.concatPowerRecords secs)

eqAdjacent :: (a -> a -> Bool) -> SequData a -> Bool
eqAdjacent f =
   and . ListHT.mapAdjacent f . Fold.toList

runTests :: IO Bool
runTests = $quickCheckAll
