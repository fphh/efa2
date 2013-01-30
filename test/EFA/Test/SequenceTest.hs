{-# LANGUAGE TemplateHaskell #-}

module EFA.Test.SequenceTest where

import EFA.Signal.SequenceData (SequData(..))
import qualified EFA.Signal.Sequence as Seq
-- import qualified EFA.Signal.SequenceData as SeqData

import qualified EFA.Signal.Signal as S
import EFA.Signal.Record (PowerRecord(..))
import EFA.Signal.Base (Val)

-- import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)

import qualified Data.List.HT as HTL


failing_prop_genSequ :: PowerRecord [] Val -> Bool
failing_prop_genSequ prec =
   Seq.approxSequPwrRecord (1e-8)
      (snd (Seq.genSequ prec))
--      (Seq.chopAtZeroCrossingsPowerRecord prec)
      (Seq.chopAtZeroCrossingsPowerRecord prec)

prop_chopMatchingCutsApprox :: PowerRecord [] Val -> Bool
prop_chopMatchingCutsApprox prec =
   case Seq.chopAtZeroCrossingsPowerRecord prec :: SequData (PowerRecord [] Val) of
      SequData xs ->
         and $
         HTL.mapAdjacent
            (\(PowerRecord xt xm)
              (PowerRecord yt ym) ->
                fmap snd (S.viewR xt) == fmap fst (S.viewL yt)
                &&
                fmap (fmap snd . S.viewR) xm == fmap (fmap fst . S.viewL) ym)
            xs

prop_chopProjectiveApprox :: PowerRecord [] Val -> Bool
prop_chopProjectiveApprox prec =
   let secs :: SequData (PowerRecord [] Val)
       secs = Seq.chopAtZeroCrossingsPowerRecord prec
   in  Seq.approxSequPwrRecord (1e-8)
          secs
          (Seq.chopAtZeroCrossingsPowerRecord $
           Seq.concatPowerRecords secs)

prop_chopMatchingCutsExact :: PowerRecord [] Rational -> Bool
prop_chopMatchingCutsExact prec =
   case Seq.chopAtZeroCrossingsPowerRecord prec
         :: SequData (PowerRecord [] Rational) of
      SequData xs ->
         and $
         HTL.mapAdjacent
            (\(PowerRecord xt xm)
              (PowerRecord yt ym) ->
                fmap snd (S.viewR xt) == fmap fst (S.viewL yt)
                &&
                fmap (fmap snd . S.viewR) xm == fmap (fmap fst . S.viewL) ym)
            xs

prop_chopProjectiveExact :: PowerRecord [] Rational -> Bool
prop_chopProjectiveExact prec =
   let secs :: SequData (PowerRecord [] Rational)
       secs = Seq.chopAtZeroCrossingsPowerRecord prec
   in  secs
       ==
       (Seq.chopAtZeroCrossingsPowerRecord $
        Seq.concatPowerRecords secs)

runTests :: IO Bool
runTests = $quickCheckAll
