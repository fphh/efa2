{-# LANGUAGE TemplateHaskell #-}

module EFA2.Test.SequenceTest where

import qualified EFA2.Signal.Sequence as Seq
import qualified EFA2.Signal.SequenceData as SeqData

import qualified EFA2.Signal.Signal as S

-- import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)

import qualified Data.List.HT as HTL


failing_prop_genSequ :: SeqData.ListPowerRecord -> Bool
failing_prop_genSequ prec =
   Seq.approxSequPwrRecord (1e-8)
      (snd (Seq.genSequ prec))
--      (Seq.chopAtZeroCrossingsPowerRecord prec)
      (Seq.chopAtZeroCrossingsPowerRecord prec)

prop_chopMatchingCutsApprox :: SeqData.ListPowerRecord -> Bool
prop_chopMatchingCutsApprox prec =
   case Seq.chopAtZeroCrossingsPowerRecord prec :: SeqData.SequPwrRecord of
      SeqData.SequData xs ->
         and $
         HTL.mapAdjacent
            (\(SeqData.PowerRecord xt xm)
              (SeqData.PowerRecord yt ym) ->
                fmap snd (S.viewR xt) == fmap fst (S.viewL yt)
                &&
                fmap (fmap snd . S.viewR) xm == fmap (fmap fst . S.viewL) ym)
            xs

prop_chopProjectiveApprox :: SeqData.ListPowerRecord -> Bool
prop_chopProjectiveApprox prec =
   let secs :: SeqData.SequData SeqData.ListPowerRecord
       secs = Seq.chopAtZeroCrossingsPowerRecord prec
   in  Seq.approxSequPwrRecord (1e-8)
          secs
          (Seq.chopAtZeroCrossingsPowerRecord $
           Seq.concatPowerRecords secs)

prop_chopMatchingCutsExact :: SeqData.PowerRecord [] Rational -> Bool
prop_chopMatchingCutsExact prec =
   case Seq.chopAtZeroCrossingsPowerRecord prec
         :: SeqData.SequData (SeqData.PowerRecord [] Rational) of
      SeqData.SequData xs ->
         and $
         HTL.mapAdjacent
            (\(SeqData.PowerRecord xt xm)
              (SeqData.PowerRecord yt ym) ->
                fmap snd (S.viewR xt) == fmap fst (S.viewL yt)
                &&
                fmap (fmap snd . S.viewR) xm == fmap (fmap fst . S.viewL) ym)
            xs

prop_chopProjectiveExact :: SeqData.PowerRecord [] Rational -> Bool
prop_chopProjectiveExact prec =
   let secs :: SeqData.SequData (SeqData.PowerRecord [] Rational)
       secs = Seq.chopAtZeroCrossingsPowerRecord prec
   in  secs
       ==
       (Seq.chopAtZeroCrossingsPowerRecord $
        Seq.concatPowerRecords secs)

runTests :: IO Bool
runTests = $quickCheckAll
