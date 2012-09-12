{-# LANGUAGE TemplateHaskell #-}

module EFA2.Test.SequenceTest where

import qualified EFA2.Signal.Sequence as Seq
import qualified EFA2.Signal.SequenceData as SeqData

import qualified EFA2.Signal.Signal as S

-- import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)

import qualified Data.List.HT as HTL


failing_prop_genSequ :: SeqData.PowerRecord -> Bool
failing_prop_genSequ prec =
   Seq.approxSequPwrRecord (1e-8)
      (snd (Seq.genSequ prec))
--      (Seq.chopAtZeroCrossingsPowerRecord prec)
      (Seq.chopAtZeroCrossingsPowerRecord prec)

prop_chopMatchingCuts :: SeqData.PowerRecord -> Bool
prop_chopMatchingCuts prec =
   case Seq.chopAtZeroCrossingsPowerRecord prec of
      SeqData.SequData xs ->
         and $
         HTL.mapAdjacent
            (\(SeqData.SecPowerRecord xt xm)
              (SeqData.SecPowerRecord yt ym) ->
                fmap snd (S.viewR xt) == fmap fst (S.viewL yt)
                &&
                fmap (fmap snd . S.viewR) xm == fmap (fmap fst . S.viewL) ym)
            xs

prop_chopProjective :: SeqData.PowerRecord -> Bool
prop_chopProjective prec =
   let secs = Seq.chopAtZeroCrossingsPowerRecord prec
   in  Seq.approxSequPwrRecord (1e-8)
          secs
          (Seq.chopAtZeroCrossingsPowerRecord $
           (\(SeqData.SecPowerRecord xt xm) ->
              SeqData.PowerRecord (S.convert xt) (fmap S.convert xm)) $
           Seq.concatPowerRecords secs)

runTests :: IO Bool
runTests = $quickCheckAll
