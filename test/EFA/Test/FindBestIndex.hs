{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module EFA.Test.FindBestIndex where


import qualified EFA.Application.DoubleSweep as DoubleSweep
import qualified EFA.Application.Sweep as Sweep
import EFA.Application.Sweep (Sweep)

import qualified EFA.Equation.Arithmetic as Arith

import qualified Data.Vector.Unboxed as UV

import qualified Test.QuickCheck as QC
import Test.QuickCheck ((==>), (.&&.))

import Test.QuickCheck.All (quickCheckAll)

import qualified EFA.TestUtility as Test

import Data.Tuple.HT (fst3, snd3)
import Data.Ord (comparing)
import qualified Data.List as List

import Control.Monad (void)



etasys :: Sweep UV.Vector Double
etasys = Sweep.fromList [1, 2, 3]

obj0 :: Sweep UV.Vector Double
obj0 = Sweep.fromList [5, 1, 4]

obj1 :: Sweep UV.Vector Double
obj1 = Sweep.fromList [-5, -1, -4]

cond0 :: Sweep UV.Vector Bool
cond0 = Sweep.fromList [True, False, True]

cond1 :: Sweep UV.Vector Bool
cond1 = Sweep.fromList [True, True, True]

cond2 :: Sweep UV.Vector Bool
cond2 = Sweep.fromList [False, False, False]


test1, test2, test3 :: Bool
test1 = DoubleSweep.findBestIndex cond0 obj0 etasys == Just (0,5.0,1.0)
test2 = DoubleSweep.findBestIndex cond1 obj0 etasys == Just (0,5.0,1.0)
test3 = DoubleSweep.findBestIndex cond2 obj0 etasys == Nothing

test4, test5, test6 :: Bool
test4 = DoubleSweep.findBestIndex cond0 obj1 etasys == Just (2,-4.0,3.0)
test5 = DoubleSweep.findBestIndex cond1 obj1 etasys == Just (1,-1.0,2.0)
test6 = DoubleSweep.findBestIndex cond2 obj1 etasys == Nothing



emptyEsys :: Sweep UV.Vector Double
emptyEsys = Sweep.fromList []

emptyObj :: Sweep UV.Vector Double
emptyObj = Sweep.fromList []

emptyCond :: Sweep UV.Vector Double
emptyCond = Sweep.fromList []

test7 :: Bool
test7 = DoubleSweep.findBestIndex cond2 obj1 etasys == Nothing




orGen :: QC.Gen a -> QC.Gen a -> QC.Gen a
orGen g1 g2 = do
  x <- QC.choose (0, 1 :: Int)
  case x of
       0 -> g1
       1 -> g2
       _ -> error "EFA.Test.FindBestIndex.orGen"

arbitraryOrNan ::
  (Arith.Constant a, Arith.Product a, QC.Arbitrary a) => QC.Gen a
arbitraryOrNan = 
  let nan = Arith.zero Arith.~/ Arith.zero
  in QC.arbitrary `orGen` return nan

len :: Int
len = 200


newtype BoolSweep =
  BoolSweep { unBoolSweep :: Sweep UV.Vector Bool} deriving (Show)

instance QC.Arbitrary BoolSweep where
           arbitrary =
             QC.vectorOf len QC.arbitrary
               >>= return . BoolSweep . Sweep.fromList

data PairOfSweeps =
  PairOfSweeps (Sweep UV.Vector Double) (Sweep UV.Vector Double)
  deriving (Show)

instance QC.Arbitrary PairOfSweeps where
           arbitrary = do
             es <- QC.vectorOf len arbitraryOrNan
             os <- mapM (\x -> if isNaN x then return x else arbitraryOrNan) es
             return $ PairOfSweeps (Sweep.fromList os) (Sweep.fromList es)


shortLen :: Int
shortLen = 3

newtype ShortSweep a =
  ShortSweep { unShortSweep :: Sweep UV.Vector a } deriving (Show)

instance (QC.Arbitrary a, UV.Unbox a) =>
         QC.Arbitrary (ShortSweep a) where
           arbitrary =
             QC.vectorOf shortLen QC.arbitrary
               >>= return . ShortSweep . Sweep.fromList


prop_check_pairOfSweeps ::
  PairOfSweeps ->
  Bool
prop_check_pairOfSweeps (PairOfSweeps obj esys) =
  List.foldl1' (&&)
  $ zipWith (\e o -> not (isNaN e) || isNaN o) (Sweep.toList esys) (Sweep.toList obj)

testProperty ::
  (RealFloat a, UV.Unbox a, Arith.Constant a,
   Sweep.SweepClass sweep UV.Vector a,
   Sweep.SweepClass sweep UV.Vector Bool) =>
  sweep UV.Vector Bool ->
  sweep UV.Vector a ->
  sweep UV.Vector a ->
  ((Int, a, a) -> Bool) ->
  Bool
testProperty cond obj esys p =
  maybe True p $ DoubleSweep.findBestIndex cond obj esys

prop_is_condition_true ::
  BoolSweep ->
  PairOfSweeps ->
  Bool
prop_is_condition_true (BoolSweep cond) (PairOfSweeps obj esys) =
  testProperty cond obj esys $
    \(idx, _, _) -> c UV.! idx == True
  where c = Sweep.unSweep cond


prop_found_max ::
  BoolSweep ->
  PairOfSweeps ->
  Bool
prop_found_max (BoolSweep cond) (PairOfSweeps obj esys) =
  testProperty cond obj esys $
    \(_, ov, _) ->
         ov == ( UV.maximum
                 $ UV.filter (not . isNaN)
                 $ UV.map snd3
                 $ UV.filter fst3
                 $ UV.zipWith3 (,,) c o e )
  where c = Sweep.unSweep cond
        o = Sweep.unSweep obj
        e = Sweep.unSweep esys


prop_is_eta_associated_with_max ::
  BoolSweep ->
  PairOfSweeps ->
  Bool
prop_is_eta_associated_with_max (BoolSweep cond) (PairOfSweeps obj esys) =
  testProperty cond obj esys $
    \(idx, _, x) -> x == e UV.! idx
  where e = Sweep.unSweep esys


prop_no_valid_nothing ::
  ShortSweep Bool ->
  ShortSweep Double ->
  ShortSweep Double ->
  QC.Property
prop_no_valid_nothing cond obj esys =
  (allFalse ==> fbiIsNothing)
  .&&.
  (fbiIsNothing ==> allFalse)
  where allFalse = UV.all (==False) $ Sweep.unSweep c
        fbiIsNothing = findBestIndex_reference c o e == Nothing

        c = unShortSweep cond
        o = unShortSweep obj
        e = unShortSweep esys



findBestIndex_reference ::
  (Ord a, Arith.Constant a, UV.Unbox a,RealFloat a,
   Sweep.SweepVector UV.Vector a,
   Sweep.SweepClass sweep UV.Vector a,
   Sweep.SweepVector UV.Vector Bool,
   Sweep.SweepClass sweep UV.Vector Bool) =>
  (sweep UV.Vector Bool) ->
  (sweep UV.Vector a) ->
  (sweep UV.Vector a) ->
  Maybe (Int, a, a)

findBestIndex_reference cond objVal esys =
  if UV.null fv
     then Nothing
     else Just (idx, o, e)
  where
        cs = Sweep.fromSweep cond
        es = Sweep.fromSweep esys
        os = Sweep.fromSweep objVal


        idxList = UV.fromList [ 0.. UV.length cs - 1 ]
        
        fv = UV.filter (\(_, c1, o1, _) -> c1 && (not $ isNaN o1))
             $ UV.zipWith4 (,,,) idxList cs os es

        maxIdx = UV.maxIndexBy (comparing (\ (_,_,o2,_) -> o2)) fv
        (idx,_, o, e) = fv UV.! maxIdx



prop_reference ::
  BoolSweep ->
  PairOfSweeps ->
  Bool
prop_reference (BoolSweep cond) (PairOfSweeps obj esys) =
  findBestIndex_reference cond obj esys == DoubleSweep.findBestIndex cond obj esys



runTests :: IO Bool
runTests = do
  Test.single "FindBestIndex.test1" test1
  Test.single "FindBestIndex.test2" test2
  Test.single "FindBestIndex.test3" test3
  Test.single "FindBestIndex.test4" test4
  Test.single "FindBestIndex.test5" test5
  Test.single "FindBestIndex.test6" test6

  $quickCheckAll


main :: IO ()
main = void runTests
