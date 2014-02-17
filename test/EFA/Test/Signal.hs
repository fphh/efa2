{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}


module EFA.Test.Signal where

import EFA.Signal.Signal(TC(TC),UTSignal,UTSignal2,UTSignal3,
                         interp1LinValid, interp2WingProfileValid, interp3WingProfileValid)
import qualified EFA.Signal.Signal as Signal

import EFA.Signal.Data(Data(Data),Nil)
import EFA.Signal.Typ(Typ,UT)
import qualified EFA.Signal.Interp as Interp
import EFA.Signal.Interp(ExtrapMethod(ExtrapLinear),
                         Val(Inter, Extra, Invalid), unpack,
                         ExtrapMethod(ExtrapLinear,ExtrapLinear2,
                                      ExtrapNone,ExtrapVal,ExtrapLast))


import Test.QuickCheck.All (quickCheckAll)

check :: TC Signal.Sample (Typ UT UT UT) (Data Nil (Interp.Val Double)) ->
         TC Signal.Sample (Typ UT UT UT) (Data Nil (Interp.Val Double))->
         Bool
check (TC (Data x)) (TC (Data y)) = interCheck x y

checkSig :: UTSignal [] (Interp.Val Double) ->
         UTSignal [] (Interp.Val Double) ->
         Bool
checkSig xs ys = Signal.all (==True) $ Signal.zipWith interCheck xs ys

interCheck :: (Interp.Val Double) -> (Interp.Val Double) -> Bool
interCheck Invalid Invalid = True
interCheck (Inter x) (Inter y) = valCheck x y
interCheck (Extra x) (Extra y) = valCheck x y
interCheck _ _ = False

valCheck :: Double -> Double -> Bool
valCheck x1 x2 = abs (x1 - x2) < 10^^(-12::Integer)

-- | Test selected points
prop_interp1LinValid_Simple :: Bool
prop_interp1LinValid_Simple = all (==True) tests
  where
    xSig :: UTSignal [] Double
    xSig = TC $ Data [1, 2]

    ySig :: UTSignal [] Double
    ySig = TC $ Data [1, 11]

    tests = [(interp1LinValid "Signal Test" Interp.Linear ExtrapNone xSig ySig
              (TC $ Data 1)) `check` (TC $ Data $ Inter 1),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapNone xSig ySig
              (TC $ Data 2)) `check` (TC $ Data $ Inter 11),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapNone xSig ySig
              (TC $ Data 1.2)) `check` (TC $ Data $ Inter 3),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapNone xSig ySig
              (TC $ Data 1.5)) `check` (TC $ Data $ Inter 6),
             (interp1LinValid "Signal Test" Interp.Linear2 ExtrapNone xSig ySig
              (TC $ Data 1.2)) `check` (TC $ Data $ Inter 3),
             (interp1LinValid "Signal Test" Interp.Linear2 ExtrapNone xSig ySig
              (TC $ Data 1.5)) `check` (TC $ Data $ Inter 6),
             (interp1LinValid "Signal Test" Interp.Nearest ExtrapNone xSig ySig
              (TC $ Data 1.2)) `check` (TC $ Data $ Inter 1),
             (interp1LinValid "Signal Test" Interp.Nearest ExtrapNone xSig ySig
              (TC $ Data 1.5)) `check` (TC $ Data $ Inter 11)]


prop_interp1LinValid_Extrap :: Bool
prop_interp1LinValid_Extrap = all (==True) tests
  where
    xSig :: UTSignal [] Double
    xSig = TC $ Data [1, 2]

    ySig :: UTSignal [] Double
    ySig = TC $ Data [1, 11]

    tests = [(interp1LinValid "Signal Test" Interp.Linear (ExtrapVal 99) xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Extra 99),
             (interp1LinValid "Signal Test" Interp.Linear (ExtrapVal 99) xSig ySig
              (TC $ Data 2.2)) `check` (TC $ Data $ Extra 99),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapLast xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Extra 1),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapLast xSig ySig
              (TC $ Data 2.2)) `check` (TC $ Data $ Extra 11),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapLinear xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Extra (-9)),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapLinear xSig ySig
              (TC $ Data 2.2)) `check` (TC $ Data $ Extra 13),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapLinear2 xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Extra (-9)),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapLinear2 xSig ySig
              (TC $ Data 2.2)) `check` (TC $ Data $ Extra 13)]

-- | Case of vertical segment (both x-Values are same)
prop_interp1LinValid_Vertical :: Bool
prop_interp1LinValid_Vertical = foldl (&&) True tests -- all (==True) tests
  where
    xSig :: UTSignal [] Double
    xSig = TC $ Data [1, 1, 2]

    ySig :: UTSignal [] Double
    ySig = TC $ Data [1, 2, 3]

    tests = [(interp1LinValid "Signal Test" Interp.Linear ExtrapLinear xSig ySig
              (TC $ Data 1)) `check` (TC $ Data $ Inter 1.5),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapLinear xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Invalid),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapNone xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Invalid),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapLast xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Extra 1),
             (interp1LinValid "Signal Test" Interp.Linear ExtrapNone xSig ySig
              (TC $ Data 1.5)) `check` (TC $ Data $ Inter 2.5)]

-- | Test against alternative formulated Interpolation
prop_interp1LinValid12 :: (TC Signal.Sample (Typ UT UT UT) (Data Nil Double)) -> Bool
prop_interp1LinValid12 x = (interp1LinValid "Signal Test" Interp.Linear ExtrapLinear xSig ySig x)
                           `check`
                           (interp1LinValid "Signal Test" Interp.Linear2 ExtrapLinear2 xSig ySig x)
  where
    xSig :: UTSignal [] Double
    ySig :: UTSignal [] Double
    xSig = Signal.fromList [-1,-0.2,0,0.1,1,3]
    ySig = Signal.fromList [0,1,10,-20,20,30]

-- | Test against alternative formulated Interpolation
prop_interp1LinValid :: (TC Signal.Sample (Typ UT UT UT) (Data Nil Double)) -> Bool
prop_interp1LinValid x = case
  (fst $ Signal.fromScalar (Signal.minmax xSig)) <= Signal.fromSample x &&
  (snd $ Signal.fromScalar (Signal.minmax xSig)) >= Signal.fromSample x
  of
  False  -> (interpolValue) == (Invalid)
  True -> abs (testVal - unpack interpolValue) < 10^^(-12::Integer)
  where
    testVal = ymax-(xmax-xval)*(ymax-ymin)/(xmax-xmin)
    ymin = (fst $ Signal.fromScalar (Signal.minmax ySig))
    ymax = (snd $ Signal.fromScalar (Signal.minmax ySig))
    xmin = (fst $ Signal.fromScalar (Signal.minmax xSig))
    xmax = (snd $ Signal.fromScalar (Signal.minmax xSig))
    xval = Signal.fromSample x
    xSig = Signal.fromList [0,100]
    ySig = Signal.fromList [1,10]
    xSig,ySig:: UTSignal [] Double
    interpolValue= Signal.fromSample $ interp1LinValid "Signal Test"
                    Interp.Linear ExtrapNone xSig ySig x


-- | test 2D-Interpolation with selected points
prop_interp2WingProfileValid_Simple :: Bool
prop_interp2WingProfileValid_Simple = all (==True) tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]
    zSig = Signal.fromList2 [[10,30],[20,40]]

    -- Test the edge points
    tests = [(interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 1) (Signal.toSample 3)) `check` (TC $ Data $ Inter 10),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 2) (Signal.toSample 3)) `check` (TC $ Data $ Inter 20),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 1) (Signal.toSample 4)) `check` (TC $ Data $ Inter 30),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 2) (Signal.toSample 4)) `check` (TC $ Data $ Inter 40),

    -- Test the edge mid points
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 1.5) (Signal.toSample 3)) `check` (TC $ Data $ Inter 15),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 1.5) (Signal.toSample 4)) `check` (TC $ Data $ Inter 35),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 1) (Signal.toSample 3.5)) `check` (TC $ Data $ Inter 20),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 2) (Signal.toSample 3.5)) `check` (TC $ Data $ Inter 30),

    -- Test the mid point
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 1.5) (Signal.toSample 3.5)) `check` (TC $ Data $ Inter 25)]

-- | Surface with a Step
prop_interp2WingProfileValid_Step :: Bool
prop_interp2WingProfileValid_Step = all (==True) $ tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2,2]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3,4,4],[3,4,4],[3,4,4]]
    zSig = Signal.fromList2 [[10,10,30],[10,10,30],[20,20,40]]

    tests = [(interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 3)) `check` (TC $ Data $ Inter 10),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 3)) `check` (TC $ Data $ Inter 15),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 4)) `check` (TC $ Data $ Inter 20),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 30),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 5)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 3)) `check` (TC $ Data $ Extra 20),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 4)) `check` (TC $ Data $ Invalid)]


-- | test 2D-Interpolation with selected points
prop_interp2WingProfileValid_ExtrapNone :: Bool
prop_interp2WingProfileValid_ExtrapNone = all (==True) tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]
    zSig = Signal.fromList2 [[10,30],[20,40]]

    -- Test no extrapolation
    tests = [(interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 3)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 4)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 3)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 4)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 2)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 5)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 2)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 5)) `check` (TC $ Data $ Invalid),
            (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 2)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 5)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 2)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 5)) `check` (TC $ Data $ Invalid)]

-- | test 2D-Interpolation with selected points
prop_interp2WingProfileValid_ExtrapLast :: Bool
prop_interp2WingProfileValid_ExtrapLast = all (==True) tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]
    zSig = Signal.fromList2 [[10,30],[20,40]]

    tests = [(interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 3)) `check` (TC $ Data $ Extra 10),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 4)) `check` (TC $ Data $ Extra 30),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 3)) `check` (TC $ Data $ Extra 20),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 4)) `check` (TC $ Data $ Extra 40),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 10),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 30),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 20),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 40),
            (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 10),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 30),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 20),
             (interp2WingProfileValid "" Interp.Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 40)]

-- | test 2D-Interpolation with selected points
prop_interp2WingProfileValid_ExtrapLinear :: Bool
prop_interp2WingProfileValid_ExtrapLinear = all (==True) tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]
    zSig = Signal.fromList2 [[10,30],[20,40]]
-- Test linear extrapolation
    tests = [(interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 3)) `check` (TC $ Data $ Extra 0),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 4)) `check` (TC $ Data $ Extra 20),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 3)) `check` (TC $ Data $ Extra 30),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 4)) `check` (TC $ Data $ Extra 50),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 2)) `check` (TC $ Data $ Extra (-10)),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 50),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 0),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 60),
            (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 2)) `check` (TC $ Data $ Extra (-20)),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 40),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 10),
             (interp2WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 70)]

prop_interp2WingProfileValid_linear12 :: (TC Signal.Sample (Typ UT UT UT) (Data Nil Double)) ->
                                         (TC Signal.Sample (Typ UT UT UT) (Data Nil Double)) ->
                                         Bool
prop_interp2WingProfileValid_linear12 x y =
  (interp2WingProfileValid "Signal Test" Interp.Linear ExtrapLinear xSig ySig zSig x y) `check`
  (interp2WingProfileValid "Signal Test" Interp.Linear ExtrapLinear xSig ySig zSig x y)
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2,5]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4, 5,6],[3 , 4, 6,7],[3 , 5, 6,8]]
    zSig = Signal.fromList2 [[10,30,10,5],[20,40,20,0],[20,40,10,2]]

prop_interp3WingProfileValid :: Bool
prop_interp3WingProfileValid = all (==True) tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]

    zSig,vSig :: UTSignal3 [] [] [] Double
    zSig = Signal.fromList3 [[[5,6],[5,6]],[[5,6],[5,6]]]
    vSig = Signal.fromList3 [[[10,30],[12,32]],[[20,40],[22,42]]]

    tests = [(interp3WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1) (Signal.toSample 3) (Signal.toSample 5)) `check` (TC $ Data $ Inter 10),
             (interp3WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 2) (Signal.toSample 3) (Signal.toSample 5)) `check` (TC $ Data $ Inter 20),
             (interp3WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1) (Signal.toSample 4) (Signal.toSample 5)) `check` (TC $ Data $ Inter 12),
             (interp3WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1) (Signal.toSample 3) (Signal.toSample 6)) `check` (TC $ Data $ Inter 30),
             (interp3WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1) (Signal.toSample 3) (Signal.toSample 5.5)) `check` (TC $ Data $ Inter 20),
             (interp3WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1) (Signal.toSample 3.5) (Signal.toSample 5)) `check` (TC $ Data $ Inter 11),
             (interp3WingProfileValid "" Interp.Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1.5) (Signal.toSample 3) (Signal.toSample 5)) `check` (TC $ Data $ Inter 15),
             (interp3WingProfileValid "" Interp.Linear ExtrapNone xSig ySig zSig vSig
             (Signal.toSample 0) (Signal.toSample 3) (Signal.toSample 5)) `check` (TC $ Data $ Invalid)]

prop_interp3WingProfileValidWithSignal :: Bool
prop_interp3WingProfileValidWithSignal = all (==True) tests


  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]

    zSig,vSig :: UTSignal3 [] [] [] Double
    zSig = Signal.fromList3 [[[5,6],[5,6]],[[5,6],[5,6]]]
    vSig = Signal.fromList3 [[[10,30],[12,32]],[[20,40],[22,42]]]

    tests = [(Signal.interp3WingProfileValidWithSignal "" Interp.Linear ExtrapNone xSig ySig zSig vSig
             (Signal.fromList [1,2,1]) (Signal.fromList [3,3,4]) (Signal.fromList [5,5,5])) `checkSig` (Signal.fromList [Inter 10,Inter 20,Inter 12])]





runTests :: IO Bool
runTests = $quickCheckAll

