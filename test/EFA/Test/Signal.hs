{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}


module EFA.Test.Signal where

import EFA.Signal.Signal
  (TC(TC),UTSignal,UTSignal2,UTSignal3, viewL, viewR,
   interp1LinValid, interp2WingProfileValid, interp3WingProfileValid)
import qualified EFA.Signal.Signal as Signal

import EFA.Signal.Data (Data(Data), Nil, Apply)
import EFA.Signal.Typ(Typ,UT)
import qualified EFA.Signal.Interp as Interp
import EFA.Signal.Interp
  (Val(Inter, Extra, Invalid),
   Method(Linear, Nearest),
   ExtrapMethod(ExtrapLinear, ExtrapNone, ExtrapLast, ExtrapVal, ExtrapError))

import EFA.TestUtility (Func(Func), unFunc)

import Test.QuickCheck.All (quickCheckAll)
import qualified Test.QuickCheck as QC
import System.Random (Random)

import Control.Applicative (liftA2, (<*>), pure)
import Control.Monad (replicateM)

import Prelude hiding (Left, Right)

import qualified Data.List as List


-------------------------------------------------------------------

signal :: Apply ab c -> TC s t (Data ab c)
signal = TC . Data

check :: TC Signal.Sample (Typ UT UT UT) (Data Nil (Interp.Val Double)) ->
         TC Signal.Sample (Typ UT UT UT) (Data Nil (Interp.Val Double))->
         Bool
check (TC (Data x)) (TC (Data y)) = interCheck x y

checkSig :: UTSignal [] (Interp.Val Double) ->
         UTSignal [] (Interp.Val Double) ->
         Bool
checkSig xs ys = Signal.all (==True) $ Signal.zipWith interCheck xs ys

interCheck :: Interp.Val Double -> Interp.Val Double -> Bool
interCheck Invalid Invalid = True
interCheck (Inter x) (Inter y) = valCheck x y
interCheck (Extra x) (Extra y) = valCheck x y
interCheck _ _ = False


machineEpsilon :: Double
machineEpsilon =
  last
  . Prelude.map (subtract 1)
  . takeWhile (/= 1)
  . Prelude.map (+ 1)
  . iterate (/2) $ 1

valCheck :: Double -> Double -> Bool
valCheck x1 x2 = abs (x1 - x2) < 10^^(-12 :: Integer)

-------------------------------------------------------------------
-- Reference

data Pt a = Pt { xPt :: a, yPt :: a } deriving (Show)

interpolate ::
  Fractional a =>
  Pt a -> Pt a -> a -> a
interpolate (Pt x0 y0) (Pt x1 y1) x =
  y0 * (x1-x)/(x1-x0) + y1*(x-x0)/(x1-x0)


findInterval :: (a -> Bool) -> [a] -> Maybe (a, a)
findInterval p xs =
 case List.span p xs of
      (as@(_:_), b:_) -> Just (last as, b)
      _ -> Nothing

prop_findInterval :: AscSignal Double -> X Double -> Bool
prop_findInterval (AscSignal xs) (TC (Data x)) =
  let ys = Signal.toList xs
  in case findInterval (<= x) ys of
          Just (a, b) -> a <= x && x <= b
          Nothing -> case ys of
                          u:_:_ -> x < u || last ys <= x
                          _ -> True

interpolateSignal ::
  (Fractional a, Ord a) =>
  Interp.Method a ->
  AscSignal a ->
  Signal a ->
  X a ->
  Y a
interpolateSignal im (AscSignal xSig) (Signal ySig) (TC (Data x)) =
  signal $ case zs of
       Just (a, b) -> Inter $
         case im of
              Interp.Nearest -> if x - xPt a < xPt b - x then yPt a else yPt b
              _              -> interpolate a b x
       _ -> Invalid
  where xs = Signal.toList xSig
        ys = Signal.toList ySig
        zs = findInterval ((x >=) . xPt) (zipWith Pt xs ys)

data ExtraDir = Left | Middle | Right deriving (Show)

extrapolateSignal ::
  Method Double ->
  ExtrapMethod Double ->
  AscSignal Double ->
  Signal Double ->
  X Double ->
  Y Double
extrapolateSignal im em (AscSignal xSig) (Signal ySig) (TC (Data x)) =
  case (ptsL, ptsR) of
       (Just pL@(p0L, p1L), Just pR@(p0R, p1R)) ->
         case side pL pR x of

              Left -> signal $
                case em of
                     ExtrapNone    -> Invalid
                     ExtrapLinear  -> Extra $ interpolate p0L p1L x
                     ExtrapVal y   -> Extra $ y
                     ExtrapLast    -> Extra $ yPt p0L
                     ExtrapError   -> error "exrapolateSignal"
              Right -> signal $
                case em of
                     ExtrapNone    -> Invalid
                     ExtrapLinear  -> Extra $ interpolate p0R p1R x
                     ExtrapVal y   -> Extra $ y
                     ExtrapLast    -> Extra $ yPt p1R
                     ExtrapError   -> error "exrapolateSignal"

              Middle -> interpolateSignal im (AscSignal xSig) (Signal ySig) (TC (Data x))

       _ -> error $ "extrapolateSignal: not enough points in signal"
                    ++ show xSig ++ ", " ++ show ySig

  where
        ptsL :: Maybe (Pt Double, Pt Double)
        ptsL = do
          (TC (Data x0), xtail) <- viewL xSig
          (TC (Data x1), _) <- viewL xtail
          (TC (Data y0), ytail) <- viewL ySig
          (TC (Data y1), _) <- viewL ytail
          return (Pt x0 y0, Pt x1 y1)

        ptsR :: Maybe (Pt Double, Pt Double)
        ptsR = do
          (xtail, TC (Data x1)) <- viewR xSig
          (_, TC (Data x0)) <- viewR xtail
          (ytail, TC (Data y1)) <- viewR ySig
          (_, TC (Data y0)) <- viewR ytail
          return (Pt x0 y0, Pt x1 y1)

        side (p0L, _) (_, p1R) xx =
          if xx < xPt p0L
             then Left
             else if xx > xPt p1R
                     then Right
                     else Middle



-------------------------------------------------------------------
-- Reference combine

combine :: (a -> a -> a) -> Val a -> Val a -> Val a
combine _ Invalid _ = Invalid
combine _ _ Invalid = Invalid
combine f (Extra x) (Extra y) = Extra $ f x y
combine f (Extra x) (Inter y) = Extra $ f x y
combine f (Inter x) (Extra y) = Extra $ f x y
combine f (Inter x) (Inter y) = Inter $ f x y


combineResults :: Val a -> Val a -> Val a -> Val a
combineResults x y z = combine (\ v _ -> v) z h
  where h = combine (\ v _ -> v) x y

-------------------------------------------------------------------
-- Test data generation

type X a = TC Signal.Sample (Typ UT UT UT) (Data Nil a)

type Y a = TC Signal.Sample (Typ UT UT UT) (Data Nil (Val a))

len :: Int
len = 10

newtype AscSignal a = AscSignal (UTSignal [] a) deriving (Show)

allDifferent ::
  (QC.Arbitrary a, Random a, Fractional a) =>
  Int -> QC.Gen [a]
allDifferent n =
  liftA2 (List.scanl (+)) QC.arbitrary (replicateM (n-1) $ QC.choose (0, 3))

instance (QC.Arbitrary a, Ord a, Fractional a, Random a) =>
         QC.Arbitrary (AscSignal a) where
  arbitrary = allDifferent len >>= return . AscSignal . signal

newtype Signal a = Signal (UTSignal [] a) deriving (Show)

instance (QC.Arbitrary a, Ord a) => QC.Arbitrary (Signal a) where
  arbitrary = QC.vectorOf len QC.arbitrary
    >>= return . Signal . signal

instance (QC.Arbitrary a, QC.CoArbitrary a) => QC.Arbitrary (Val (Func a)) where
  arbitrary = do
    Interp.ValConstructor ctr <- QC.arbitrary
    fmap ctr QC.arbitrary

-------------------------------------------------------------------

prop_checkAscSignal ::
  AscSignal Double ->
  Bool
prop_checkAscSignal (AscSignal sig) =
  and $ zipWith (<) xs (tail xs)
  where xs = Signal.toList sig


-- | Test selected points
prop_interp1LinValid_Simple :: Bool
prop_interp1LinValid_Simple = and tests
  where
    xSig :: UTSignal [] Double
    xSig = TC $ Data [1, 2]

    ySig :: UTSignal [] Double
    ySig = TC $ Data [1, 11]

    tests = [(interp1LinValid "Signal Test" Linear ExtrapNone xSig ySig
              (TC $ Data 1)) `check` (TC $ Data $ Inter 1),
             (interp1LinValid "Signal Test" Linear ExtrapNone xSig ySig
              (TC $ Data 2)) `check` (TC $ Data $ Inter 11),
             (interp1LinValid "Signal Test" Linear ExtrapNone xSig ySig
              (TC $ Data 1.2)) `check` (TC $ Data $ Inter 3),
             (interp1LinValid "Signal Test" Linear ExtrapNone xSig ySig
              (TC $ Data 1.5)) `check` (TC $ Data $ Inter 6),
             (interp1LinValid "Signal Test" Nearest ExtrapNone xSig ySig
              (TC $ Data 1.2)) `check` (TC $ Data $ Inter 1),
             (interp1LinValid "Signal Test" Nearest ExtrapNone xSig ySig
              (TC $ Data 1.5)) `check` (TC $ Data $ Inter 11)]


prop_interp1LinValid_Extrap :: Bool
prop_interp1LinValid_Extrap = and tests
  where
    xSig :: UTSignal [] Double
    xSig = TC $ Data [1, 2]

    ySig :: UTSignal [] Double
    ySig = TC $ Data [1, 11]

    tests = [(interp1LinValid "Signal Test" Linear (ExtrapVal 99) xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Extra 99),

             (interp1LinValid "Signal Test" Linear (ExtrapVal 99) xSig ySig
              (TC $ Data 2.2)) `check` (TC $ Data $ Extra 99),

             (interp1LinValid "Signal Test" Linear ExtrapLast xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Extra 1),

             (interp1LinValid "Signal Test" Linear ExtrapLast xSig ySig
              (TC $ Data 2.2)) `check` (TC $ Data $ Extra 11),

             (interp1LinValid "Signal Test" Linear ExtrapLinear xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Extra (-9)),

             (interp1LinValid "Signal Test" Linear ExtrapLinear xSig ySig
              (TC $ Data 2.2)) `check` (TC $ Data $ Extra 13) ]

-- | Case of vertical segment (both x-Values are same)
prop_interp1LinValid_Vertical :: Bool
prop_interp1LinValid_Vertical = foldl (&&) True tests
  where
    xSig :: UTSignal [] Double
    xSig = TC $ Data [1, 1, 2]

    ySig :: UTSignal [] Double
    ySig = TC $ Data [1, 2, 3]

    tests = [(interp1LinValid "Signal Test" Linear ExtrapLinear xSig ySig
              (TC $ Data 1)) `check` (TC $ Data $ Inter 1.5),
             (interp1LinValid "Signal Test" Linear ExtrapLinear xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Invalid),
             (interp1LinValid "Signal Test" Linear ExtrapNone xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Invalid),
             (interp1LinValid "Signal Test" Linear ExtrapLast xSig ySig
              (TC $ Data 0)) `check` (TC $ Data $ Extra 1),
             (interp1LinValid "Signal Test" Linear ExtrapNone xSig ySig
              (TC $ Data 1.5)) `check` (TC $ Data $ Inter 2.5)]




-- | test 2D-Interpolation with selected points
prop_interp2WingProfileValid_Simple :: Bool
prop_interp2WingProfileValid_Simple = and tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]
    zSig = Signal.fromList2 [[10,30],[20,40]]

    -- Test the edge points
    tests = [(interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 1) (Signal.toSample 3)) `check` (TC $ Data $ Inter 10),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 2) (Signal.toSample 3)) `check` (TC $ Data $ Inter 20),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 1) (Signal.toSample 4)) `check` (TC $ Data $ Inter 30),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 2) (Signal.toSample 4)) `check` (TC $ Data $ Inter 40),

    -- Test the edge mid points
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 1.5) (Signal.toSample 3)) `check` (TC $ Data $ Inter 15),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 1.5) (Signal.toSample 4)) `check` (TC $ Data $ Inter 35),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 1) (Signal.toSample 3.5)) `check` (TC $ Data $ Inter 20),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 2) (Signal.toSample 3.5)) `check` (TC $ Data $ Inter 30),

    -- Test the mid point
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
               (Signal.toSample 1.5) (Signal.toSample 3.5)) `check` (TC $ Data $ Inter 25)]

-- | Surface with a Step
prop_interp2WingProfileValid_Step :: Bool
prop_interp2WingProfileValid_Step = and $ tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2,2]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3,4,4],[3,4,4],[3,4,4]]
    zSig = Signal.fromList2 [[10,10,30],[10,10,30],[20,20,40]]

    tests = [(interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 3)) `check` (TC $ Data $ Inter 10),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 3)) `check` (TC $ Data $ Inter 15),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 4)) `check` (TC $ Data $ Inter 20),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 30),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 5)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 3)) `check` (TC $ Data $ Extra 20),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 4)) `check` (TC $ Data $ Invalid)]


-- | test 2D-Interpolation with selected points
prop_interp2WingProfileValid_ExtrapNone :: Bool
prop_interp2WingProfileValid_ExtrapNone = and tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]
    zSig = Signal.fromList2 [[10,30],[20,40]]

    -- Test no extrapolation
    tests = [(interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 3)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 4)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 3)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 4)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 2)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 5)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 2)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 5)) `check` (TC $ Data $ Invalid),
            (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 2)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 5)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 2)) `check` (TC $ Data $ Invalid),
             (interp2WingProfileValid "" Linear ExtrapNone xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 5)) `check` (TC $ Data $ Invalid)]

-- | test 2D-Interpolation with selected points
prop_interp2WingProfileValid_ExtrapLast :: Bool
prop_interp2WingProfileValid_ExtrapLast = and tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]
    zSig = Signal.fromList2 [[10,30],[20,40]]

    tests = [(interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 3)) `check` (TC $ Data $ Extra 10),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 4)) `check` (TC $ Data $ Extra 30),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 3)) `check` (TC $ Data $ Extra 20),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 4)) `check` (TC $ Data $ Extra 40),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 10),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 30),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 20),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 40),
            (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 10),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 30),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 20),
             (interp2WingProfileValid "" Linear ExtrapLast xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 40)]

-- | test 2D-Interpolation with selected points
prop_interp2WingProfileValid_ExtrapLinear :: Bool
prop_interp2WingProfileValid_ExtrapLinear = and tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ], [3 , 4]]
    zSig = Signal.fromList2 [[10,30], [20,40]]
-- Test linear extrapolation
    tests = [(interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 3)) `check` (TC $ Data $ Extra 0),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 4)) `check` (TC $ Data $ Extra 20),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 3)) `check` (TC $ Data $ Extra 30),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 4)) `check` (TC $ Data $ Extra 50),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 2)) `check` (TC $ Data $ Extra (-10)),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 1)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 50),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 0),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 2)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 60),
            (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 2)) `check` (TC $ Data $ Extra (-20)),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 0)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 40),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 2)) `check` (TC $ Data $ Extra 10),
             (interp2WingProfileValid "" Linear ExtrapLinear xSig ySig zSig
                (Signal.toSample 3)   (Signal.toSample 5)) `check` (TC $ Data $ Extra 70)]


prop_interp2WingProfileValid_linear12 :: (TC Signal.Sample (Typ UT UT UT) (Data Nil Double)) ->
                                         (TC Signal.Sample (Typ UT UT UT) (Data Nil Double)) ->
                                         Bool
prop_interp2WingProfileValid_linear12 x y =
  (interp2WingProfileValid "Signal Test" Linear ExtrapLinear xSig ySig zSig x y) `check`
  (interp2WingProfileValid "Signal Test" Linear ExtrapLinear xSig ySig zSig x y)
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2,5]

    ySig,zSig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4, 5, 6], [3 , 4, 6,7], [3 , 5, 6,8]]
    zSig = Signal.fromList2 [[10,30,10,5], [20,40,20,0], [20,40,10,2]]

prop_interp3WingProfileValid :: Bool
prop_interp3WingProfileValid = and tests
  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]

    zSig,vSig :: UTSignal3 [] [] [] Double
    zSig = Signal.fromList3 [[[5,6],[5,6]],[[5,6],[5,6]]]
    vSig = Signal.fromList3 [[[10,30],[12,32]],[[20,40],[22,42]]]

    tests = [(interp3WingProfileValid "" Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1) (Signal.toSample 3) (Signal.toSample 5)) `check` (TC $ Data $ Inter 10),
             (interp3WingProfileValid "" Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 2) (Signal.toSample 3) (Signal.toSample 5)) `check` (TC $ Data $ Inter 20),
             (interp3WingProfileValid "" Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1) (Signal.toSample 4) (Signal.toSample 5)) `check` (TC $ Data $ Inter 12),
             (interp3WingProfileValid "" Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1) (Signal.toSample 3) (Signal.toSample 6)) `check` (TC $ Data $ Inter 30),
             (interp3WingProfileValid "" Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1) (Signal.toSample 3) (Signal.toSample 5.5)) `check` (TC $ Data $ Inter 20),
             (interp3WingProfileValid "" Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1) (Signal.toSample 3.5) (Signal.toSample 5)) `check` (TC $ Data $ Inter 11),
             (interp3WingProfileValid "" Linear ExtrapLinear xSig ySig zSig vSig
             (Signal.toSample 1.5) (Signal.toSample 3) (Signal.toSample 5)) `check` (TC $ Data $ Inter 15),
             (interp3WingProfileValid "" Linear ExtrapNone xSig ySig zSig vSig
             (Signal.toSample 0) (Signal.toSample 3) (Signal.toSample 5)) `check` (TC $ Data $ Invalid)]

prop_interp3WingProfileValidWithSignal :: Bool
prop_interp3WingProfileValidWithSignal = and tests


  where
    xSig :: UTSignal [] Double
    xSig = Signal.fromList [1,2]

    ySig :: UTSignal2 [] [] Double
    ySig = Signal.fromList2 [[3 ,4 ],[3 , 4]]

    zSig,vSig :: UTSignal3 [] [] [] Double
    zSig = Signal.fromList3 [[[5,6],[5,6]],[[5,6],[5,6]]]
    vSig = Signal.fromList3 [[[10,30],[12,32]],[[20,40],[22,42]]]

    tests = [(Signal.interp3WingProfileValidWithSignal "" Linear ExtrapNone xSig ySig zSig vSig
             (Signal.fromList [1,2,1]) (Signal.fromList [3,3,4]) (Signal.fromList [5,5,5])) `checkSig` (Signal.fromList [Inter 10,Inter 20,Inter 12])]



prop_Linear ::
  AscSignal Double ->
  Signal Double ->
  X Double ->
  Bool
prop_Linear xs@(AscSignal xSig) ys@(Signal ySig) x =
  interp1LinValid "Test.Signal" Linear ExtrapNone xSig ySig x
  `check`
  interpolateSignal Linear xs ys x

prop_Nearest ::
  AscSignal Double ->
  Signal Double ->
  X Double ->
  Bool
prop_Nearest xs@(AscSignal xSig) ys@(Signal ySig) x =
  interp1LinValid "Test.Signal" Nearest ExtrapNone xSig ySig x
  `check`
  interpolateSignal Nearest xs ys x

prop_extrapolate ::
  Method Double ->
  ExtrapMethod Double ->
  AscSignal Double ->
  Signal Double ->
  X Double ->
  Bool
prop_extrapolate im em xs@(AscSignal xSig) ys@(Signal ySig) x =
  interp1LinValid "Test.Signal" im em xSig ySig x
  `check`
  extrapolateSignal im em xs ys x


-------------------------------------------------------------------


prop_functor_identity :: Val Double -> Bool
prop_functor_identity x = fmap id x == x


prop_functor_fusion ::
  Func Double -> Func Double -> Val Double -> Bool
prop_functor_fusion (Func f) (Func g) x =
  (fmap f . fmap g) x == fmap (f . g) x


prop_applicative_identity ::
  Val Double -> Bool
prop_applicative_identity v =
  (pure id <*> v) == v


prop_applicative_composition ::
--  Val (Double -> Double) -> Val (Double -> Double) -> Val Double -> Bool
  Val (Func Double) -> Val (Func Double) -> Val Double -> Bool
prop_applicative_composition u v w =
  (pure (.) <*> u' <*> v' <*> w) == (u' <*> (v' <*> w))
  where u' = fmap unFunc u
        v' = fmap unFunc v


prop_applicative_homomorphism ::
  Func Double -> Double -> Bool
prop_applicative_homomorphism (Func f) x =
  (pure f <*> pure x) == (pure (f x) :: Val Double)


prop_applicative_interchange ::
  Val (Func Double) -> Double -> Bool
prop_applicative_interchange f y =
  (f' <*> pure y) == (pure ($ y) <*> f')
  where f' = fmap unFunc f


prop_combine :: Val Double -> Val Double -> Bool
prop_combine x y =
  liftA2 (+) x y == combine (+) x y


prop_combine_results ::
  Val Double -> Val Double -> Val Double -> Bool
prop_combine_results x y z =
  Interp.combine3 x y z == combineResults x y z


runTests :: IO Bool
runTests = $quickCheckAll


main :: IO ()
main = runTests >>= print

