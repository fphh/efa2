{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}


module EFA.Test.Cube where

import EFA.Signal.Signal
  (TC(TC),UTSignal,UTSignal2,--UTSignal3, viewL, viewR,
   --interp1LinValid,
   interp2WingProfileValid)
   --interp3WingProfileValid)

import qualified EFA.Value.Type as Type
import qualified EFA.Signal.Signal as Signal

import EFA.Signal.Data (Data(Data), Nil) --, Apply)
import EFA.Signal.Typ(Typ,UT)
import qualified EFA.Signal.Interp as Interp
import EFA.Data.Interpolation
   (Val(Inter, Extra, Invalid),
   Method(Linear), --, Nearest),
   ExtrapMethod(ExtrapLinear,
                ExtrapNone,
                ExtrapLast,
                ExtrapVal,
                ExtrapError
               ))

import qualified Test.QuickCheck as QC

import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.ND as ND
import EFA.Utility(ModuleName(ModuleName),Caller,FunctionName, genCaller)

--import EFA.TestUtility (Func(Func), unFunc)

import Test.QuickCheck.All (quickCheckAll)
--import qualified Test.QuickCheck as QC
--import System.Random (Random)

--import Control.Applicative (liftA2, (<*>), pure)
--import Control.Monad (replicateM)

import Prelude hiding (Left, Right)
import qualified Data.List as List


--import qualified Data.List as List
import qualified EFA.Data.ND.Cube.Map as Cube
import qualified EFA.Data.ND.Cube.Map(Cube)
import qualified EFA.Data.Interpolation as DataInterp
--import EFA.Data.Interpolation(Val(Inter,Extra,Invalid))
import EFA.Utility.Trace (mytrace)
--import qualified EFA.Data.ND as ND
import EFA.TestUtility (Func(Func), unFunc)
import qualified Data.Map as Map

modul::ModuleName
modul=ModuleName "EFA.Test.Cube"

nc :: FunctionName -> Caller
nc = genCaller modul

check x y = mytrace 0 "Test.Cube" "check" $ checkVal (mytrace 1 "Test.Cube" "val" x)  (mytrace 1 "Test.Cube" "ref" y)

--check :: Val Double -> Val Double -> Bool
checkVal Invalid Invalid = True
checkVal (Inter x) (Inter y) = valCheck x y
checkVal (Extra x) (Extra y) = valCheck x y
checkVal _ _ = False

cube :: EFA.Data.ND.Cube.Map.Cube typ ND.Dim2 [Char] [] Double Double
cube = Cube.create (nc "cube") [("x",Type.P,[1,2]),("y",Type.P,[3,4])] [10,30,20,40]

cube3D :: EFA.Data.ND.Cube.Map.Cube typ ND.Dim3 [Char] [] Double Double
cube3D = Cube.create (nc "cube") [("x",Type.P,[1,2]),("y",Type.P,[3,4]),("z",Type.P,[5,6])] [10,30,12,32,20,40,22,42]

valCheck :: Double -> Double -> Bool
valCheck x1 x2 = abs (x1 - x2) < 10^^(-12 :: Integer)

-- | test 2D-Interpolation with selected points
prop_interp2D_TestPoints :: Bool
prop_interp2D_TestPoints = and tests
  where
    call=nc "prop_interp2D_TestPoints"
    f = DataInterp.dim1 "prop_interp2D_TestPoints" Linear ExtrapNone

    -- Test the edge points
    tests = [
      (Cube.interpolate call f cube (ND.Data [1,3]) `check` Inter (10)),
      (Cube.interpolate call f cube (ND.Data [2,3])  `check` Inter (20)),
      (Cube.interpolate call f cube (ND.Data [1,4])  `check` Inter (30)),
      (Cube.interpolate call f cube (ND.Data [2,4]) `check` Inter (40)),

      (Cube.interpolate call f cube (ND.Data [1.5,3]) `check` Inter (15)),
      (Cube.interpolate call f cube (ND.Data [1.5,4]) `check` Inter (35)),
      (Cube.interpolate call f cube (ND.Data [1,3.5]) `check` Inter (20)),
      (Cube.interpolate call f cube (ND.Data [2,3.5]) `check` Inter (30)),

      (Cube.interpolate call f cube (ND.Data [1.5,3.5]) `check` Inter (25))]

prop_ExtrapLinear2D_TestPoints :: Bool
prop_ExtrapLinear2D_TestPoints = and tests
  where
    call=nc "prop_ExtrapLinear2D_TestPoints"
    f = DataInterp.dim1 "prop_ExtrapLinear2D_TestPoints" Linear ExtrapLinear
    tests = [
      (Cube.interpolate call f cube (ND.Data [0,3]) `check` Extra (0)),
      (Cube.interpolate call f cube (ND.Data [0,4])  `check` Extra (20)),
      (Cube.interpolate call f cube (ND.Data [3,3])  `check` Extra (30)),
      (Cube.interpolate call f cube (ND.Data [3,4]) `check` Extra (50)),
      (Cube.interpolate call f cube (ND.Data [1,2]) `check` Extra (-10)),
      (Cube.interpolate call f cube (ND.Data [1,5]) `check` Extra (50)),
      (Cube.interpolate call f cube (ND.Data [2,2]) `check` Extra (0)),
      (Cube.interpolate call f cube (ND.Data [2,5]) `check` Extra (60)),
      (Cube.interpolate call f cube (ND.Data [0,2]) `check` Extra (-20)),
      (Cube.interpolate call f cube (ND.Data [0,5]) `check` Extra (40)),
      (Cube.interpolate call f cube (ND.Data [3,2]) `check` Extra (10)),
      (Cube.interpolate call f cube (ND.Data [3,5]) `check` Extra (70))
      ]

prop_ExtrapNone_TestPoints :: Bool
prop_ExtrapNone_TestPoints = and tests
  where
    call=nc "prop_ExtrapNone_TestPoints"
    f = DataInterp.dim1 "prop_ExtrapNone_TestPoints" Linear ExtrapNone
    tests = [
      (Cube.interpolate call f cube (ND.Data [0,3]) `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [0,4])  `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [3,3])  `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [3,4]) `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [1,2]) `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [1,5]) `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [2,2]) `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [2,5]) `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [0,2]) `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [0,5]) `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [3,2]) `check` Invalid),
      (Cube.interpolate call f cube (ND.Data [3,5]) `check` Invalid)
      ]

prop_Interp3D_TestPoints :: Bool
prop_Interp3D_TestPoints = and tests --(foldl (+) 0 $ map (\ x -> if True then 1 else 0) tests) > 100
  where
    -- cube3D = Cube.create (nc "cube") [("x",[1,2]),("y",[3,4]),("z",[5,6])] [10,30,12,32, 20,40,22,42]
    call=nc "prop_Interp3D_TestPoints"
    f = DataInterp.dim1 "prop_Interp3D_TestPoints" Linear ExtrapNone
    tests = [
      Cube.interpolate call f cube3D (ND.Data [1,3,5]) `check` Inter 10,
      Cube.interpolate call f cube3D (ND.Data [2,3,5]) `check` Inter 20,
      Cube.interpolate call f cube3D (ND.Data [1,4,5]) `check` Inter 12,

      Cube.interpolate call f cube3D (ND.Data [1,3,6]) `check` Inter 30,
      Cube.interpolate call f cube3D (ND.Data [1,3,5.5]) `check` Inter 20,
      Cube.interpolate call f cube3D (ND.Data [1,3.5,5]) `check` Inter 11,

      Cube.interpolate call f cube3D (ND.Data [1.5,3,5]) `check` Inter 15,
      Cube.interpolate call f cube3D (ND.Data [0,3,5]) `check` Invalid]


prop_lookUp :: Bool
prop_lookUp = and tests
  where
    cube4D = Cube.create (nc "cube") [("x",Type.P,[1,2]),
                                      ("y",Type.E,[3,4]),
                                      ("z",Type.P,[7,8])] $
             [10,30,11,31,12,32,13,33::Int] :: Cube.Cube typ ND.Dim3 String [] Double Int
    caller =nc "prop_Interp3D_TestPoints"
    tests = [
      Cube.lookUp caller  (ND.Data $ map Strict.Idx [0,0,0]) cube4D == 10,
      Cube.lookUp caller  (ND.Data $ map Strict.Idx [0,0,1]) cube4D == 30,
      Cube.lookUp caller  (ND.Data $ map Strict.Idx [0,1,0]) cube4D == 11,
      Cube.lookUp caller  (ND.Data $ map Strict.Idx [0,1,1]) cube4D == 31,
      Cube.lookUp caller  (ND.Data $ map Strict.Idx [1,0,0]) cube4D == 12,
      Cube.lookUp caller  (ND.Data $ map Strict.Idx [1,0,1]) cube4D == 32,
      Cube.lookUp caller  (ND.Data $ map Strict.Idx [1,1,0]) cube4D == 13,
      Cube.lookUp caller  (ND.Data $ map Strict.Idx [1,1,1]) cube4D == 33]


prop_extract :: Bool
prop_extract = and tests
  where
    f x y z = (Cube.getVector $ Cube.getData $ Cube.extract (nc "Demo.Cube.Main") cube4D x y) == z
    cube4D = Cube.create (nc "cube") [("x",Type.P,[1,2]),
                                      ("y",Type.E,[3,4]),
                                      ("z",Type.P,[7,8])] $
             [10,30,11,31,12,32,13,33::Int] :: Cube.Cube typ ND.Dim3 String [] Double Int
    caller = nc "prop_Interp3D_TestPoints"
    tests = [
      f (ND.Data $ map ND.Idx [1,2]) (Map.fromList [(ND.Idx 0, Strict.Idx 0)]) [10,30,11,31],
      f (ND.Data $ map ND.Idx [1,2]) (Map.fromList [(ND.Idx 0, Strict.Idx 1)]) [12,32,13,33],
      f (ND.Data $ map ND.Idx [0,2]) (Map.fromList [(ND.Idx 1, Strict.Idx 0)]) [10,30,12,32],
      f (ND.Data $ map ND.Idx [0,2]) (Map.fromList [(ND.Idx 1, Strict.Idx 1)]) [11,31,13,33],
      f (ND.Data $ map ND.Idx [0,1]) (Map.fromList [(ND.Idx 2, Strict.Idx 0)]) [10,11,12,13],
      f (ND.Data $ map ND.Idx [0,1]) (Map.fromList [(ND.Idx 2, Strict.Idx 1)]) [30,31,32,33]
      ]


{-

TODO: Referenzimplementation und Test von HH aktivieren

-------------------------------------------------------------------

machineEpsilon :: Double
machineEpsilon =
  last
  . Prelude.map (subtract 1)
  . takeWhile (/= 1)
  . Prelude.map (+ 1)
  . iterate (/2) $ 1


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
-}

runTests :: IO Bool
runTests = $quickCheckAll


main :: IO ()
main = runTests >>= print

