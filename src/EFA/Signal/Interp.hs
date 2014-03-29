{-# LANGUAGE FlexibleInstances #-}


module EFA.Signal.Interp where

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-), Product, (~*), (~/), Constant)


import qualified Test.QuickCheck as QC
import System.Random (Random)

import Control.Applicative

data Pos = HitLeft | Inside | HitRight | Outside | Undefined deriving (Show)

data Val a = Inter a
           | Extra a
           | Invalid deriving (Show, Eq)

instance Functor Val where
  fmap _ Invalid = Invalid
  fmap f (Extra x) = Extra (f x)
  fmap f (Inter x) = Inter (f x)

instance Applicative Val where
  pure x = Inter x
  Extra f <*> Extra x = Extra (f x)
  Extra f <*> Inter x = Extra (f x)
  Inter f <*> Extra x = Extra (f x)
  Inter f <*> Inter x = Inter (f x)
  _ <*> _ = Invalid


instance (Arith.Sum a) => Arith.Sum (Val a) where
  x ~+ y = liftA2 (~+) x y
  {-# INLINE (~+) #-}

  x ~- y =  liftA2 (~-) x y
  {-# INLINE (~-) #-}

  negate x =  fmap Arith.negate x
  {-# INLINE negate #-}


instance (Arith.Product a, Arith.Constant a) =>
         Arith.Product (Val  a) where
  x ~* y = liftA2 (~*) x y
  {-# INLINE (~*) #-}

  x ~/ y = liftA2 (~/) x y
  {-# INLINE (~/) #-}

  recip x = fmap Arith.recip x
  {-# INLINE recip #-}

  constOne _ = Inter $ Arith.one
  {-# INLINE constOne #-}

{-

instance Arith.Integrate (Sweep vec a) where
  type Scalar (Sweep vec a) = (Sweep vec a)
  integrate = id
  {-# INLINE integrate #-}


instance (UV.Unbox a, Eq a, Num a, Arith.Constant a) =>
         Arith.ZeroTestable (Sweep UV.Vector a) where
  allZeros (Sweep x) = UV.and (UV.map (Arith.zero ==) x)
  {-# INLINE allZeros #-}

  coincidingZeros (Sweep x) (Sweep y) =
    UV.or $ UV.zipWith (\a b -> a == Arith.zero && b == Arith.zero) x y

instance (FormatValue.FormatValue a, UV.Unbox a) =>
         FormatValue.FormatValue (Sweep UV.Vector a) where
  formatValue = FormatValue.formatValue . toList

-}



isExtra:: Val a -> Bool
isExtra (Extra _) = True
isExtra _ = False

isInvalid:: Val a -> Bool
isInvalid Invalid = True
isInvalid _ = False

unpack :: (Product a, Constant a) => Val a -> a
unpack (Inter x) = x
unpack (Extra x) = x
unpack Invalid = (Arith.zero) ~/ (Arith.zero)

makeInvalid :: Product a => a -> Val a
makeInvalid _ = Invalid

data Method a = Linear | Nearest deriving (Show)

data ExtrapMethod a =
  ExtrapLinear
  | ExtrapNone
  | ExtrapVal a
  | ExtrapLast
  | ExtrapError
  deriving (Show)

linear :: (Product a, Sum a) => (a, a) -> (a, a) -> a  -> a
linear (x0, x1) (y0, y1) x =
  y0 ~* (x1 ~- x) ~/ (x1 ~- x0) ~+ y1 ~* (x ~- x0) ~/ (x1 ~- x0)

nearest :: (Ord a, Sum a,Product a) => (a, a) -> (a, a) -> a  -> a
nearest (x1, x2) (y1, y2) x =
  if Arith.abs (x1 ~- x) < Arith.abs (x2 ~- x) then y1 else y2


dim1 :: (Eq a,Ord a, Sum a, Product a, Show a) =>
              String ->
              Method a ->
              ExtrapMethod a ->
              (a,a) -> (a,a) -> a -> Val a

dim1 caller inmethod exmethod (x1, x2) (y1, y2) x =
  case compare x1 x2 of

    LT -> case getPos (x1, x2) x of
               Undefined -> Invalid
               HitLeft -> Inter y1
               HitRight -> Inter y2
               Inside ->
                 case inmethod of
                   Linear -> Inter $ linear (x1,x2) (y1,y2) x
                   Nearest -> Inter $ nearest (x1, x2) (y1, y2) x
               Outside ->
                 case exmethod of
                   ExtrapLinear ->  Extra $ linear (x1,x2) (y1,y2) x
                   ExtrapLast ->  Extra $ nearest (x1, x2) (y1,y2) x
                   (ExtrapVal val) ->  Extra $ val
                   ExtrapNone ->  Invalid
                   ExtrapError ->
                     error $ "Error in interp1Core called by " ++ caller ++
                             ": Method ExtrapError - Extrapolation not allowed" ++
                             "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x

    EQ -> case getPos (x1, x2) x of
               Undefined -> Invalid
               Outside ->
                 case exmethod of
                   ExtrapLinear ->  Invalid
                   ExtrapLast -> Extra $ if x < x1 then y1 else y2
                   (ExtrapVal val) ->  Extra $ val
                   ExtrapNone ->  Invalid
                   ExtrapError ->
                     error $ "Error in interp1Core called by " ++ caller ++
                             ": Method ExtrapError - Extrapolation not allowed" ++
                             "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x

               _   -> Inter $ (y1 ~+ y2) ~/ (Arith.constOne x1 ~+ Arith.constOne x1)

    GT -> error ("Error in interp1Core called by " ++ caller ++ ": x1 greater than x2")

getPos :: (Eq a, Ord a) => (a,a) -> a -> Pos
getPos (x1,x2) x =
  case (x == x1, x > x1 , x < x2 , x == x2) of
       (True,_,_,False) -> HitLeft
       (False,_,_,True) -> HitRight
       (True,_,_,True) -> Inside -- aplies when step in signal => x1=x2=x
       (_,True,True,_) -> Inside
       (_,False,True,_) -> Outside
       (_,True,False,_) -> Outside
       (_,_,_,_) -> Undefined -- error "Error in getPos - Impossible branch"


combine3 :: Val a -> Val a -> Val a -> Val a
combine3 x y z =
  liftA3 (\a _ _ -> a) z y x


-- = Only for testing

newtype ValConstructor a = ValConstructor (a -> Val a)

instance QC.Arbitrary (ValConstructor a) where
  arbitrary = do
    m <- QC.choose (0, 2 :: Int)
    return $ ValConstructor $
      case m of
           0 -> const Invalid
           1 -> Extra
           2 -> Inter
           x -> error $ "3: not defined for " ++ show x


instance (QC.Arbitrary a, Num a, Random a) => QC.Arbitrary (ExtrapMethod a) where
  arbitrary = QC.choose (0, 3 :: Int) >>= f
    where f 0 = return ExtrapLinear
          f 1 = return ExtrapNone
          f 2 = QC.choose (-100, 100) >>= return . ExtrapVal
          f 3 = return ExtrapLast
          f x = error $ "1: f not defined for " ++ show x

instance (QC.Arbitrary a, Num a, Random a) => QC.Arbitrary (Method a) where
  arbitrary = QC.choose (0, 1 :: Int) >>= f
    where f 0 = return Linear
          f 1 = return Nearest
          f x = error $ "2: f not defined for " ++ show x

instance QC.Arbitrary (Val (Double -> Double)) where
  arbitrary = do
    f <- QC.arbitrary
    ValConstructor ctr <- QC.arbitrary
    return (ctr f)

instance QC.Arbitrary (Val Double) where
  arbitrary = do
    x <- QC.choose (negate 10, 10 :: Double)
    ValConstructor f <- QC.arbitrary
    return (f x)
