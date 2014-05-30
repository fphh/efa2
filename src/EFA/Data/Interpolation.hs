{-# LANGUAGE FlexibleInstances #-}


module EFA.Data.Interpolation where

import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom

import EFA.Utility(Caller,
                   merror,-- (|>),
                   ModuleName(..),FunctionName, genCaller)

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-), Product, (~*), (~/), Constant)

import EFA.Report.FormatValue as FormatValue
--import qualified Test.QuickCheck as QC
--import System.Random (Random)
import qualified EFA.Value.Type as Type 

import Control.Applicative
import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)

modul :: ModuleName
modul = ModuleName "Data.Interpolation"

nc :: FunctionName -> Caller
nc = genCaller modul

data Pos = HitLeft | Inside | HitRight | Outside | Undefined deriving (Show)

-- TODO: Problems showing the string -> double quotes and wrong unit scaling, no numer format
data Val a = Inter a
                 | Extra a
                 | Invalid [String] deriving (Show)


instance (Arith.Sum a) => Arith.Sum (Val a) where
  x ~+ y = combineWith (~+) x y
  {-# INLINE (~+) #-}
  
  x ~- y = combineWith (~-) x y
  {-# INLINE (~-) #-}

  negate x = Arith.negate x
  {-# INLINE negate #-}

instance (Arith.Product a, Arith.Constant a) =>
         Arith.Product (Val a) where
  x ~* y =  combineWith (~*) x y
  {-# INLINE (~*) #-}

  x ~/ y =  combineWith (~/) x y
  {-# INLINE (~/) #-}

  recip x = Arith.recip x
  {-# INLINE recip #-}

  constOne _ = Inter $ Arith.one
  {-# INLINE constOne #-}

instance (Constant a) => Constant (Val a) where
   zero = (Inter Arith.zero)
   fromInteger x = Inter $ Arith.fromInteger x
   fromRational x = Inter $ Arith.fromRational x

instance (Constant a, Eq a) => Eq (Val a) where
  (==) x y = (unpack x) == (unpack y) 
  (/=) x y = (unpack x) /= (unpack y)

instance (Constant a, Eq a,Ord a) => Ord (Val a) where
  (>=) x y = (unpack x) >= (unpack y)  
  (>) x y = (unpack x) > (unpack y)  
  (<=) x y = (unpack x) <= (unpack y)  
  (<) x y = (unpack x) < (unpack y)  

instance (FormatValue a,Show a) => FormatValue.FormatValue (Val a) where 
  formatValue (Inter x) =(FormatValue.formatValue  x) 
  formatValue (Extra x) = Format.pair (FormatValue.formatValue 'E') (FormatValue.formatValue x) 
  formatValue (Invalid xs) = (Format.list $ map Format.literal xs)
  
instance Tuple.C a => Tuple.C (Val a) where
  text (Inter x) = Tuple.text x
  text (Extra x) = Tuple.text x
  text (Invalid x) = Tuple.text (0/0::Double)

instance Atom.C (Val a) where

-- TODO : apply display unit to Invalid ?? 
instance (Type.ToDisplayUnit a) => Type.ToDisplayUnit (Val a) where
  toDisplayUnit (Inter x) = Inter (Type.toDisplayUnit x)
  toDisplayUnit (Extra x) = Extra (Type.toDisplayUnit x)
  toDisplayUnit (Invalid x) = Invalid x
  
instance Type.GetDynamicType a =>Type.GetDynamicType (Val a) where 
  getDynamicType (Inter x) = Type.getDynamicType x
  getDynamicType (Extra x) = Type.getDynamicType x
  getDynamicType (Invalid x) = Type.UD
  

instance Functor Val where
  fmap f (Invalid xs) = Invalid xs -- $ map (\(label,x) -> (label,f x)) xs
  fmap f (Extra x) = Extra (f x)
  fmap f (Inter x) = Inter (f x)


instance Applicative Val where
  pure x = Inter x
  Extra f <*> Extra x = Extra (f x)
  Extra f <*> Inter x = Extra (f x)
  Inter f <*> Extra x = Extra (f x)
  Inter f <*> Inter x = Inter (f x)
  Invalid xs <*> _ = Invalid xs
  _ <*> Invalid xs = Invalid xs
  
combineWith :: (a -> a -> a) -> Val a -> Val a -> Val a  
combineWith f x y = case (x,y) of 
 (Inter v,Inter v1) -> Inter (f v v1)
 (Inter v,Extra v1) -> Extra (f v v1)
 (Extra v,Inter v1) -> Extra (f v v1)
 (Extra v,Extra v1) -> Extra (f v v1)
 (Inter _,Invalid v1) -> Invalid v1
 (Invalid v,Inter _)  -> Invalid v
 (Extra _,Invalid v1) -> Invalid v1
 (Invalid v,Extra _)  -> Invalid v
 (Invalid v,Invalid v1) -> Invalid $ v++v1 

combine :: Val a -> Val a -> Val a  
combine x y = combineWith (\v _ -> v) x y
 
combine3 :: Val a -> Val a -> Val a -> Val a
combine3 x y z = combine (combine x y) z  

{-

TODO - funktor definierte Arith und Test klassen wieder reaktivieren
instance (Arith.Sum a) => Arith.Sum (Val a) where
  x ~+ y = liftA2 (~+) x y
  {-# INLINE (~+) #-}

  x ~- y =  liftA2 (~-) x y
  {-# INLINE (~-) #-}

  negate x =  fmap Arith.negate x
  {-# INLINE negate #-}


instance (Arith.Product a, Arith.Constant a) =>
         Arith.Product (Val a) where
  x ~* y = liftA2 (~*) x y
  {-# INLINE (~*) #-}

  x ~/ y = liftA2 (~/) x y
  {-# INLINE (~/) #-}

  recip x = fmap Arith.recip x
  {-# INLINE recip #-}

  constOne _ = Inter $ Arith.one
  {-# INLINE constOne #-}
-}

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
isInvalid (Invalid _) = True
isInvalid _ = False

unpack :: (Product a, Constant a) => Val a -> a
unpack (Inter x) = x
unpack (Extra x) = x
unpack (Invalid _) = (Arith.zero) ~/ (Arith.zero)

makeInvalid :: Product a => a -> Val a
makeInvalid _ = Invalid []

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
              Caller ->
              Method a ->
              ExtrapMethod a ->
              String ->
              (a,a) -> (a,a) -> Val a -> Val a

dim1 _ _  _ _  _ _ (Invalid xs) = (Invalid xs)

dim1 caller inmethod exmethod label (x1, x2) (y1, y2) (Extra x) = 
  case compare x1 x2 of

    LT -> case getPos (x1, x2) x of
               Undefined -> Invalid [show label ++ "@" ++ show x]
               HitLeft -> Extra y1
               HitRight -> Extra y2
               Inside ->
                 case inmethod of
                   Linear -> Extra $ linear (x1,x2) (y1,y2) x
                   Nearest -> Extra $ nearest (x1, x2) (y1, y2) x
               Outside ->
                 case exmethod of
                   ExtrapLinear ->  Extra $ linear (x1,x2) (y1,y2) x
                   ExtrapLast ->  Extra $ nearest (x1, x2) (y1,y2) x
                   (ExtrapVal val) ->  Extra $ val
                   ExtrapNone ->  Invalid [show label ++ "@" ++ show x]
                   ExtrapError ->
                     merror caller modul "interp1Core" ("Extrapolation not allowed" ++
                             "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)

    EQ -> case getPos (x1, x2) x of
               Undefined -> Invalid [show label ++ "@" ++ show x]
               Outside ->
                 case exmethod of
                   ExtrapLinear ->  Invalid [show label ++ "@" ++ show x]
                   ExtrapLast -> Extra $ if x < x1 then y1 else y2
                   (ExtrapVal val) ->  Extra $ val
                   ExtrapNone ->  Invalid [show label ++ "@" ++ show x]
                   ExtrapError ->
                     merror caller modul "interp1Core" ("Extrapolation not allowed" ++
                             "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)

               _   -> Extra $ (y1 ~+ y2) ~/ (Arith.constOne x1 ~+ Arith.constOne x1)

    GT -> merror caller modul "interp1Core" "x1 greater than x2"
    
dim1 caller inmethod exmethod label (x1, x2) (y1, y2) (Inter x) =
  case compare x1 x2 of

    LT -> case getPos (x1, x2) x of
               Undefined -> Invalid [show label ++ "@" ++ show x]
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
                   ExtrapNone ->  Invalid [show label ++ "@" ++ show x]
                   ExtrapError ->
                     merror caller modul "interp1Core" ("Extrapolation not allowed" ++
                             "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)

    EQ -> case getPos (x1, x2) x of
               Undefined -> Invalid [show label ++ "@" ++ show x]
               Outside ->
                 case exmethod of
                   ExtrapLinear ->  Invalid  [show label ++ "@" ++ show x]
                   ExtrapLast -> Extra $ if x < x1 then y1 else y2
                   (ExtrapVal val) ->  Extra $ val
                   ExtrapNone ->  Invalid [show label ++ "@" ++ show x]
                   ExtrapError ->
                     merror caller modul "interp1Core" ("Extrapolation not allowed" ++
                             "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)

               _   -> Inter $ (y1 ~+ y2) ~/ (Arith.constOne x1 ~+ Arith.constOne x1)

    GT -> merror caller modul "interp1Core" "x1 greater than x2"

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

{-
combine3 :: Val a -> Val a -> Val a -> Val a
combine3 x y z =
  liftA3 (\a _ _ -> a) z y x


-- = Only for testing

newtype ValConstructor a label = ValConstructor (a -> Val a)

instance QC.Arbitrary (ValConstructor label a) where
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

instance QC.Arbitrary (Val String (Double -> Double)) where
  arbitrary = do
    f <- QC.arbitrary
    ValConstructor ctr <- QC.arbitrary
    return (ctr f)

instance QC.Arbitrary (Val String Double) where
  arbitrary = do
    x <- QC.choose (negate 10, 10 :: Double)
    ValConstructor f <- QC.arbitrary
    return (f x)
-}
