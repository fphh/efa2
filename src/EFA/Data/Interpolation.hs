{-# LANGUAGE FlexibleInstances #-}


module EFA.Data.Interpolation where

import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom

import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Value.State as ValueState

import EFA.Utility(Caller,
                   merror, (|>),
                   ModuleName(..),FunctionName, genCaller)

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-), Product, (~*), (~/), Constant, sqrt, square, constOne, Root)

import EFA.Report.FormatValue as FormatValue
--import qualified Test.QuickCheck as QC
--import System.Random (Random)
import qualified EFA.Value.Type as Type 

import qualified EFA.Utility.Trace as UtTrace
import Debug.Trace(trace)

import Control.Applicative
import qualified EFA.Report.Format as Format
--import EFA.Report.Format (Format)

import Prelude hiding (sqrt)

modul :: ModuleName
modul = ModuleName "Data.Interpolation"

nc :: FunctionName -> Caller
nc = genCaller modul

data Pos = HitLeft | Inside | HitRight | Outside | Undefined deriving (Show)

data Val a = Inter a
                 | Extra a
                 | Invalid [String] deriving (Show)

instance (Eq a, Product a, Constant a) => Arith.ZeroTestable (Val a) where
  allZeros (Invalid _) = False
  allZeros x = Arith.zero == unpack x
  coincidingZeros (Invalid _) _ = False
  coincidingZeros _ (Invalid _) = False
  coincidingZeros x y = Arith.zero == unpack x && Arith.zero == unpack y

instance (Arith.NaNTestable a) => Arith.NaNTestable (Val a) where
  checkIsNaN (Inter x) = Arith.checkIsNaN x
  checkIsNaN (Extra x) = Arith.checkIsNaN x
  checkIsNaN (Invalid _) = False

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
  text (Invalid _) = Tuple.text (0/0::Double)

instance Atom.C (Val a) where

-- TODO : apply display unit to Invalid ?? 
instance (Type.ToDisplayUnit a) => Type.ToDisplayUnit (Val a) where
  toDisplayUnit (Inter x) = Inter (Type.toDisplayUnit x)
  toDisplayUnit (Extra x) = Extra (Type.toDisplayUnit x)
  toDisplayUnit (Invalid x) = Invalid x
  
instance Type.GetDynamicType a =>Type.GetDynamicType (Val a) where 
  getDynamicType (Inter x) = Type.getDynamicType x
  getDynamicType (Extra x) = Type.getDynamicType x
  getDynamicType (Invalid _) = Type.UD
  

instance Functor Val where
  fmap _ (Invalid xs) = Invalid xs
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
  

-- TODO :: CombineWith & Co ersetzen mit Functor
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



-- TODO - funktor definierte Arith und Test klassen wieder reaktivieren
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

data Method a = Linear | Nearest | LinEtaUpStream | LinEtaDwnStream deriving (Show)

data ExtrapMethod a =
  ExtrapLinear
  | ExtrapLinEtaUpStream
  | ExtrapLinEtaDwnStream
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

-- | Interpolation to be used to interpolate the reverse-Efficiency-Curve on the Upstream-side, 
-- when the down stream efficiency curve was defined with linear interpolation
-- this special interpolation provides also reversable results even when not hitting supporting points
linearEtaUpStream :: (Sum a, Product a) => (a, a) -> (a, a) -> a  -> a
linearEtaUpStream (p0, p1) (n0, n1) p = n0 ~/ (one ~- c ~* dP)
  where
    one = Arith.constOne p
    dP = (p ~- p0)
    c = (n1 ~- n0) ~/ (p1 ~- p0)
    
-- | Interpolation to be used to interpolate the reverse-Efficiency-Curve on the Downstream-side, 
-- when the up stream efficiency curve was defined with linear interpolation
-- this special interpolation provides also reversable results even when not hitting supporting points
linearEtaDwnStream :: (Ord a, Root a, Sum a, Product a) => (a, a) -> (a, a) -> a  -> a
linearEtaDwnStream  (p0, p1) (n0, n1) p = if n1 >= n0 then (n0 ~+ sqrt (square n0 ~+  (four~*dn~*dP)~/dP0))~/two 
                                          else (n0 ~- sqrt (square n0 ~+ (four~*dn~*dP)~/dP0))~/two
                                            
  where
    two = constOne p ~+ constOne p 
    four = two ~+ two
    dP = (p ~- p0)
    dn = (n1 ~- n0) 
    dP0 = p1 ~- p0


dim1 :: (Eq a,Ord a, Sum a, Product a, Show a, Root a) =>
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
                   LinEtaUpStream -> Extra $ linearEtaUpStream (x1,x2) (y1,y2) x
                   LinEtaDwnStream -> Extra $ linearEtaDwnStream (x1,x2) (y1,y2) x
                   Nearest -> Extra $ nearest (x1, x2) (y1, y2) x
               Outside ->
                 case exmethod of
                   ExtrapLinear ->  Extra $ linear (x1,x2) (y1,y2) x
                   ExtrapLinEtaUpStream -> Extra $ linearEtaUpStream (x1,x2) (y1,y2) x
                   ExtrapLinEtaDwnStream -> Extra $ linearEtaDwnStream (x1,x2) (y1,y2) x
                   ExtrapLast ->  Extra $ nearest (x1, x2) (y1,y2) x
                   (ExtrapVal val) ->  Extra $ val
                   ExtrapNone ->  Invalid [show label ++ "@" ++ show x]
                   ExtrapError ->
                     merror caller modul "interp1Core" ("Extrapolation not allowed " ++
                             "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)

    EQ -> case getPos (x1, x2) x of
               Undefined -> Invalid [show label ++ "@" ++ show x]
               Outside ->
                 case exmethod of
                   ExtrapLinear ->  Invalid [show label ++ "@" ++ show x]
                   ExtrapLinEtaUpStream -> Invalid [show label ++ "@" ++ show x]
                   ExtrapLinEtaDwnStream -> Invalid [show label ++ "@" ++ show x]
                   ExtrapLast -> Extra $ if x < x1 then y1 else y2
                   (ExtrapVal val) ->  Extra $ val
                   ExtrapNone ->  Invalid [show label ++ "@" ++ show x]
                   ExtrapError ->
                     merror caller modul "interp1Core" ("Extrapolation not allowed " ++
                             "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)

               _   -> Extra $ (y1 ~+ y2) ~/ (Arith.constOne x1 ~+ Arith.constOne x1)

    GT -> merror caller modul "interp1Core" $ "x1 greater than x2 - x1: " ++ show x1 ++ " x2: " ++ show x2
    
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
                   LinEtaUpStream -> trace "InterInsideLTUp" $ Inter $ linearEtaUpStream (x1,x2) (y1,y2) x
                   LinEtaDwnStream -> trace "InterInsideLtDwn" $ Inter $ linearEtaDwnStream (x1,x2) (y1,y2) x
               Outside ->
                 case exmethod of
                   ExtrapLinear ->  Extra $ linear (x1,x2) (y1,y2) x
                   ExtrapLinEtaUpStream -> Extra $ linearEtaUpStream (x1,x2) (y1,y2) x
                   ExtrapLinEtaDwnStream -> Extra $ linearEtaDwnStream (x1,x2) (y1,y2) x
                   ExtrapLast ->  Extra $ nearest (x1, x2) (y1,y2) x
                   (ExtrapVal val) ->  Extra $ val
                   ExtrapNone ->  Invalid [show label ++ "@" ++ show x]
                   ExtrapError ->
                     merror caller modul "interp1Core" ("Extrapolation not allowed " ++
                             "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)

    EQ -> case getPos (x1, x2) x of
               Undefined -> Invalid [show label ++ "@" ++ show x]
               Outside ->
                 case exmethod of
                   ExtrapLinear ->  Invalid  [show label ++ "@" ++ show x]
                   ExtrapLinEtaUpStream -> Invalid [show label ++ "@" ++ show x]
                   ExtrapLinEtaDwnStream -> Invalid [show label ++ "@" ++ show x]
                   ExtrapLast -> Extra $ if x < x1 then y1 else y2
                   (ExtrapVal val) ->  Extra $ val
                   ExtrapNone ->  Invalid [show label ++ "@" ++ show x]
                   ExtrapError ->
                     merror caller modul "interp1Core" ("Extrapolation not allowed " ++
                             "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)

               _   -> Inter $ (y1 ~+ y2) ~/ (Arith.constOne x1 ~+ Arith.constOne x1)

    GT -> merror caller modul "interp1Core" $ "x1 greater than x2 - x1: " ++ show x1 ++ " x2: " ++ show x2

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

-- | This is the Version to Interpolate the optimal maps
-- TODO -- should it be moved to Axis.Strict ?       
dim1WithSupport ::
  (Eq a,Ord a, Sum a, Product a, Show a, Root a, Constant a) =>
  Caller ->
  Method a ->
  String ->
  Strict.SupportingPoints (a,Val a) 
  -> a 
  -> Val a
dim1WithSupport _ _ _ (Strict.LeftPoint (_,y)) _ = y
dim1WithSupport _ _ _ (Strict.RightPoint (_,y)) _ = y
dim1WithSupport caller inmethod label (Strict.PairOfPoints (x0,y0) (x1,y1)) x = combine3 y y0 y1 
  where y = dim1 (caller |> nc "dim1WithSupport") inmethod ExtrapError label (x0,x1) (unpack y0, unpack y1) (Inter x)
  
dim1PerState ::
  (Eq a,Ord a, Sum a, Product a, Show a, Constant a, Root a) =>
  Caller ->
  Method a ->
  String ->
  (a,a) ->
  (ValueState.Map (Val a),ValueState.Map (Val a)) -> 
  a -> 
  ValueState.Map (Val a)
dim1PerState caller inmethod label (x0,x1) (y0,y1) x = ValueState.zipWith3 combine3 y y0 y1 
  where y = ValueState.zipWith f y0 y1
        f ya yb = dim1 (caller |> nc "dim1PerState") inmethod ExtrapError label (x0,x1) 
            (unpack ya, unpack yb) (Inter x)
  
dim1PerStateWithMaybe ::
  (Eq a,Ord a, Sum a, Product a, Show a, Constant a, Root a) =>
  Caller ->
  Method a ->
  String ->
  (a,a) ->
  (ValueState.Map (Maybe (Val a)),ValueState.Map (Maybe (Val a))) -> 
  a -> 
  ValueState.Map (Maybe (Val a))
dim1PerStateWithMaybe caller inmethod label (x0,x1) (y0,y1) x = ValueState.zipWith3 (liftA3 combine3) y y0 y1 
  where y = ValueState.zipWith f y0 y1
        f (Just ya) (Just yb) = Just $ dim1 (caller |> nc "dim1PerState") inmethod ExtrapError label (x0,x1) 
            (unpack ya, unpack yb) (Inter x)
        f _ _ = Nothing
        
-- TODO: correct logic behind greaterThanWithInvalid to correspond with >
-- | Invalid is always smaller, deliver first Index with maximum Value
greaterThanWithInvalid :: (Arith.Constant a,Ord a) => Val a -> Val a -> Bool
greaterThanWithInvalid (Invalid _) x = case x of
                                          (Invalid _) -> False
                                          _ -> True
greaterThanWithInvalid _ (Invalid _) = False
greaterThanWithInvalid x y = (unpack y) > (unpack x)

-- | Invalid is always bigger
lessThanWithInvalid :: (Arith.Constant a,Ord a) => Val a -> Val a -> Bool
lessThanWithInvalid (Invalid _) _ = True
lessThanWithInvalid _ (Invalid _) = False
lessThanWithInvalid x y = (unpack y) < (unpack x)


-- | Invalid is the worst in this case the smalles Value
compareMaxWithInvalid :: (Ord a,Constant a) => Val a -> Val a -> Ordering
compareMaxWithInvalid (Invalid _) (Invalid _) = EQ 
compareMaxWithInvalid (Invalid _) _ = LT
compareMaxWithInvalid _ (Invalid _) = GT
compareMaxWithInvalid x y = compare (unpack x) (unpack y)

-- | Invalid is the worst in this case the smalles Value
compareMinWithInvalid :: (Ord a,Constant a) => Val a -> Val a -> Ordering
compareMinWithInvalid (Invalid _) (Invalid _) = EQ 
compareMinWithInvalid (Invalid _) _ = GT
compareMinWithInvalid _ (Invalid _) = LT
compareMinWithInvalid x y = compare (unpack x) (unpack y)
