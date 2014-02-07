module EFA.Signal.Interp where

import qualified Prelude as P
import Prelude ((==),(>=), (>),Show,Eq, String, Ord,error,(++), ($), show, Bool(True, False),fmap,Functor)

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-), Product, (~*), (~/), Constant)

import Data.Ord (compare, Ordering(GT, EQ, LT))

data Pos = HitLeft | Inside | HitRight | Outside | Undefined
data Val a = Inter a | Extra a | Invalid deriving (Show, Eq)

instance Functor Val where
  fmap _ Invalid = Invalid 
  fmap f (Extra x) = (Extra $ f x) 
  fmap f (Inter x) = (Inter $ f x) 

instance (Arith.Sum a) => Arith.Sum (Val a) where
  x ~+ y = combine (~+) x y
  {-# INLINE (~+) #-}

  x ~- y =  combine (~-) x y
  {-# INLINE (~-) #-}

  negate x =  fmap Arith.negate x
  {-# INLINE negate #-}


instance (Arith.Product a, Arith.Constant a) =>
         Arith.Product (Val  a) where
  x ~* y = combine (~*) x y
  {-# INLINE (~*) #-}

  x ~/ y = combine (~/) x y
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

data Method a = Linear | Linear2 | Nearest
data ExtrapMethod a = ExtrapLinear | ExtrapLinear2 | ExtrapNone | ExtrapVal a | ExtrapLast | ExtrapError

lin1 :: (Product a, Sum a) => (a,a) -> (a,a) -> a  -> a
lin1 (x1,x2) (y1,y2) x = ((y2 ~- y1) ~/ (x2 ~- x1)) ~* (x ~- x1) ~+ y1

lin1' :: (Product a, Sum a) => (a,a) -> (a,a) -> a  -> a
lin1' (x1,x2) (y1,y2) x = ((y2 ~- y1) ~/ (x2 ~- x1)) ~* (x ~- x2) ~+ y2

lin1_twice ::(Product a, Sum a) => (a,a) -> (a,a) -> a  -> a
lin1_twice (x1,x2) (y1,y2) x = (y~+y')~/(Arith.constOne x1 ~+ Arith.constOne x1)
  where y =  lin1 (x1,x2) (y1,y2) x
        y' =  lin1' (x1,x2) (y1,y2) x

nearest1 :: (Ord a, Sum a,Product a) => (a,a) -> (a,a) -> a  -> a
nearest1 (x1,x2) (y1,y2) x = if x >= (x1~+x2)~/ (Arith.constOne x1 ~+ Arith.constOne x1) then y2 else y1

dim1 :: (Eq a,Ord a, Sum a, Product a, Show a) =>
              String ->
              Method a ->
              ExtrapMethod a ->
              (a,a) -> (a,a) -> a -> Val a

dim1 caller inmethod exmethod (x1,x2) (y1,y2) x = case compare x2 x1 of
  LT -> error ("Error in interp1Core called by " ++ caller ++ ": x1 greater than x2")
  EQ -> case getPos (x1,x2) x of
            Undefined -> Invalid
            Outside -> case exmethod of
              ExtrapLinear ->  Invalid
              ExtrapLinear2 ->  Invalid
              ExtrapLast ->  Extra $ nearest1 (x1,x2) (y1,y2) x
              (ExtrapVal val) ->  Extra $ val
              ExtrapNone ->  Invalid
              ExtrapError -> error ("Error in interp1Core called by " ++ caller ++
                                      ": Method ExtrapError - Extrapolation not allowed" ++
                                      "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)
            _   -> Inter $ (y1 ~+ y2) ~/ (Arith.constOne x1 ~+ Arith.constOne x1)               

  GT ->  case getPos (x1,x2) x of
                Undefined -> Invalid
                HitLeft -> Inter y1
                HitRight -> Inter y2
                Inside -> case inmethod of
                  Linear -> Inter $ lin1 (x1,x2) (y1,y2) x
                  Linear2 -> Inter $ lin1_twice (x1,x2) (y1,y2) x
                  Nearest -> Inter $ nearest1 (x1,x2) (y1,y2) x
                Outside -> case exmethod of
                  ExtrapLinear ->  Extra $ lin1 (x1,x2) (y1,y2) x
                  ExtrapLinear2 ->  Extra $ lin1_twice (x1,x2) (y1,y2) x
                  ExtrapLast ->  Extra $ nearest1 (x1,x2) (y1,y2) x
                  (ExtrapVal val) ->  Extra $ val
                  ExtrapNone ->  Invalid
                  ExtrapError -> error ("Error in interp1Core called by " ++ caller ++
                                      ": Method ExtrapError - Extrapolation not allowed" ++
                                      "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)

getPos :: (Eq a, Ord a) => (a,a) -> a -> Pos
getPos (x1,x2) x = case (x P.== x1, x P.> x1 , x P.< x2 , x P.== x2) of   
  (True,_,_,False) -> HitLeft
  (False,_,_,True) -> HitRight
  (True,_,_,True) -> Inside -- aplies when step in signal => x1=x2=x
  (_,True,True,_) -> Inside
  (_,False,True,_) -> Outside
  (_,True,False,_) -> Outside
  (_,_,_,_) -> Undefined -- error "Error in getPos - Impossible branch" 
  
combine :: (a -> a -> a) -> Val a -> Val a -> Val a
combine _ Invalid _ = Invalid
combine _ _ Invalid = Invalid
combine f (Extra x) (Extra y) = Extra $ f x y
combine f (Extra x) (Inter y) = Extra $ f x y
combine f (Inter x) (Extra y) = Extra $ f x y
combine f (Inter x) (Inter y) = Inter $ f x y


combineResults:: Val a -> Val a -> Val a -> Val a
combineResults x y z = combine (\ v _ -> v) z h
  where h= combine (\ v _ -> v) x y  