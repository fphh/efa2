module EFA.Signal.Interp where

import qualified Prelude as P
import Prelude ((==),(>=), (>),Show,Eq, String, Ord,error,(++), ($), show, Bool(True, False))

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-), Product, (~*), (~/), Constant)

import Data.Ord (compare,Ordering(GT,EQ,LT))

data Pos = Within | Outside
data Val a = Inter a | Extra a | Invalid deriving (Show, Eq)

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
              Pos->
              (a,a) -> (a,a) -> a -> Val a

dim1 caller inmethod exmethod interpos (x1,x2) (y1,y2) x = case compare x2 x1 of
  LT -> error ("Error in interp1Core called by " ++ caller ++ ": x1 greater than x2")
  EQ -> case interpos of
            Within -> Inter $ (y1 ~+ y2) ~/ (Arith.constOne x1 ~+ Arith.constOne x1)
            Outside -> case exmethod of
              ExtrapLinear ->  Invalid
              ExtrapLinear2 ->  Invalid
              ExtrapLast ->  Extra $ nearest1 (x1,x2) (y1,y2) x
              (ExtrapVal val) ->  Extra $ val
              ExtrapNone ->  Invalid
              ExtrapError -> error ("Error in interp1Core called by " ++ caller ++
                                      ": Method ExtrapError - Extrapolation not allowed" ++
                                      "x1: " ++ show x1 ++ " x2: " ++ show x2 ++ " x: " ++ show x)

  GT ->  if x == x1 then Inter y1
              else case interpos of
                Within -> case inmethod of
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
