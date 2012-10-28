module EFA2.Bijection where

import Control.Category (Category, id, (.))
import Control.Monad (liftM2)

import qualified Prelude as P
import Prelude hiding (id, (.), div, sqrt, recip)


{- |
If you construct a value using 'Cons'
then you are responsible for using functions
that are inverse to each other.
The safe way to construct bijections
is to compose the given primitives.
-}
data T a b = Cons (a -> b) (b -> a)

instance Category T where
   id = Cons id id
   Cons ff fb . Cons gf gb =
      Cons (ff . gf) (gb . fb)

instance C T where
   cons = Cons
   inverse (Cons f g) = Cons g f

infixr 0 $#

($#) :: T a b -> a -> b
Cons f _g $# a = f a


{- |
Using 'Weak' constructor may break invariances. Cf. 'Cons'.
-}
data Weak a b = Weak (Maybe (a -> b)) (Maybe (b -> a))

weak :: T a b -> Weak a b
weak (Cons f g) = Weak (Just f) (Just g)


instance Category Weak where
   id = cons id id
   Weak ff fb . Weak gf gb =
      Weak (liftM2 (.) ff gf) (liftM2 (.) gb fb)

instance C Weak where
   cons f g = Weak (Just f) (Just g)
   inverse (Weak f g) = Weak g f



-- * primitives

class Category f => C f where
   inverse :: f a b -> f b a
   cons :: (a -> b) -> (b -> a) -> f a b


add, sub :: (C f, Num a) => a -> f a a
add x = cons (x+) (subtract x)
sub x = cons (x-) (x-)


mul, div :: (C f, Fractional a) => a -> f a a
mul x = cons (*x) (/x)
div x = cons (x/) (x/)

recip :: (C f, Fractional a) => f a a
recip = cons P.recip P.recip

sqrt :: (C f, Floating a) => f a a
sqrt  = cons P.sqrt (^(2::Int))



infixr 6 #+
infixl 6 +#, -#
infix 6 #-

(#+), (#-) :: (C f, Num a) => a -> f b a -> f b a
(+#), (-#) :: (C f, Num a) => f b a -> a -> f b a
x #+ f = add x . f
f +# x = cons (+x) (subtract x) . f
x #- f = sub x . f
f -# x = cons (subtract x) (+x) . f


infixr 7 #*
infixl 7 *#, /#
infix 7 #/

(#*), (#/) :: (C f, Fractional a) => a -> f b a -> f b a
(*#), (/#) :: (C f, Fractional a) => f b a -> a -> f b a
x #* f = mul x . f
f *# x = cons (*x) (/x) . f
x #/ f = div x . f
f /# x = cons (/x) (*x) . f


frecip :: (C f, Fractional a) => f b a -> f b a
frecip = (recip.)

fsqrt :: (C f, Floating a) => f b a -> f b a
fsqrt = (sqrt.)
