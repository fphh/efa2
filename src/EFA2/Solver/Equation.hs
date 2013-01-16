

module EFA2.Solver.Equation where

import qualified EFA2.Solver.Term as Term
import qualified EFA2.Report.Format as Format
import EFA2.Report.FormatValue (FormatValue, formatValue)

import EFA2.Interpreter.Env as Env
import qualified EFA2.Signal.Index as Idx

import Control.Monad (liftM2)

import qualified Data.Ratio as Ratio
import Data.Ratio (Ratio, (%))

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Stream as Stream
import Data.Stream (Stream)


data Equation =
            EqTerm := EqTerm
          | EqEdge Env.Index Env.Index Env.Index
          | Given Env.Index deriving (Show, Eq, Ord)

data EdgeUnknown =
            PowerIn
          | Eta
          | PowerOut
             deriving (Show, Eq, Ord, Enum)

data Assign =
            AbsAssign AbsAssign
          | AssignEdge EdgeUnknown Env.Index Env.Index Env.Index
             deriving (Show, Eq, Ord)

data AbsAssign =
            Env.Index ::= EqTerm
          | GivenIdx Env.Index deriving (Show, Eq, Ord)

type EqTerm = Term Env.Index

data Term a =
            Atom a
          | Const Rational
               {- we initialize it only with 0 or 1,
                  but constant folding may yield any rational number -}

          | Minus (Term a)
          | Recip (Term a)
          | (Term a) :+ (Term a)
          | (Term a) :* (Term a) deriving (Show, Eq, Ord)



instance Num (Term idx) where
   fromInteger x = Const (x % 1)
   negate = Minus
   (+) = (:+)
   (*) = (:*)

instance Fractional (Term idx) where
   fromRational = Const
   recip = Recip
   (/) = (&/)


instance Functor Term where
   fmap f =
      let go t =
             case t of
                Atom a -> Atom $ f a
                Const x -> Const x

                Minus x -> Minus $ go x
                Recip x -> Recip $ go x
                x :+ y -> go x :+ go y
                x :* y -> go x :* go y
      in go

infixl 1 !=, !:=, :=, ::=
infixl 7  !*, !/, :*, &/
infixl 6  !+, !-, :+, &-


{-

{- |
For consistency with '(:+)' it should be named '(:-)'
but this is reserved for constructors.
-}

-}

(&-) :: Term a -> Term a -> Term a
x &- y  =  x :+ Minus y

(&/) :: Term a -> Term a -> Term a
x &/ y  =  x :* Recip y



(!+) :: (MkTermC a, MkTermC b) => a -> b -> EqTerm
x !+ y = mkTerm x :+ mkTerm y

(!-) :: (MkTermC a, MkTermC b) => a -> b -> EqTerm
x !- y = mkTerm x &- mkTerm y

(!*) :: (MkTermC a, MkTermC b) => a -> b -> EqTerm
x !* y = mkTerm x :* mkTerm y

(!/) :: (MkTermC a, MkTermC b) => a -> b -> EqTerm
x !/ y = mkTerm x &/ mkTerm y

(!=) :: (MkTermC a, MkTermC b) => a -> b -> Equation
x != y = mkTerm x := mkTerm y

(!:=) :: (MkIdxC a, MkTermC b) => a -> b -> AbsAssign
x !:= y = mkIdx x ::= mkTerm y

give :: MkIdxC a => a -> Equation
give = Given . mkIdx


class MkIdxC a where
   mkIdx :: a -> Env.Index

instance MkIdxC Idx.Energy where mkIdx = Env.Energy
instance MkIdxC Idx.DEnergy where mkIdx = Env.DEnergy
instance MkIdxC Idx.MaxEnergy where mkIdx = Env.MaxEnergy
instance MkIdxC Idx.DMaxEnergy where mkIdx = Env.DMaxEnergy
instance MkIdxC Idx.Power where mkIdx = Env.Power
instance MkIdxC Idx.DPower where mkIdx = Env.DPower
instance MkIdxC Idx.Eta where mkIdx = Env.Eta
instance MkIdxC Idx.DEta where mkIdx = Env.DEta
instance MkIdxC Idx.DTime where mkIdx = Env.DTime
instance MkIdxC Idx.X where mkIdx = Env.X
instance MkIdxC Idx.DX where mkIdx = Env.DX
instance MkIdxC Idx.Y where mkIdx = Env.Y
instance MkIdxC Idx.DY where mkIdx = Env.DY
instance MkIdxC Idx.Var where mkIdx = Env.Var
instance MkIdxC Idx.Storage where mkIdx = Env.Store


class MkVarC a where
   mkVarCore :: Env.Index -> a

instance MkVarC Env.Index where
   mkVarCore = id

instance MkVarC a => MkVarC (Term a) where
   mkVarCore = Atom . mkVarCore

instance MkVarC a => MkVarC (Term.Term a) where
   mkVarCore = Term.Atom . mkVarCore

mkVar :: (MkIdxC a, MkVarC b) => a -> b
mkVar = mkVarCore . mkIdx


class MkTermC a where
   mkTerm :: a -> EqTerm

instance MkTermC Idx.Energy where mkTerm = mkVar
instance MkTermC Idx.DEnergy where mkTerm = mkVar
instance MkTermC Idx.Power where mkTerm = mkVar
instance MkTermC Idx.DPower where mkTerm = mkVar
instance MkTermC Idx.Eta where mkTerm = mkVar
instance MkTermC Idx.DEta where mkTerm = mkVar
instance MkTermC Idx.DTime where mkTerm = mkVar
instance MkTermC Idx.X where mkTerm = mkVar
instance MkTermC Idx.DX where mkTerm = mkVar
instance MkTermC Idx.Var where mkTerm = mkVar
instance MkTermC Idx.Storage where mkTerm = mkVar


instance MkTermC Env.Index where
   mkTerm = Atom

instance MkTermC Double where
   mkTerm = Const . realToFrac

instance MkTermC Integer where
   mkTerm = Const . fromInteger

instance Integral int => MkTermC (Ratio int) where
   mkTerm x = Const $ toInteger (Ratio.numerator x) % toInteger (Ratio.denominator x)

class ToIndex idx where
   toIndex :: idx -> Env.Index

instance ToIndex Env.Index where
   toIndex = id

instance (ToIndex idx) => MkTermC (Term idx) where
   mkTerm = fmap toIndex


instance (Eq a, FormatValue a) => FormatValue (Term a) where
   formatValue = formatTerm


formatTerm ::
   (FormatValue a, Format.Format output) => Term a -> output
formatTerm =
   let go t =
          case t of
             Const x -> Format.real (fromRational x :: Double)
             Atom x -> formatValue x

             x :+ y -> Format.parenthesize $ Format.plus (go x) (go y)
             x :* y -> Format.multiply (go x) (go y)

             Recip x -> Format.recip $ go x
             Minus x -> Format.minus $ Format.parenthesize $ go x
   in  go


--------------------------------------------------------------------


pushMult :: Term a -> Term a
pushMult t = Term.add $ go t
  where go :: Term a -> NonEmpty.T [] (Term a)
        go (Minus u) = fmap Minus (go u)
        go (Recip u) = NonEmpty.singleton $ Recip $ pushMult u
        go (u :+ v) = NonEmpty.append (go u) (go v)
        go (u :* v) = liftM2 (:*) (go u) (go v)
        go s = NonEmpty.singleton s

streamPairs :: Stream a -> Stream (a, a)
streamPairs xs = Stream.zip xs (Stream.tail xs)

iterateUntilFix :: (Eq a) => (a -> a) -> a -> a
iterateUntilFix f =
   fst . Stream.head . Stream.dropWhile (uncurry (/=)) .
   streamPairs . Stream.iterate f

simplifyOld :: Eq a => Term a -> Term a
simplifyOld = iterateUntilFix simplify' . pushMult
  where simplify' :: Eq a => Term a -> Term a
        simplify' (Const x :+ Const y) = Const $ x+y
        simplify' ((Const 0.0) :+ x) = simplify' x
        simplify' (x :+ (Const 0.0)) = simplify' x

        simplify' (Const x :* Const y) = Const $ x*y
        simplify' ((Const 1.0) :* x) = simplify' x
        simplify' (x :* (Const 1.0)) = simplify' x
        simplify' ((Const 0.0) :* _) = Const 0.0
        simplify' (_ :* (Const 0.0)) = Const 0.0

        simplify' (Recip (Const x)) = Const $ recip x
        simplify' (x :* (Recip y)) | x == y = Const 1.0
        simplify' ((Minus x) :* (Recip y)) | x == y = Const (-1.0)
        simplify' ((Recip x) :* y) | x == y = Const 1.0
        simplify' ((Recip x) :* (Minus y)) | x == y = Const (-1.0)

        simplify' (Recip (Recip x)) = simplify' x
        simplify' (Recip x) = Recip (simplify' x)

        simplify' (Minus (Const x)) = Const $ negate x
        simplify' (Minus (Minus x)) = simplify' x
        simplify' (Minus x) = Minus (simplify' x)
        simplify' ((Minus x) :* (Minus y)) = simplify' x :* simplify' y
        simplify' (x :+ y) = simplify' x :+ simplify' y
        simplify' (x :* y) = simplify' x :* simplify' y
        simplify' x = x


simplify :: Ord a => Term a -> Term a
simplify = fromNormalTerm . toNormalTerm

toNormalTerm :: Ord a => Term a -> Term.Term a
toNormalTerm =
   let go t =
          case t of
             Atom a -> Term.Atom a
             Const x -> fromRational x

             Minus x -> negate $ go x
             Recip x -> recip $ go x
             x :+ y -> go x + go y
             x :* y -> go x * go y
   in  go

fromNormalTerm :: Ord a => Term.Term a -> Term a
fromNormalTerm = Term.evaluate Atom
