{-# LANGUAGE TypeFamilies #-}
module EFA.Example.NestedDelta where

import qualified EFA.Example.Utility as Utility
import EFA.Equation.System ((=.=))

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Graph.Topology.Index as Idx
import EFA.Utility (Pointed)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Foldable as Fold
import Control.Applicative (Applicative, pure, liftA2)



data Extruder f a =
   Extruder {
      extrudeOuter :: f (Maybe a) -> Record.ExtDelta f (Maybe a),
      extrudeInner ::
         (a -> f (Maybe a)) ->
         a -> a -> Record.ExtDelta f (Maybe a)
   }

extrudeStart :: InnerExtrusion Record.Absolute a
extrudeStart = InnerExtrusion $ Record.Absolute . Just


beforeDelta :: (Applicative f, Arith.Sum a) => Extruder f a
beforeDelta =
   Extruder {
      extrudeOuter = \x ->
         Record.ExtDelta {
            Record.extBefore = x,
            Record.extAfter = pure Nothing,
            Record.extDelta = fmap (fmap Arith.clear) x
         },
      extrudeInner = \cons x y ->
         Record.ExtDelta {
            Record.extBefore = cons x,
            Record.extAfter = pure Nothing,
            Record.extDelta = cons y
         }
   }

beforeAfter :: (Applicative f, Arith.Sum a) => Extruder f a
beforeAfter =
   Extruder {
      extrudeOuter = \x ->
         Record.ExtDelta {
            Record.extBefore = x,
            Record.extAfter = fmap (fmap Arith.clear) x,
            Record.extDelta = pure Nothing
         },
      extrudeInner = \cons x y ->
         Record.ExtDelta {
            Record.extBefore = cons x,
            Record.extAfter = cons y,
            Record.extDelta = pure Nothing
         }
   }

afterDelta :: (Applicative f, Arith.Sum a) => Extruder f a
afterDelta =
   Extruder {
      extrudeOuter = \x ->
         Record.ExtDelta {
            Record.extBefore = pure Nothing,
            Record.extAfter = x,
            Record.extDelta = fmap (fmap Arith.clear) x
         },
      extrudeInner = \cons x y ->
         Record.ExtDelta {
            Record.extBefore = pure Nothing,
            Record.extAfter = cons x,
            Record.extDelta = cons y
         }
   }


newtype
   InnerExtrusion f a =
      InnerExtrusion {runInnerExtrusion :: a -> f (Maybe a)}
newtype
   OuterExtrusion f a =
      OuterExtrusion {runOuterExtrusion :: a -> a -> f (Maybe a)}


infixr 0 <&, <&>, &>

(&>) ::
   (Arith.Sum a) =>
   Extruder f a ->
   InnerExtrusion f a ->
   InnerExtrusion (Record.ExtDelta f) a
e &> InnerExtrusion f =
   InnerExtrusion $ \x -> extrudeInner e f x (Arith.clear x)

(<&>) ::
   (Arith.Sum a) =>
   Extruder f a ->
   InnerExtrusion f a ->
   OuterExtrusion (Record.ExtDelta f) a
e <&> InnerExtrusion f = OuterExtrusion $ extrudeInner e f

(<&) ::
   (Arith.Sum a) =>
   Extruder f a ->
   OuterExtrusion f a ->
   OuterExtrusion (Record.ExtDelta f) a
e <& OuterExtrusion f =
   OuterExtrusion $ \x y -> extrudeOuter e $ f x y



infixr 0 &&>

data
   ParameterRecord g f a =
      ParameterRecord {
         getInnerCube :: InnerExtrusion f a,
         getParameterRecord :: g (OuterExtrusion f a)
      }

parameterStart :: ParameterRecord NonEmpty.Empty Record.Absolute a
parameterStart = ParameterRecord absolute NonEmpty.Empty

(&&>) ::
   (Functor g, Arith.Sum a) =>
   Extruder f a ->
   ParameterRecord g f a ->
   ParameterRecord (NonEmpty.T g) (Record.ExtDelta f) a
e &&> ParameterRecord inner xs =
   ParameterRecord (e&>inner)
      (NonEmpty.Cons (e<&>inner) $ fmap (e<&) xs)


parameterSymbol ::
   (Pointed term,
    t ~ Utility.VarTerm var Idx.Delta term node,
    Eq t, Arith.Sum t, Arith.Constant t,
    Ord (idx node),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   OuterExtrusion rec t ->
   idx node -> rec (Maybe t)

parameterSymbol param idx =
   runOuterExtrusion param
      (Utility.symbol (Idx.before $ Var.index idx))
      (Utility.symbol (Idx.delta $ Var.index idx))

absoluteSymbol ::
   (Absolute rec, Pointed term,
    t ~ Utility.VarTerm var Idx.Delta term node,
    Eq t, Arith.Sum t, Arith.Constant t,
    Ord (idx node),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   idx node -> rec (Maybe t)

absoluteSymbol idx =
   absoluteRecord (Utility.symbol (Idx.before $ Var.index idx))

parameterRecord ::
   (Arith.Sum x) =>
   OuterExtrusion rec x ->
   x -> x -> rec (Maybe x)
parameterRecord = runOuterExtrusion

absoluteRecord ::
   (Absolute rec, Arith.Sum x) =>
   x -> rec (Maybe x)
absoluteRecord = runInnerExtrusion absolute


class Absolute rec where
   absolute :: InnerExtrusion rec a

instance Absolute Record.Absolute where
   absolute = extrudeStart

instance Absolute Record.Delta where
   absolute =
      InnerExtrusion $ \x ->
         Record.Delta {
            Record.before = Just x,
            Record.delta = Nothing,
            Record.after = Nothing
         }

instance
   (Absolute f, Applicative f) =>
      Absolute (Record.ExtDelta f) where
   absolute =
      InnerExtrusion $ \x ->
         Record.ExtDelta {
            Record.extBefore = runInnerExtrusion absolute x,
            Record.extDelta = pure Nothing,
            Record.extAfter = pure Nothing
         }



infix 0 ?=


(?=) ::
   (EqGen.Record rec,
    Eq x, Arith.Sum x,
    EqGen.Element idx rec s a v
       ~ EqGen.VariableRecord rec s x,
    Env.AccessMap idx, Ord (idx node), Var.Type idx ~ var) =>
   idx node -> rec (Maybe x) ->
   EqGen.EquationSystem rec node s a v
evar ?= val  =
   Fold.fold $
   liftA2
      (\rec ->
         Fold.foldMap
            (\x -> EqGen.variable (Idx.Record rec evar) =.= EqGen.constant x))
      Record.indices val


givenParameterSymbol ::
   (EqGen.Record rec, Pointed term,
    t ~ Utility.VarTerm var Idx.Delta term node,
    Eq t, Arith.Sum t, Arith.Constant t,
    EqGen.Element idx rec s scalar signal
       ~ EqGen.VariableRecord rec s t,
    Ord (idx node),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   idx node ->
   OuterExtrusion rec t ->
   EqGen.EquationSystem rec node s scalar signal
givenParameterSymbol idx param =
   idx ?= parameterSymbol param idx


givenParameterNumber ::
   (EqGen.Record rec,
    Eq x, Arith.Sum x,
    Ord (idx node), Env.AccessMap idx, Var.Index idx,
    EqGen.Element idx rec s a v
       ~ EqGen.VariableRecord rec s x) =>
   idx node -> x -> x ->
   OuterExtrusion rec x ->
   EqGen.EquationSystem rec node s a v
givenParameterNumber idx x y param =
   idx ?= parameterRecord param x y
