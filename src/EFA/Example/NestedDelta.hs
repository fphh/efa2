{-# LANGUAGE TypeFamilies #-}
module EFA.Example.NestedDelta (
   module EFA.Example.NestedDelta,
   (?=),
   ) where

import qualified EFA.Example.Utility as Utility
import EFA.Equation.System ((?=))

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Graph.Topology.Index as Idx
import EFA.Utility (Pointed)

import qualified Data.NonEmpty as NonEmpty
import Control.Applicative (Applicative, pure)



data Extruder f a =
   Extruder {
      extrudeOuter :: f (Result a) -> Record.ExtDelta f (Result a),
      extrudeInner ::
         (a -> f (Result a)) ->
         a -> a -> Record.ExtDelta f (Result a)
   }

extrudeStart :: InnerExtrusion Record.Absolute a
extrudeStart = InnerExtrusion $ Record.Absolute . Determined


beforeDelta :: (Applicative f, Arith.Sum a) => Extruder f a
beforeDelta =
   Extruder {
      extrudeOuter = \x ->
         Record.ExtDelta {
            Record.extBefore = x,
            Record.extAfter = pure Undetermined,
            Record.extDelta = fmap (fmap Arith.clear) x
         },
      extrudeInner = \cons x y ->
         Record.ExtDelta {
            Record.extBefore = cons x,
            Record.extAfter = pure Undetermined,
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
            Record.extDelta = pure Undetermined
         },
      extrudeInner = \cons x y ->
         Record.ExtDelta {
            Record.extBefore = cons x,
            Record.extAfter = cons y,
            Record.extDelta = pure Undetermined
         }
   }

afterDelta :: (Applicative f, Arith.Sum a) => Extruder f a
afterDelta =
   Extruder {
      extrudeOuter = \x ->
         Record.ExtDelta {
            Record.extBefore = pure Undetermined,
            Record.extAfter = x,
            Record.extDelta = fmap (fmap Arith.clear) x
         },
      extrudeInner = \cons x y ->
         Record.ExtDelta {
            Record.extBefore = pure Undetermined,
            Record.extAfter = cons x,
            Record.extDelta = cons y
         }
   }


newtype
   InnerExtrusion f a =
      InnerExtrusion {runInnerExtrusion :: a -> f (Result a)}
newtype
   OuterExtrusion f a =
      OuterExtrusion {runOuterExtrusion :: a -> a -> f (Result a)}


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
         getAbsoluteRecord :: InnerExtrusion f a,
         getParameterRecord :: g (OuterExtrusion f a)
      }

parameterStart :: ParameterRecord NonEmpty.Empty Record.Absolute a
parameterStart = ParameterRecord extrudeStart NonEmpty.Empty

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
   idx node -> rec (Result t)

parameterSymbol param idx =
   runOuterExtrusion param
      (Utility.symbol (Idx.before $ Var.index idx))
      (Utility.symbol (Idx.delta $ Var.index idx))

absoluteSymbol ::
   (Pointed term,
    t ~ Utility.VarTerm var Idx.Delta term node,
    Eq t, Arith.Sum t, Arith.Constant t,
    Ord (idx node),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   InnerExtrusion rec t ->
   idx node -> rec (Result t)

absoluteSymbol absolute idx =
   absoluteRecord absolute (Utility.symbol (Idx.before $ Var.index idx))

parameterRecord ::
   (Arith.Sum x) =>
   OuterExtrusion rec x ->
   x -> x -> rec (Result x)
parameterRecord = runOuterExtrusion

absoluteRecord ::
   (Arith.Sum x) =>
   InnerExtrusion rec x ->
   x -> rec (Result x)
absoluteRecord = runInnerExtrusion



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
