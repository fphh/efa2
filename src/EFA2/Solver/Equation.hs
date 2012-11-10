module EFA2.Solver.Equation where

import EFA2.Interpreter.Env as Env
import EFA2.Signal.Index (toDiffUse)
import qualified EFA2.Signal.Index as Idx

import Control.Monad (liftM2)

import qualified Data.Ratio as Ratio
import Data.Ratio (Ratio, (%))
import Data.Maybe.HT (toMaybe)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Stream as Stream
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Data.Stream (Stream)

import Debug.Trace (trace)
import Text.Printf (printf)


-- TOTHINK: Die Algorithmen aus dem Verzeichnis Solver sollten
-- über den Datentyp EqTerm parametrisierbar sein. Die Abhängigkeitsanalyse
-- sollte nichts mit den konkreten Termen zu tun haben. Diese Entscheidung
-- haette wahrscheinlich auch Einfluss auf InVar...

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
      in  go

infixl 1 !=, !:=, :=, ::=
infixl 7  !*, :*
infixl 6  !+, :+

(!+) :: (MkTermC a, MkTermC b) => a -> b -> EqTerm
x !+ y = mkTerm x :+ mkTerm y

(!*) :: (MkTermC a, MkTermC b) => a -> b -> EqTerm
x !* y = mkTerm x :* mkTerm y

(!=) :: (MkTermC a, MkTermC b) => a -> b -> Equation
x != y = mkTerm x := mkTerm y

(!:=) :: (MkIdxC a, MkTermC b) => a -> b -> AbsAssign
x !:= y = mkIdx x ::= mkTerm y

give :: MkIdxC a => a -> Equation
give = Given . mkIdx


class MkIdxC a where
   mkIdx :: a -> Env.Index

instance MkIdxC Idx.Energy where mkIdx = Energy
instance MkIdxC Idx.DEnergy where mkIdx = DEnergy
instance MkIdxC Idx.Power where mkIdx = Power
instance MkIdxC Idx.DPower where mkIdx = DPower
instance MkIdxC Idx.FEta where mkIdx = FEta
instance MkIdxC Idx.DEta where mkIdx = DEta
instance MkIdxC Idx.DTime where mkIdx = DTime
instance MkIdxC Idx.X where mkIdx = X
instance MkIdxC Idx.DX where mkIdx = DX
instance MkIdxC Idx.Var where mkIdx = Var
instance MkIdxC Idx.Storage where mkIdx = Store


class MkVarC a where
   mkVarCore :: Env.Index -> a

instance MkVarC Env.Index where
   mkVarCore = id

instance MkVarC a => MkVarC (Term a) where
   mkVarCore = Atom . mkVarCore

mkVar :: (MkIdxC a, MkVarC b) => a -> b
mkVar = mkVarCore . mkIdx


class MkTermC a where
   mkTerm :: a -> EqTerm

instance MkTermC Idx.Energy where mkTerm = mkVar
instance MkTermC Idx.DEnergy where mkTerm = mkVar
instance MkTermC Idx.Power where mkTerm = mkVar
instance MkTermC Idx.DPower where mkTerm = mkVar
instance MkTermC Idx.FEta where mkTerm = mkVar
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


add :: NonEmpty.T [] (Term a) -> Term a
add = NonEmpty.foldl1 (:+)

mult :: NonEmpty.T [] (Term a) -> Term a
mult = NonEmpty.foldl1 (:*)


showEdgeIdx :: Idx.Section -> Idx.Record -> Int -> Int -> String
showEdgeIdx (Idx.Section s) (Idx.Record r) x y =
   show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y

showIdx :: ToIndex idx => idx -> String
showIdx idx =
   case toIndex idx of
      Energy (Idx.Energy s r x y) -> "E_" ++ showEdgeIdx s r x y
      DEnergy (Idx.DEnergy s r x y) -> "dE_" ++ showEdgeIdx s r x y

      Power (Idx.Power s r x y) -> "P_" ++ showEdgeIdx s r x y
      DPower (Idx.DPower s r x y) -> "dP_" ++ showEdgeIdx s r x y

      FEta (Idx.FEta s r x y) -> "n_" ++ showEdgeIdx s r x y
      DEta (Idx.DEta s r x y) -> "dn_" ++ showEdgeIdx s r x y

      DTime (Idx.DTime (Idx.Section s) (Idx.Record r)) ->
         "dt_" ++ show s ++ "." ++ show r

      X (Idx.X s r x y) -> "x_" ++ showEdgeIdx s r x y
      DX (Idx.DX s r x y) -> "dx_" ++ showEdgeIdx s r x y

      Var (Idx.Var (Idx.Section s) (Idx.Record r) u x) ->
         "v_" ++ show s ++ "." ++ show r ++ "_" ++ show u ++ "." ++ show x
      Store (Idx.Storage (Idx.Section s) (Idx.Record r) n) ->
         "s_" ++ show s ++ "." ++ show r ++ "_" ++ show n

showEqTerm :: ToIndex idx => Term idx -> String
showEqTerm (Const x) = show (fromRational x :: Double)
showEqTerm (Atom x) = showIdx (toIndex x)

showEqTerm (x :+ y) = "(" ++ showEqTerm x ++ " + " ++ showEqTerm y ++ ")"
showEqTerm (x :* y) = showEqTerm x ++ " * " ++ showEqTerm y

showEqTerm (Recip x) = "1/(" ++ showEqTerm x ++ ")"
showEqTerm (Minus x) = "-(" ++ showEqTerm x ++ ")"

showEdge ::
   (ToIndex idx) =>
   EdgeUnknown -> idx -> idx -> idx -> String
showEdge e power0 eta power1 =
   case e of
      PowerOut -> showIdx power1 ++ " = f(" ++ showIdx power0 ++ ", " ++ showIdx eta ++ ")"
      PowerIn -> showIdx power0 ++ " = b(" ++ showIdx power1 ++ ", " ++ showIdx eta ++ ")"
      Eta -> showIdx eta ++ " = n(" ++ showIdx power0 ++ ", " ++ showIdx power1 ++ ")"

showEquation :: Equation -> String
showEquation (Given x) = showIdx x ++ " given"
showEquation (EqEdge p0 n p1) = showEdge PowerOut p0 n p1
showEquation (x := y) = showEqTerm x ++ " = " ++ showEqTerm y

showAbsAssign :: AbsAssign -> String
showAbsAssign (GivenIdx x) = showIdx x ++ " given"
showAbsAssign (x ::= y) = showIdx x ++ " = " ++ showEqTerm y

showAssign :: Assign -> String
showAssign (AbsAssign assign) = showAbsAssign assign
showAssign (AssignEdge e p0 n p1) = showEdge e p0 n p1

showEqTerms :: ToIndex idx => [Term idx] -> String
showEqTerms ts = L.intercalate "\n" $ map showEqTerm ts

showEquations :: [Equation] -> String
showEquations ts = L.intercalate "\n" $ map showEquation ts

showAssigns :: [Assign] -> String
showAssigns ts = L.intercalate "\n" $ map showAssign ts


newtype LatexString = LatexString { unLatexString :: String } deriving (Show, Eq)


toLatexString' :: EqTerm -> String
toLatexString' (Const x) = printf "%.6f   " (fromRational x :: Double)
toLatexString' (Atom x) = idxToLatexString x

toLatexString' (x :+ y) = "(" ++ toLatexString' x ++ " + " ++ toLatexString' y ++ ")"
toLatexString' (x :* y) = toLatexString' x ++ " * " ++ toLatexString' y

toLatexString' (Recip x) = "\\frac{1}{" ++ toLatexString' x ++ "}"
toLatexString' (Minus x) = "-(" ++ toLatexString' x ++ ")"

edgeToLatexString ::
   EdgeUnknown -> Index -> Index -> Index -> String
edgeToLatexString e power0 eta power1 =
   case e of
      PowerOut -> idxToLatexString power1 ++ " = f(" ++ idxToLatexString power0 ++ ", " ++ idxToLatexString eta ++ ")"
      PowerIn -> idxToLatexString power0 ++ " = b(" ++ idxToLatexString power1 ++ ", " ++ idxToLatexString eta ++ ")"
      Eta -> idxToLatexString eta ++ " = n(" ++ idxToLatexString power0 ++ ", " ++ idxToLatexString power1 ++ ")"


edgeIdxToLatexString :: Idx.Section -> Idx.Record -> Int -> Int -> String
edgeIdxToLatexString (Idx.Section s) (Idx.Record r) x y =
   "{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"

idxToLatexString :: Env.Index -> String
idxToLatexString idx =
   case idx of
      Energy (Idx.Energy s r x y) -> "E_{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"
      DEnergy (Idx.DEnergy s r x y) -> "\\Delta E_" ++ edgeIdxToLatexString s r x y

      Power (Idx.Power s r x y) -> "P_" ++ edgeIdxToLatexString s r x y
      DPower (Idx.DPower s r x y) -> "\\Delta P_" ++ edgeIdxToLatexString s r x y

      FEta (Idx.FEta s r x y) -> "\\eta_" ++ edgeIdxToLatexString s r x y
      DEta (Idx.DEta s r x y) -> "\\Delta \\eta_" ++ edgeIdxToLatexString s r x y

      DTime (Idx.DTime (Idx.Section s) (Idx.Record r)) -> "\\Delta t_{" ++ show s ++ "." ++ show r ++ "}"

      X (Idx.X s r x y) -> "x_" ++ edgeIdxToLatexString s r x y
      DX (Idx.DX s r x y) -> "\\Delta x_" ++ edgeIdxToLatexString s r x y

      Var (Idx.Var (Idx.Section s) (Idx.Record r) u x) ->
         "v_{" ++ show s ++ "." ++ show r ++ "." ++ show u ++ "." ++ show x ++ "}"
      Store (Idx.Storage (Idx.Section s) (Idx.Record r) n) ->
         "s_{" ++ show s ++ "." ++ show r ++ "." ++ show n ++ "}"

eqToLatexString' :: Equation -> String
eqToLatexString' (Given x) = idxToLatexString x ++ " \\mbox{given}"
eqToLatexString' (EqEdge p0 n p1) = edgeToLatexString PowerOut p0 n p1
eqToLatexString' (x := y) = toLatexString' x ++ " = " ++ toLatexString' y

toLatexString :: EqTerm -> LatexString
toLatexString t = LatexString $ "$" ++ toLatexString' t ++ "$"

eqToLatexString :: Equation -> LatexString
eqToLatexString t = LatexString $ "$" ++ eqToLatexString' t ++ "$"


-- | This function takes a predicate p that determines, wether
-- a term is a variable or not. It then takes a term and
-- determines the set of variables contained in the term,
-- according to the predicate.
mkVarSetEq :: (Ord a) => (EqTerm -> Maybe a) -> Equation -> S.Set a
mkVarSetEq p (Given x) = mkVarSet p $ Atom x
mkVarSetEq p (EqEdge p0 n p1) =
   S.unions $ map (mkVarSet p) $ Atom p0 : Atom n : Atom p1 : []
mkVarSetEq p (x := y) = S.union (mkVarSet p x) (mkVarSet p y)

mkVarSet :: (Ord a) => (EqTerm -> Maybe a) -> EqTerm -> S.Set a
mkVarSet p = mkVarSet'
   where
      mkVarSet' t =
         case p t of
            Just v -> S.singleton v
            Nothing ->
               case t of
                  -- fn@(FEta _) -> S.insert fn (mkVarSet' p)
                  x :+ y -> S.union (mkVarSet' x) (mkVarSet' y)
                  x :* y -> S.union (mkVarSet' x) (mkVarSet' y)
                  Minus x -> mkVarSet' x
                  Recip x -> mkVarSet' x
                  _ -> S.empty


-- The following functions transform an equation.

data Dir = L | R deriving (Show, Eq)

type TPath = [Dir]


prepStep ::
   (Show eq0, Show eq1) =>
   eq0 -> eq1 -> (Maybe TPath, Maybe TPath) -> Maybe TPath
prepStep t s p =
   case p of
      (Nothing, x) -> fmap (R:) x
      (x, Nothing) -> fmap (L:) x
      _ -> error $ "error in looking for path to (" ++ show t ++ ") in (" ++ show s ++ ")"

findVarEq :: Env.Index -> Equation -> Maybe TPath
findVarEq t s =
   prepStep t s $
   case s of
      Given u -> (findIndex t u, Nothing)
      u := v  -> (findVar t u, findVar t v)
      EqEdge p0 n p1 ->
         (findIndex t p1,
          prepStep t s (findIndex t p0, findIndex t n))

findIndex :: Env.Index -> Env.Index -> Maybe TPath
findIndex t s = toMaybe (t == s) []

findVar :: Env.Index -> EqTerm -> Maybe TPath
findVar t s =
   if Atom t == s
     then Just []
     else prepStep t s $
        case s of
           (u :+ v) -> (findVar t u, findVar t v)
           (u :* v) -> (findVar t u, findVar t v)
           (Minus u) -> (findVar t u, Nothing)    -- coding: Minus has only left operand.
           (Recip u) -> (findVar t u, Nothing)    -- coding: Recip has only left operand.
           _ -> (Nothing, Nothing)

isolateVar :: Env.Index -> Equation -> TPath -> Assign
isolateVar s (Given _u) [L] = AbsAssign (GivenIdx s)
isolateVar s (Given u) [R] = AbsAssign (s ::= Atom u)
isolateVar s (u := v) (L:p) = AbsAssign (s ::= isolateVar' u p v)
isolateVar s (u := v) (R:p) = AbsAssign (s ::= isolateVar' v p u)
{-
isolateVar s (EqEdge p0 n p1) p =
   case p of
      [L]   -> AssignEdge PowerOut p0 n s
      [R,L] -> AssignEdge PowerIn s n p1
      [R,R] -> AssignEdge Eta p0 s p1
-}
isolateVar s (EqEdge p0 n p1) p =
   (\e -> AssignEdge e p0 n p1) $
   case p of
      [L]   -> if s==p1 then PowerOut else error "isolateVar PowerOut"
      [R,L] -> if s==p0 then PowerIn  else error "isolateVar PowerIn"
      [R,R] -> if s==n  then Eta      else error "isolateVar Eta"
isolateVar s t p = error $ "isolateVar:\n" ++ show s ++ "\n" ++ show t ++ "\n" ++ show p


isolateVar' :: EqTerm -> TPath -> (EqTerm -> EqTerm)
-- the underscore should equal the 's' variable from isolateVar
-- we might check this at runtime
isolateVar' _ [] = id
isolateVar' (u :+ v) (L:p) = isolateVar' u p . ((Minus v) :+)
isolateVar' (u :+ v) (R:p) = isolateVar' v p . ((Minus u) :+)

isolateVar' (u :* v) (L:p) = isolateVar' u p . ((Recip v) :*)
isolateVar' (u :* v) (R:p) = isolateVar' v p . ((Recip u) :*)

isolateVar' (Minus u) (L:p) = isolateVar' u p . Minus
isolateVar' (Recip u) (L:p) = isolateVar' u p . Recip

{-
isolateVar' (FDiff p' e dp de) (L:p) = isolateVar' dp p . f
  where f x@(DEnergy (Idx.DEnergy s r a b)) = BDiff (Energy (Idx.Energy s r a b)) e x de
isolateVar' (BDiff p' e dp de) (L:p) = isolateVar' dp p . f
  where f x@(DEnergy (Idx.DEnergy s r a b)) = FDiff (Energy (Idx.Energy s r a b)) e x de
-}

-- this is the main function for transforming Equations
-- It takes an unknown variable and an equation.
-- The resulting equation has
-- the unknown variable isolated on its left hand side (lhs),
-- such that we can evaluate the rhs in order to calculate
-- the value of the unknown variable.
transformEq :: Env.Index -> Equation -> Assign
transformEq unknown t =
   maybe
      (error $ "transformEq: did not find " ++ show unknown)
      (isolateVar unknown t) $
   findVarEq unknown t

--------------------------------------------------------------------


pushMult :: Term a -> Term a
pushMult t = add $ pushMult' t

{-
pushMult :: EqTerm -> EqTerm
pushMult t =
   case t of
      u := v  ->  add (pushMult' u) := add (pushMult' v)
      _       ->  add (pushMult' t)
-}

pushMult' :: Term a -> NonEmpty.T [] (Term a)
pushMult' (Minus u) = fmap Minus (pushMult' u)
pushMult' (Recip u) = NonEmpty.singleton $ Recip $ pushMult u
pushMult' (u :+ v) = NonEmpty.append (pushMult' u) (pushMult' v)
pushMult' (u :* v) = liftM2 (:*) (pushMult' u) (pushMult' v)
pushMult' t = NonEmpty.singleton t

streamPairs :: Stream a -> Stream (a, a)
streamPairs xs = Stream.zip xs (Stream.tail xs)

iterateUntilFix :: (Eq a) => (a -> a) -> a -> a
iterateUntilFix f =
   fst . Stream.head . Stream.dropWhile (uncurry (/=)) .
   streamPairs . Stream.iterate f

simplify :: Eq a => Term a -> Term a
simplify = iterateUntilFix simplify' . pushMult
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

{-
simplifyEq :: Equation -> Equation
simplifyEq (Given x) = Given x
simplifyEq (x := y) = simplify x := simplify y
-}

additiveTerms :: EqTerm -> [EqTerm]
additiveTerms = NonEmpty.flatten . additiveTermsNonEmpty

additiveTermsNonEmpty :: EqTerm -> NonEmpty.T [] EqTerm
additiveTermsNonEmpty = recourse . simplify . pushMult
  where recourse (x :+ y) =
           NonEmpty.append (recourse x) (recourse y)
        recourse t = NonEmpty.singleton t


setEqTerms :: Envs rec EqTerm -> EqTerm -> EqTerm
setEqTerms envs term =
   case term of
      Atom i ->
         case i of
            Power idx -> M.findWithDefault term idx (powerMap envs)
            DPower idx -> M.findWithDefault term idx (dpowerMap envs)
            Energy idx -> M.findWithDefault term idx (energyMap envs)
            DEnergy idx -> M.findWithDefault term idx (denergyMap envs)
            FEta idx -> maybe term (\t -> trace "setEqTerms" $ t undefined) $ M.lookup idx (fetaMap envs)
            DEta idx -> maybe term (\t -> trace "setEqTerms" $ t undefined) $ M.lookup idx (detaMap envs)
            X idx -> M.findWithDefault term idx (xMap envs)
            DX idx -> M.findWithDefault term idx (dxMap envs)
            Var idx -> M.findWithDefault term idx (varMap envs)
            Store idx -> M.findWithDefault term idx (storageMap envs)
            DTime idx -> M.findWithDefault term idx (dtimeMap envs)
      (Minus t) -> Minus (setEqTerms envs t)
      (Recip t) -> Recip (setEqTerms envs t)
      (s :+ t) -> setEqTerms envs s :+ setEqTerms envs t
      (s :* t) -> setEqTerms envs s :* setEqTerms envs t
      _ -> term

--------------------------------------------------------------------

toAbsEquation :: Assign -> AbsAssign
toAbsEquation (AbsAssign assign) = assign
toAbsEquation (AssignEdge e p0 n p1)  =
   case e of
      PowerOut -> p1 ::= mkTerm p0 :* mkTerm n
      PowerIn  -> p0 ::= mkTerm p1 :* Recip (mkTerm n)
      Eta      -> n  ::= mkTerm p0 :* Recip (mkTerm p1)

toAbsEquations :: [Assign] -> [AbsAssign]
toAbsEquations = map toAbsEquation


assignToEquation :: Assign -> Equation
assignToEquation (AssignEdge _e p0 n p1)  =  EqEdge p0 n p1
assignToEquation (AbsAssign assign) =
   case assign of
      GivenIdx x  ->  Given x
      x ::= y  ->  Atom x := y


mkDiffEqTerm :: Idx.Record -> Assign -> [AbsAssign]

-- v_0.1_OutSum.0 = P_0.1_0.1
mkDiffEqTerm _ (AbsAssign (Var (Idx.Var s r use n) ::= Atom (Power (Idx.Power _ _ f t)))) =
  {- trace ("--->: " ++ showEqTerm z ++ " s=> " ++ showEqTerm eres) $ -} [res, eres]
  where res = v !:= dp
        v = Idx.Var s r (toDiffUse use) n
        dp = Idx.DPower s r f t

        eres = de !:= dp !* dt
        dt = Idx.DTime s r
        de = Idx.DEnergy s r f t



-- v_0.1_OutSum.1 = (P_0.1_1.2 + P_0.1_1.3) ...
mkDiffEqTerm _ (AbsAssign (Var (Idx.Var s r use n) ::= as@(_x :+ _y))) =
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm res) $ -} [res]
  where res = v ::= dps
        ats = additiveTermsNonEmpty as
        v = mkVar $ Idx.Var s r (toDiffUse use) n
        dps = add $ fmap g ats
        g (Atom (Power (Idx.Power _ _ f t))) = mkVar $ Idx.DPower s r f t

-- P_0.1_0.1 = v_0.1_OutSum.0
mkDiffEqTerm _ (AbsAssign (Power (Idx.Power _ _ f t) ::= Atom (Var (Idx.Var s r use n)))) =
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm eres) $ -} [res, eres]
  where res = dp ::= v
        v = mkVar $ Idx.Var s r (toDiffUse use) n
        dp = mkVar $ Idx.DPower s r f t

        eres = de !:= dp !* dt
        dt = Idx.DTime s r
        de = Idx.DEnergy s r f t


-- v_0.1_OutSum.1 = v_0.1_InSum.1
mkDiffEqTerm _ (AbsAssign (Var (Idx.Var s r use0 n) ::= Atom (Var (Idx.Var _ _ use1 _)))) =
  {- trace (showEqTerm z ++ " => " ++ showEqTerm res) $ -} [res]
  where res = v0 ::= v1
        v0 = mkVar $ Idx.Var s r (toDiffUse use0) n
        v1 = mkVar $ Idx.Var s r (toDiffUse use1) n

-- P_0.1_1.0 = f(P_0.1_0.1, n_0.1_0.1)
mkDiffEqTerm oldrec (AssignEdge PowerOut (Power _) _ (Power (Idx.Power s newrec f t))) =
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm eres) $ -} [res, eres]
  where res = dq ::= (dp :* n) :+ (p :* dn) :+ (dp :* dn)
        dq = mkVar $ Idx.DPower s newrec f t
        dp = mkVar $ Idx.DPower s newrec t f
        n = mkVar $ Idx.FEta s oldrec t f
        dn = mkVar $ Idx.DEta s newrec t f
        p = mkVar $ Idx.Power s oldrec t f

        eres = de !:= dq !* dt
        dt = Idx.DTime s newrec
        de = Idx.DEnergy s newrec f t

-- P_0.1_1.2 = f(v_0.1_OutSum.1, x_0.1_1.2)
mkDiffEqTerm oldrec (AssignEdge PowerOut (Var (Idx.Var _ _ use _)) _ (Power (Idx.Power s newrec f t))) =
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm eres) $ -} [res, eres]
  where res = dq ::= (dv :* x) :+ (v :* dx) :+ (dv :* dx)
        dq = mkVar $ Idx.DPower s newrec f t
        dv = mkVar $ Idx.Var s newrec (toDiffUse use) f
        x = mkVar $ Idx.X s oldrec f t
        dx = mkVar $ Idx.DX s newrec f t
        v = mkVar $ Idx.Var s oldrec use f

        eres = de !:= dq !* dt
        dt = Idx.DTime s newrec
        de = Idx.DEnergy s newrec f t

-- P_0.1_1.2 = b(P_0.1_2.1, n_0.1_1.2)
mkDiffEqTerm oldrec (AssignEdge PowerIn (Power (Idx.Power s newrec f t)) (FEta _) _) =
  {- trace (showEqTerm z ++ " => " ++ showEqTerm res) $ -} [res, eres]
  where res = dq ::= (dp :* (Recip n)) :+ (Minus ((p :* dn) :* nom)) :+ (Minus ((dp :* dn) :* nom))
        dq = mkVar $ Idx.DPower s newrec f t
        dp = mkVar $ Idx.DPower s newrec t f
        p = mkVar $ Idx.Power s oldrec t f
        n = mkVar $ Idx.FEta s oldrec f t
        dn = mkVar $ Idx.DEta s newrec f t
        nom = Recip ((dn :* n) :+ (n :* n))

        eres = de !:= dq !* dt
        dt = Idx.DTime s newrec
        de = Idx.DEnergy s newrec f t


-- v_0.1_OutSum.1 = b(P_0.1_1.2, x_0.1_1.2)
mkDiffEqTerm oldrec (AssignEdge PowerIn (Var (Idx.Var s newrec use _n)) _ (Power (Idx.Power _ _ f t))) =
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm eres) $ -} [res, eres]
  where res = v ::= (dp :* (Recip x)) :+ (Minus ((p :* dx) :* nom)) :+ (Minus ((dp :* dx) :* nom))
        v = mkVar $ Idx.Var s newrec (toDiffUse use) f
        p = mkVar $ Idx.Power s oldrec f t
        dp = mkVar $ Idx.DPower s newrec f t
        x = mkVar $ Idx.X s oldrec f t
        dx = mkVar $ Idx.DX s newrec f t
        nom = Recip ((dx :* x) :+ (x :* x))

        eres = de ::= dq :* dt
        dq = mkVar $ Idx.DPower s newrec f t
        dt = mkVar $ Idx.DTime s newrec
        de = mkVar $ Idx.DEnergy s newrec f t

mkDiffEqTerm _ _ = []


mkDiffEqTermEquations :: Idx.Record -> [Assign] -> [Assign]
mkDiffEqTermEquations rec =
   map AbsAssign . concatMap (mkDiffEqTerm rec)

--------------------------------------------------------------------
-- interpretEq len envs (InEqual (Idx.E idx) rhs) = envs { energyMap = insert len idx envs rhs (energyMap envs) }

interpretEqTermRhs :: Envs rec EqTerm -> EqTerm -> EqTerm
interpretEqTermRhs envs t =
   case t of
      Atom i ->
         case i of
            Power idx -> M.findWithDefault t idx (powerMap envs)
            DPower idx -> M.findWithDefault t idx (dpowerMap envs)
            Energy idx -> M.findWithDefault t idx (energyMap envs)
            DEnergy idx -> M.findWithDefault t idx (denergyMap envs)
            FEta idx -> maybe t ($undefined) $ M.lookup idx (fetaMap envs)
            DEta idx -> maybe t ($undefined) $ M.lookup idx (detaMap envs)
            Var idx -> M.findWithDefault t idx (varMap envs)
            X idx -> M.findWithDefault t idx (xMap envs)
            DX idx -> M.findWithDefault t idx (dxMap envs)
            Store idx -> M.findWithDefault t idx (storageMap envs)
            DTime idx -> M.findWithDefault t idx (dtimeMap envs)
      (Minus x) -> Minus $ interpretEqTermRhs envs x
      (Recip x) -> Recip $ interpretEqTermRhs envs x
      (x :+ y) -> interpretEqTermRhs envs x :+ interpretEqTermRhs envs y
      (x :* y) -> interpretEqTermRhs envs x :* interpretEqTermRhs envs y
      _ -> t

insertEqTerm ::
   Ord k =>
   k -> Envs rec EqTerm -> EqTerm -> M.Map k EqTerm -> M.Map k EqTerm
insertEqTerm idx envs rhs m = M.insert idx (interpretEqTermRhs envs rhs) m

interpretEqTermEq :: Envs rec EqTerm -> AbsAssign -> Envs rec EqTerm
interpretEqTermEq envs (GivenIdx x) =
 case Atom x of
  t ->
   case x of
      Power idx -> envs { powerMap = insertEqTerm idx envs t (powerMap envs) }
      DPower idx -> envs { dpowerMap = insertEqTerm idx envs t (dpowerMap envs) }
      Energy idx -> envs { energyMap = insertEqTerm idx envs t (energyMap envs) }
      DEnergy idx -> envs { denergyMap = insertEqTerm idx envs t (denergyMap envs) }
      FEta idx -> envs { fetaMap = M.insert idx (const t) (fetaMap envs) }
      DEta idx -> envs { detaMap = M.insert idx (const t) (detaMap envs) }
      Var idx -> envs { varMap = insertEqTerm idx envs t (varMap envs) }
      X idx -> envs { xMap = insertEqTerm idx envs t (xMap envs) }
      DX idx -> envs { dxMap = insertEqTerm idx envs t (dxMap envs) }
      Store idx -> envs { storageMap = insertEqTerm idx envs t (storageMap envs) }
      DTime idx -> envs { dtimeMap = insertEqTerm idx envs t (dtimeMap envs) }

interpretEqTermEq envs (t ::= rhs) =
   case t of
      Power idx -> envs { powerMap = insertEqTerm idx envs rhs (powerMap envs) }
      DPower idx -> envs { dpowerMap = insertEqTerm idx envs rhs (dpowerMap envs) }
      Energy idx -> envs { energyMap = insertEqTerm idx envs rhs (energyMap envs) }
      DEnergy idx -> envs { denergyMap = insertEqTerm idx envs rhs (denergyMap envs) }
      FEta idx -> envs { fetaMap = M.insert idx (const rhs) (fetaMap envs) }
      DEta idx -> envs { detaMap = M.insert idx (const rhs) (detaMap envs) }
      X idx -> envs { xMap = insertEqTerm idx envs rhs (xMap envs) }
      DX idx -> envs { dxMap = insertEqTerm idx envs rhs (dxMap envs) }
      Var idx -> envs { varMap = insertEqTerm idx envs rhs (varMap envs) }
      Store idx -> envs { storageMap = insertEqTerm idx envs rhs (storageMap envs) }
      DTime idx -> envs { dtimeMap = insertEqTerm idx envs rhs (dtimeMap envs) }

interpretEqTermFromScratch :: [AbsAssign] -> Envs NoRecord EqTerm
interpretEqTermFromScratch ts = L.foldl' interpretEqTermEq emptyEnv ts

mapEqTermEnv :: (a -> b) -> Envs rec a -> Envs rec b
mapEqTermEnv f env = emptyEnv { recordNumber = recordNumber env,
                                energyMap = M.map f (energyMap env),
                                denergyMap = M.map f (denergyMap env),
                                powerMap = M.map f (powerMap env),
                                dpowerMap = M.map f (dpowerMap env),
                                --fetaMap = M.map (f .) (fetaMap env),  -- geht nicht?
                                --detaMap = M.map (S.map f .) (detaMap env),
                                dtimeMap = M.map f (dtimeMap env),
                                xMap = M.map f (xMap env),
                                dxMap = M.map f (dxMap env),
                                varMap = M.map f (varMap env),
                                storageMap = M.map f (storageMap env) }

--------------------------------------------------------------------
