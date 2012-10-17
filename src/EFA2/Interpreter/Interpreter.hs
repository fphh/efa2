{-# LANGUAGE TypeFamilies #-}
module EFA2.Interpreter.Interpreter where

import qualified Data.Map as M
import qualified Data.List as L

import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Data as D
import qualified EFA2.Signal.Base as Base
import EFA2.Signal.Signal (toConst, (.+), (.*))
import EFA2.Signal.Typ (Typ, UT)

import EFA2.Solver.Equation (AbsAssign(GivenIdx, (::=)), EqTerm, Term(..), showIdx)
import EFA2.Interpreter.InTerm (InTerm(..), InEquation(..))
import EFA2.Interpreter.Env as Env
import EFA2.Utils.Utils (safeLookup)


eqToInTerm :: Show a => Envs a -> AbsAssign -> InEquation a
eqToInTerm envs (GivenIdx t) =
   InEqual t $
   case t of
      Energy idx -> InGiven (energyMap envs `safeLookup` idx)
      DEnergy idx -> InGiven (denergyMap envs `safeLookup` idx)
      Power idx -> InGiven (powerMap envs `safeLookup` idx)
      DPower idx -> InGiven (dpowerMap envs `safeLookup` idx)
      FEta idx -> InFunc (fetaMap envs `safeLookup` idx)
      DEta idx -> InFunc (detaMap envs `safeLookup` idx)
{-
      (DEta idx := ((FEta x) :+ (Minus (FEta y)))) -> InEqual (DEta idx) (InFunc (\z -> fx z .- fy z))
          where fx = fetaMap envs `safeLookup` x
                fy = fetaMap envs `safeLookup` y
-}
      DTime idx -> InGiven (dtimeMap envs `safeLookup` idx)
      X idx -> InGiven (xMap envs `safeLookup` idx)
      DX idx -> InGiven (dxMap envs `safeLookup` idx)
      Var idx -> InGiven (varMap envs `safeLookup` idx)
      Store idx -> InGiven (storageMap envs `safeLookup` idx)

eqToInTerm _envs (x ::= y) =
   InEqual x (eqTermToInTerm y)

eqTermToInTerm :: EqTerm -> InTerm a
eqTermToInTerm term =
   case term of
      (Const x) -> InConst x
      (Atom x) -> InIndex x

      (Recip x) -> InRecip (eqTermToInTerm x)
      (Minus x) -> InMinus (eqTermToInTerm x)
{-
      (FEdge x y) -> InFEdge (InIndex x) (InIndex y)
      (BEdge x y) -> InBEdge (InIndex x) (InIndex y)
      (NEdge x y) -> InNEdge (InIndex x) (InIndex y)
-}
      (x :+ y) -> InAdd (eqTermToInTerm x) (eqTermToInTerm y)
      (x :* y) -> InMult (eqTermToInTerm x) (eqTermToInTerm y)


showInTerm :: (Show a) => InTerm a -> String

showInTerm (InIndex x) = showIdx x
showInTerm (InConst x) = show (fromRational x :: Double) -- take 20 (show x) ++ "..."
showInTerm (InGiven xs) = "given " ++ show xs
showInTerm (InFunc _) = "given <function>"
showInTerm (InMinus t) = "-(" ++ showInTerm t ++ ")"
showInTerm (InRecip t) = "1/(" ++ showInTerm t ++ ")"

{-
showInTerm (InFEdge s t) = "f(" ++ showInTerm s ++ ", " ++ showInTerm t ++ ")"
showInTerm (InBEdge s t) = "b(" ++ showInTerm s ++ ", " ++ showInTerm t ++ ")"
showInTerm (InNEdge s t) = "n(" ++ showInTerm s ++ ", " ++ showInTerm t ++ ")"
-}

showInTerm (InAdd s t) = "(" ++ showInTerm s ++ " + " ++ showInTerm t ++ ")"
showInTerm (InMult s t) = showInTerm s ++ " * " ++ showInTerm t


showInEquation :: (Show a) => InEquation a -> String
showInEquation (InEqual s t) = showIdx s ++ " = " ++ showInTerm t

showInTerms :: (Show a) => [InTerm a] -> String
showInTerms ts = L.intercalate "\n" $ map showInTerm ts



type Signal s c a = S.TC s (Typ UT UT UT) (D.Data c a)

interpretRhs ::
   (Show v, v ~ D.Apply c a, D.ZipWith c,
    D.Storage c a, S.Const s c, S.Arith s s ~ s,
    Fractional a, Base.DArith0 a, Base.BSum a, Base.BProd a a) =>
   Int ->
   Envs (Signal s c a) ->
   InTerm (Signal s c a) ->
   Signal s c a
interpretRhs len envs term = interpretRhs' term
  where --interpretRhs' (InConst x) = S.fromVal len [x] -- Wichtig für delta Rechnung?
        --interpretRhs' (InGiven xs) = S.map (:[]) xs
        --interpretRhs' (InConst x) = S.fromVal len x
        interpretRhs' (InConst x) = toConst len $ fromRational x
        --interpretRhs' (InConst x) = toScalar (InConst x)

        interpretRhs' (InGiven xs) = xs
        interpretRhs' (InIndex i) =
           case i of
              Energy idx -> energyMap envs `safeLookup` idx
              DEnergy idx -> denergyMap envs `safeLookup` idx
              Power idx -> powerMap envs `safeLookup` idx
              DPower idx -> dpowerMap envs `safeLookup` idx
              FEta idx@(FEtaIdx s r f t) -> (fetaMap envs `safeLookup` idx) (powerMap envs `safeLookup` pidx)
                where pidx = PowerIdx s r f t
--              DEta idx -> detaMap envs `safeLookup` idx
              DEta idx@(DEtaIdx s r f t) -> (detaMap envs `safeLookup` idx) (powerMap envs `safeLookup` pidx)
                where pidx = PowerIdx s r f t
              DTime idx -> dtimeMap envs `safeLookup` idx
              X idx -> xMap envs `safeLookup` idx
              DX idx -> dxMap envs `safeLookup` idx
              Var idx -> varMap envs `safeLookup` idx
              Store idx -> storageMap envs `safeLookup` idx

        interpretRhs' (InMinus t) = S.neg (interpretRhs' t)
        interpretRhs' (InRecip (InIndex (FEta idx@(FEtaIdx s r f t)))) = S.rec $ (fetaMap envs `safeLookup` idx) pval
          where pidx = PowerIdx s r t f
                pval = powerMap envs `safeLookup` pidx
        interpretRhs' (InRecip t) = S.rec (interpretRhs' t)
        interpretRhs' (InAdd s t) = (interpretRhs' s) .+ (interpretRhs' t)
        interpretRhs' (InMult s t) = (interpretRhs' s) .* (interpretRhs' t)
        interpretRhs' t = error ("interpretRhs': " ++ show t)

insert ::
   (Ord k, Show v, v ~ D.Apply c a, D.ZipWith c,
    D.Storage c a, S.Const s c, S.Arith s s ~ s,
    Fractional a, Base.DArith0 a, Base.BSum a, Base.BProd a a) =>
   Int ->
   k ->
   Envs (Signal s c a) ->
   InTerm (Signal s c a) ->
   M.Map k (Signal s c a) ->
   M.Map k (Signal s c a)
insert len idx envs rhs m = M.insert idx (interpretRhs len envs rhs) m


interpretEq ::
   (Show v, v ~ D.Apply c a, D.ZipWith c,
    D.Storage c a, S.Const s c, S.Arith s s ~ s,
    Fractional a, Base.DArith0 a, Base.BSum a, Base.BProd a a) =>
   Int ->
   Envs (Signal s c a) ->
   InEquation (Signal s c a) ->
   Envs (Signal s c a)
interpretEq len envs eq =
   case eq of
      (InEqual (Energy idx) rhs) -> envs { energyMap = insert len idx envs rhs (energyMap envs) }
      (InEqual (DEnergy idx) rhs) -> envs { denergyMap = insert len idx envs rhs (denergyMap envs) }
      (InEqual (Power idx) rhs) -> envs { powerMap = insert len idx envs rhs (powerMap envs) }
      (InEqual (DPower idx) rhs) -> envs { dpowerMap = insert len idx envs rhs (dpowerMap envs) }
      (InEqual (FEta idx) (InFunc feta)) -> envs { fetaMap = M.insert idx feta (fetaMap envs) }
{-
      (InEqual (FEta idx@(FEtaIdx s r f t) _) (InMult (InRecip (Power pidx1)) (Power pidx2))) -> envs''
  where envs' = envs { fetaMap = M.insert idx (mkEtaFunc pts) (fetaMap envs) }
        envs'' = envs' { fetaMap = M.insert (FEtaIdx s r t f) (mkEtaFunc (reversePts pts)) (fetaMap envs') }
        p1 = powerMap envs M.! pidx1
        p2 = powerMap envs M.! pidx2
        pts = Pt p2 (p2 ./ p1)
-}
--      (InEqual (DEta idx) rhs) -> envs { detaMap = insert len idx envs rhs (detaMap envs) }
      (InEqual (DEta idx) (InFunc deta)) -> envs { detaMap = M.insert idx deta (detaMap envs) }
      (InEqual (DTime idx) rhs) -> envs { dtimeMap = insert len idx envs rhs (dtimeMap envs) }
      (InEqual (X idx) rhs) -> envs { xMap = insert len idx envs rhs (xMap envs) }
      (InEqual (DX idx) rhs) -> envs { dxMap = insert len idx envs rhs (dxMap envs) }
      (InEqual (Var idx) rhs) -> envs { varMap = insert len idx envs rhs (varMap envs) }
      (InEqual (Store idx) rhs) -> envs { storageMap = insert len idx envs rhs (storageMap envs) }

      _ -> error ("interpretEq: " ++ showInEquation eq)


interpretFromScratch ::
   (Show v, v ~ D.Apply c a, D.ZipWith c,
    D.Storage c a, S.Const s c, S.Arith s s ~ s,
    Fractional a, Base.DArith0 a, Base.BSum a, Base.BProd a a) =>
   RecordNumber ->
   Int ->
   [InEquation (Signal s c a)] ->
   Envs (Signal s c a)
interpretFromScratch rec len ts = (L.foldl' (interpretEq len) emptyEnv ts) { recordNumber = rec }


interpretWithEnv ::
   (Show v, v ~ D.Apply c a, D.ZipWith c,
    D.Storage c a, S.Const s c, S.Arith s s ~ s,
    Fractional a, Base.DArith0 a, Base.BSum a, Base.BProd a a) =>
   Int ->
   Envs (Signal s c a) ->
   InTerm (Signal s c a) ->
   Signal s c a
interpretWithEnv len envs t = interpretRhs len envs t
