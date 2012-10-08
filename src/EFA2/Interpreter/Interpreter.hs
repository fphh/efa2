{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module EFA2.Interpreter.Interpreter where

import qualified Data.Map as M
import qualified Data.List as L

import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Data as D
import EFA2.Signal.Signal (toConst, (.+), (.*))
import EFA2.Signal.Typ (Typ, UT)
import EFA2.Signal.Base (Val)

import EFA2.Solver.Equation (Equation(Given, (:=)), EqTerm(..))
import EFA2.Interpreter.InTerm (InTerm(..), InEquation(..))
import EFA2.Interpreter.Env
import EFA2.Utils.Utils (safeLookup)


eqToInTerm :: Show a => Envs a -> Equation -> InEquation a
eqToInTerm envs (Given t) =
   case t of
      (Energy idx) -> InEqual (EIdx idx) (InGiven (energyMap envs `safeLookup` idx))
      (DEnergy idx) -> InEqual (DEIdx idx) (InGiven (denergyMap envs `safeLookup` idx))
      (Power idx) -> InEqual (PIdx idx) (InGiven (powerMap envs `safeLookup` idx))
      (DPower idx) -> InEqual (DPIdx idx) (InGiven (dpowerMap envs `safeLookup` idx))
      (FEta idx) -> InEqual (FNIdx idx) (InFunc (fetaMap envs `safeLookup` idx))
      (DEta idx) -> InEqual (DNIdx idx) (InFunc (detaMap envs `safeLookup` idx))
{-
      (DEta idx := ((FEta x) :+ (Minus (FEta y)))) -> InEqual (DNIdx idx) (InFunc (\z -> fx z .- fy z))
          where fx = fetaMap envs `safeLookup` x
                fy = fetaMap envs `safeLookup` y
-}
      (DTime idx) -> InEqual (DTIdx idx) (InGiven (dtimeMap envs `safeLookup` idx))
      (X idx) -> InEqual (ScaleIdx idx) (InGiven (xMap envs `safeLookup` idx))
      (DX idx) -> InEqual (DScaleIdx idx) (InGiven (dxMap envs `safeLookup` idx))
      (Var idx) -> InEqual (VIdx idx) (InGiven (varMap envs `safeLookup` idx))
      (Store idx) -> InEqual (SIdx idx) (InGiven (storageMap envs `safeLookup` idx))

eqToInTerm _envs (x := y) =
   InEqual (eqTermToInTerm x) (eqTermToInTerm y)

eqTermToInTerm :: EqTerm -> InTerm a
eqTermToInTerm term =
   case term of
      (Const x) -> InConst x
      (Energy idx) -> EIdx idx
      (DEnergy idx) -> DEIdx idx
      (Power idx) -> PIdx idx
      (DPower idx) -> DPIdx idx
      (FEta idx) -> FNIdx idx
      (DEta idx) -> DNIdx idx
      (DTime idx) -> DTIdx idx
      (X idx) -> ScaleIdx idx
      (DX idx) -> DScaleIdx idx

      (Var idx) -> VIdx idx
      (Store idx) -> SIdx idx
      (Recip x) -> InRecip (eqTermToInTerm x)
      (Minus x) -> InMinus (eqTermToInTerm x)
      (FEdge x y) -> InFEdge (eqTermToInTerm x) (eqTermToInTerm y)
      (BEdge x y) -> InBEdge (eqTermToInTerm x) (eqTermToInTerm y)
      (NEdge x y) -> InNEdge (eqTermToInTerm x) (eqTermToInTerm y)

      (FNode x y) -> InFNode (eqTermToInTerm x) (eqTermToInTerm y)
      (BNode x y) -> InBNode (eqTermToInTerm x) (eqTermToInTerm y)
      (XNode x y) -> InXNode (eqTermToInTerm x) (eqTermToInTerm y)

      (x :+ y) -> InAdd (eqTermToInTerm x) (eqTermToInTerm y)
      (x :* y) -> InMult (eqTermToInTerm x) (eqTermToInTerm y)


showInTerm :: (Show a) => InTerm a -> String
showInTerm (EIdx (EnergyIdx s r x y)) = "E:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DEIdx (DEnergyIdx s r x y)) = "dE:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (PIdx (PowerIdx s r x y)) = "P:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DPIdx (DPowerIdx s r x y)) = "dP:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (FNIdx (FEtaIdx s r x y)) = "n:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "."  ++ show y
                                   --      ++ show y ++ "(" ++ showInTerm p ++ ")"
showInTerm (DNIdx (DEtaIdx s r x y)) = "dn:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DTIdx (DTimeIdx s r)) = "dt:" ++ show s ++ "." ++ show r
showInTerm (ScaleIdx (XIdx s r x y)) = "x:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DScaleIdx (DXIdx s r x y)) = "dx:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y

showInTerm (VIdx (VarIdx s r x y)) = "v:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (SIdx (StorageIdx s r n)) = "s:" ++ show s ++ "." ++ show r ++ ":" ++ show n
showInTerm (InConst x) = show x -- take 20 (show x) ++ "..."
showInTerm (InGiven xs) = "given " ++ show xs
showInTerm (InFunc _) = "given <function>"
showInTerm (InMinus t) = "-(" ++ showInTerm t ++ ")"
showInTerm (InRecip t) = "1/(" ++ showInTerm t ++ ")"

showInTerm (InFEdge s t) = "f(" ++ showInTerm s ++ ", " ++ showInTerm t ++ ")"
showInTerm (InBEdge s t) = "b(" ++ showInTerm s ++ ", " ++ showInTerm t ++ ")"
showInTerm (InNEdge s t) = "n(" ++ showInTerm s ++ ", " ++ showInTerm t ++ ")"

showInTerm (InFNode s t) = "fn(" ++ showInTerm s ++ ", " ++ showInTerm t ++ ")"
showInTerm (InBNode s t) = "bn(" ++ showInTerm s ++ ", " ++ showInTerm t ++ ")"
showInTerm (InXNode s t) = "xn(" ++ showInTerm s ++ ", " ++ showInTerm t ++ ")"

showInTerm (InAdd s t) = "(" ++ showInTerm s ++ " + " ++ showInTerm t ++ ")"
showInTerm (InMult s t) = showInTerm s ++ " * " ++ showInTerm t

showInEquation :: (Show a) => InEquation a -> String
showInEquation (InEqual s t) = showInTerm s ++ " = " ++ showInTerm t

showInTerms :: (Show a) => [InTerm a] -> String
showInTerms ts = L.intercalate "\n" $ map showInTerm ts



type Signal s c a = S.TC s (Typ UT UT UT) (D.Data c a)

interpretRhs ::
   (Show (D.Apply c2 Val), D.ZipWith c2 c2, D.Map c2,
    D.Storage c2 Val, S.Const s2 c2, S.Arith s2 s2 ~ s2,
    D.Zip c2 c2 ~ c2) =>
   Int ->
   Envs (Signal s2 c2 Val) ->
   InTerm (Signal s2 c2 Val) ->
   Signal s2 c2 Val
interpretRhs len envs term = interpretRhs' term
  where --interpretRhs' (InConst x) = S.fromVal len [x] -- Wichtig für delta Rechnung?
        --interpretRhs' (InGiven xs) = S.map (:[]) xs
        --interpretRhs' (InConst x) = S.fromVal len x
        interpretRhs' (InConst x) = toConst len x
        --interpretRhs' (InConst x) = toScalar (InConst x)

        interpretRhs' (InGiven xs) = xs
        interpretRhs' (EIdx idx) = energyMap envs `safeLookup` idx
        interpretRhs' (DEIdx idx) = denergyMap envs `safeLookup` idx
        interpretRhs' (PIdx idx) = powerMap envs `safeLookup` idx
        interpretRhs' (DPIdx idx) = dpowerMap envs `safeLookup` idx
        interpretRhs' (FNIdx idx@(FEtaIdx s r f t)) = (fetaMap envs `safeLookup` idx) (powerMap envs `safeLookup` pidx)
          where pidx = PowerIdx s r f t
--        interpretRhs' (DNIdx idx) = detaMap envs `safeLookup` idx
        interpretRhs' (DNIdx idx@(DEtaIdx s r f t)) = (detaMap envs `safeLookup` idx) (powerMap envs `safeLookup` pidx)
          where pidx = PowerIdx s r f t
        interpretRhs' (DTIdx idx) = dtimeMap envs `safeLookup` idx
        interpretRhs' (ScaleIdx idx) = xMap envs `safeLookup` idx
        interpretRhs' (DScaleIdx idx) = dxMap envs `safeLookup` idx
        interpretRhs' (VIdx idx) = varMap envs `safeLookup` idx
        interpretRhs' (SIdx idx) = storageMap envs `safeLookup` idx
        interpretRhs' (InMinus t) = S.neg (interpretRhs' t)
        interpretRhs' (InRecip (FNIdx idx@(FEtaIdx s r f t))) = S.rec $ (fetaMap envs `safeLookup` idx) pval
          where pidx = PowerIdx s r t f
                pval = powerMap envs `safeLookup` pidx
        interpretRhs' (InRecip t) = S.rec (interpretRhs' t)
        interpretRhs' (InAdd s t) = (interpretRhs' s) .+ (interpretRhs' t)
        interpretRhs' (InMult s t) = (interpretRhs' s) .* (interpretRhs' t)
        interpretRhs' t = error ("interpretRhs': " ++ show t)

insert ::
   (Ord k, Show (D.Apply c2 Val), D.ZipWith c2 c2, D.Map c2,
    D.Storage c2 Val, S.Const s2 c2, S.Arith s2 s2 ~ s2,
    D.Zip c2 c2 ~ c2) =>
   Int ->
   k ->
   Envs (Signal s2 c2 Val) ->
   InTerm (Signal s2 c2 Val) ->
   M.Map k (Signal s2 c2 Val) ->
   M.Map k (Signal s2 c2 Val)
insert len idx envs rhs m = M.insert idx (interpretRhs len envs rhs) m


interpretEq ::
   (Show (D.Apply c2 Val), D.ZipWith c2 c2, D.Map c2,
    D.Storage c2 Val, S.Const s2 c2, S.Arith s2 s2 ~ s2,
    D.Zip c2 c2 ~ c2) =>
   Int ->
   Envs (Signal s2 c2 Val) ->
   InEquation (Signal s2 c2 Val) ->
   Envs (Signal s2 c2 Val)
interpretEq len envs eq =
   case eq of
      (InEqual (EIdx idx) rhs) -> envs { energyMap = insert len idx envs rhs (energyMap envs) }
      (InEqual (DEIdx idx) rhs) -> envs { denergyMap = insert len idx envs rhs (denergyMap envs) }
      (InEqual (PIdx idx) rhs) -> envs { powerMap = insert len idx envs rhs (powerMap envs) }
      (InEqual (DPIdx idx) rhs) -> envs { dpowerMap = insert len idx envs rhs (dpowerMap envs) }
      (InEqual (FNIdx idx@(FEtaIdx _s _r _f _t)) (InFunc feta)) -> envs { fetaMap = M.insert idx feta (fetaMap envs) }
{-
      (InEqual (FNIdx idx@(FEtaIdx s r f t) _) (InMult (InRecip (PIdx pidx1)) (PIdx pidx2))) -> envs''
  where envs' = envs { fetaMap = M.insert idx (mkEtaFunc pts) (fetaMap envs) }
        envs'' = envs' { fetaMap = M.insert (FEtaIdx s r t f) (mkEtaFunc (reversePts pts)) (fetaMap envs') }
        p1 = powerMap envs M.! pidx1
        p2 = powerMap envs M.! pidx2
        pts = Pt p2 (p2 ./ p1) 
-}
--      (InEqual (DNIdx idx) rhs) -> envs { detaMap = insert len idx envs rhs (detaMap envs) }
      (InEqual (DNIdx idx@(DEtaIdx _s _r _f _t)) (InFunc deta)) -> envs { detaMap = M.insert idx deta (detaMap envs) }
      (InEqual (DTIdx idx) rhs) -> envs { dtimeMap = insert len idx envs rhs (dtimeMap envs) }
      (InEqual (ScaleIdx idx) rhs) -> envs { xMap = insert len idx envs rhs (xMap envs) }
      (InEqual (DScaleIdx idx) rhs) -> envs { dxMap = insert len idx envs rhs (dxMap envs) }
      (InEqual (VIdx idx) rhs) -> envs { varMap = insert len idx envs rhs (varMap envs) }
      (InEqual (SIdx idx) rhs) -> envs { storageMap = insert len idx envs rhs (storageMap envs) }

      _ -> error ("interpretEq: " ++ showInEquation eq)


interpretFromScratch ::
   (Show (D.Apply c2 Val), D.ZipWith c2 c2, D.Map c2,
    D.Storage c2 Val, S.Const s2 c2, S.Arith s2 s2 ~ s2,
    D.Zip c2 c2 ~ c2) =>
   RecordNumber ->
   Int ->
   [InEquation (Signal s2 c2 Val)] ->
   Envs (Signal s2 c2 Val)
interpretFromScratch rec len ts = (L.foldl' (interpretEq len) emptyEnv ts) { recordNumber = rec }


interpretWithEnv ::
   (Show (D.Apply c2 Val), D.ZipWith c2 c2, D.Map c2,
    D.Storage c2 Val, S.Const s2 c2, S.Arith s2 s2 ~ s2,
    D.Zip c2 c2 ~ c2) =>
   Int ->
   Envs (Signal s2 c2 Val) ->
   InTerm (Signal s2 c2 Val) ->
   Signal s2 c2 Val
interpretWithEnv len envs t = interpretRhs len envs t
