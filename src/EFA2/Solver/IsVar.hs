

module EFA2.Solver.IsVar where

import Data.Graph.Inductive
import qualified Data.Vector.Unboxed as UV
import qualified Data.List as L

import EFA2.Solver.Equation
import EFA2.Solver.Env


-- | Section, record, from, to.
data Tableau = Tableau (UV.Vector Bool) (UV.Vector Bool) (UV.Vector Bool) (UV.Vector Bool)

mkTableau :: (UpdateAcc -> EqTerm -> UpdateAcc) -> Int -> [EqTerm] -> Tableau
mkTableau updatef len ts = Tableau sec rec from to
  where empty = UV.replicate len False
        sec = UV.update empty us
        rec = UV.update empty ur
        from = UV.update empty uf
        to = UV.update empty ut
        (s, r, f, t) = L.foldl' updatef ([], [], [], []) ts
        b = repeat True
        (us, ur, uf, ut) = (UV.fromList $ zip s b, UV.fromList $ zip r b, UV.fromList $ zip f b, UV.fromList $ zip t b)

type UpdateAcc = ([Int], [Int], [Int], [Int])

powerUpdateVec :: UpdateAcc -> EqTerm -> UpdateAcc
powerUpdateVec (s, r, f, t) (Power (PowerIdx u v w x) := Given _) = (u:s, v:s, w:f, x:t)
powerUpdateVec acc _ = acc

etaUpdateVec :: UpdateAcc -> EqTerm -> UpdateAcc
etaUpdateVec (s, r, f, t) (Eta (EtaIdx u v w x) := Given _) = (u:s, v:s, w:f, x:t)
etaUpdateVec acc _ = acc

dpowerUpdateVec :: UpdateAcc -> EqTerm -> UpdateAcc
dpowerUpdateVec (s, r, f, t) (DPower (DPowerIdx u v w x) := Given _) = (u:s, v:s, w:f, x:t)
dpowerUpdateVec acc _ = acc

detaUpdateVec :: UpdateAcc -> EqTerm -> UpdateAcc
detaUpdateVec (s, r, f, t) (DEta (DEtaIdx u v w x) := Given _) = (u:s, v:s, w:f, x:t)
detaUpdateVec acc _ = acc

xUpdateVec :: UpdateAcc -> EqTerm -> UpdateAcc
xUpdateVec (s, r, f, t) (X (XIdx u v w x) := Given _) = (u:s, v:s, w:f, x:t)
xUpdateVec acc _ = acc


isVar :: Gr a b -> [EqTerm] -> (EqTerm -> Bool)
isVar g ts t
  | (Power (PowerIdx s r f t)) <- t = (ps UV.! s) && (pr UV.! r) && (pf UV.! f) && (pt UV.! t)
  | (Eta (EtaIdx s r f t)) <- t = (ps UV.! s) && (pr UV.! r) && (pf UV.! f) && (pt UV.! t)
  | (DPower (DPowerIdx s r f t)) <- t = (ps UV.! s) && (pr UV.! r) && (pf UV.! f) && (pt UV.! t)
  | (DEta (DEtaIdx s r f t)) <- t = (ps UV.! s) && (pr UV.! r) && (pf UV.! f) && (pt UV.! t)
  | (X (XIdx s r f t)) <- t = (ps UV.! s) && (pr UV.! r) && (pf UV.! f) && (pt UV.! t)
  | otherwise = False
  where len = length (nodes g)
        Tableau ps pr pf pt = mkTableau powerUpdateVec len ts
        Tableau es er ef et = mkTableau etaUpdateVec len ts
        Tableau dps dpr dpf dpt = mkTableau dpowerUpdateVec len ts
        Tableau des der def det = mkTableau detaUpdateVec len ts
        Tableau xs xr xf xt = mkTableau xUpdateVec len ts