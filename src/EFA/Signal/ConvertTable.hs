
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EFA.Signal.ConvertTable where

import qualified Data.List.Match as Match
import qualified Data.Map as M


import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Data as Data

import EFA.IO.TableParserTypes (T(..))
import qualified EFA.IO.TableParserTypes as TPT

import qualified EFA.Signal.Base as Base



transposeTable ::
  ([[d]] -> [[d]]) -> T d -> T d
transposeTable f (T (x, y) lst) =
  T (y, x) (SV.transpose $ f lst)


convertHelp ::
  ( Data.FromList cx, Data.Storage cx dx,
    Data.NestedList cx dx ~ [b],
    Data.FromList cy, Data.Storage cy dy,
    Data.NestedList cy dy ~ [b] ) =>
  ([[b]] -> [[b]]) ->
  Maybe (T b) ->
  (Sig.TC sx tx (Data.Data cx dx), [Sig.TC sy ty (Data.Data cy dy)])
convertHelp f (Just t) =
  case transposeTable f t of
       (T _ []) -> error "convertHelp: no data"
       (T _ [_]) -> error "convertHelp: only x axis, no y values"
       (T _ (as:bbs)) -> (xs, yys)
         where xs = Sig.fromList as
               yys = map Sig.fromList bbs
convertHelp _ Nothing = error "convertHelp: table not found"

convertToSignal2D, convertToSignal3D2D ::
  ( Data.FromList cy,
    Data.FromList cx,
    Data.Storage cy dy,
    Data.Storage cx dx,
    Data.NestedList cy dy ~ [b],
    Data.NestedList cx dx ~ [b]) =>
  Maybe (T b) ->
  (Sig.TC sx tx (Data.Data cx dx), [Sig.TC sy ty (Data.Data cy dy)])
convertToSignal2D = convertHelp id
convertToSignal3D2D = convertHelp tail


varMat :: [a] -> [b] -> ([[a]], [[b]])
varMat xs ys =
  (Match.replicate ys xs, map (Match.replicate xs) ys)


convertToSignal3D :: (Data.FromList c, Data.FromList c1,
      Data.FromList c2, Data.Storage c d,
      Data.Storage c1 d1, Data.Storage c2 d2,
      Data.NestedList c2 d2 ~ [[b]],
      Data.NestedList c1 d1 ~ [[b]],
      Data.NestedList c d ~ [[b]]) =>
     Maybe (T b)
     -> (Sig.TC s t (Data.Data c1 d1),
         Sig.TC s1 t1 (Data.Data c2 d2),
         Sig.TC s2 t2 (Data.Data c d))
convertToSignal3D (Just (T _ ds)) =
  case ds of
       [] -> error "convertToSignal3D: no data"
       [_] -> error "convertToSignal3D: only x axis, no y values"
       (xs:as) ->
         case SV.transpose as of
              [] -> error "convertToSignal3D: empty list"
              (ys:zs) -> (a, b, c)
                where (xs', ys') = varMat (tail xs) ys
                      a = Sig.fromList xs'
                      b = Sig.fromList ys'
                      c = Sig.fromList zs
convertToSignal3D Nothing = error "convertToSignal3D: table not found"



-- Only first column of table is used.
makeEtaFunctions2D ::
  forall d. ( Fractional d, Ord d, Show d,
    Base.BProd d d) =>
  M.Map String (d, d) -> TPT.Map d -> M.Map String (d -> d)
makeEtaFunctions2D sm = M.mapWithKey f
  where f k t = Sig.fromSample . 
                  Sig.interp1Lin k xsig ysig . 
                  Sig.toSample
         where xs, y :: Sig.PSignal [] d
               (xs, y:_) = convertToSignal2D (Just t)
               xsig = maybe xs (Sig.scale xs . fst) (M.lookup k sm)
               ysig = maybe y (Sig.scale y . snd) (M.lookup k sm)

