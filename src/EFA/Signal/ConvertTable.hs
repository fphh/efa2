
{-# LANGUAGE TypeFamilies #-}

module EFA.Signal.ConvertTable where

import qualified Data.List.Match as Match


import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Data as Data

import EFA.IO.TableParserTypes (T (..))





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
  (S.TC sx tx (Data.Data cx dx), [S.TC sy ty (Data.Data cy dy)])
convertHelp f (Just t) =
  case transposeTable f t of
       (T _ []) -> error "convertHelp: no data"
       (T _ [_]) -> error "convertHelp: only x axis, no y values"
       (T _ (as:bbs)) -> (xs, yys)
         where xs = S.fromList as
               yys = map S.fromList bbs
convertHelp _ Nothing = error "convertHelp: table not found"

convertToSignal2D, convertToSignal3D2D ::
  ( Data.FromList cy,
    Data.FromList cx,
    Data.Storage cy dy,
    Data.Storage cx dx,
    Data.NestedList cy dy ~ [b],
    Data.NestedList cx dx ~ [b]) =>
  Maybe (T b) ->
  (S.TC sx tx (Data.Data cx dx), [S.TC sy ty (Data.Data cy dy)])
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
     -> (S.TC s t (Data.Data c1 d1),
         S.TC s1 t1 (Data.Data c2 d2),
         S.TC s2 t2 (Data.Data c d))
convertToSignal3D (Just (T _ ds)) =
  case ds of
       [] -> error "convertToSignal3D: no data"
       [_] -> error "convertToSignal3D: only x axis, no y values"
       (xs:as) ->
         case SV.transpose as of
              [] -> error "convertToSignal3D: empty list"
              (ys:zs) -> (a, b, c)
                where (xs', ys') = varMat (tail xs) ys
                      a = S.fromList xs'
                      b = S.fromList ys'
                      c = S.fromList zs
convertToSignal3D Nothing = error "convertToSignal3D: table not found"