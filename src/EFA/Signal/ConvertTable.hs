
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module EFA.Signal.ConvertTable where

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Vector as SV
import EFA.Signal.Data (Data, Nil, (:>))

import EFA.IO.TableParserTypes (T(T))
import qualified EFA.IO.TableParserTypes as TPT

import qualified EFA.Signal.Base as Base

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.Match as Match
import qualified Data.Foldable as Fold



transposeTable ::
  ([[d]] -> [[d]]) -> T d -> T d
transposeTable f (T (x, y) lst) =
  T (y, x) (SV.transpose $ f lst)


convertHelp ::
  ([[a]] -> [[a]]) ->
  T a ->
  (Sig.TC sx tx (Data ([] :> Nil) a),
   NonEmpty.T [] (Sig.TC sy ty (Data ([] :> Nil) a)))
convertHelp f t =
  case transposeTable f t of
    T _ [] -> error "convertHelp: no data"
    T _ [_] -> error "convertHelp: only x axis, no y values"
    T _ (as:bs:bbs) ->
      (Sig.fromList as,
       fmap Sig.fromList $ NonEmpty.cons bs bbs)

convertToSignal2D, convertToSignal3D2D ::
  T a ->
  (Sig.TC sx tx (Data ([] :> Nil) a),
   NonEmpty.T [] (Sig.TC sy ty (Data ([] :> Nil) a)))
convertToSignal2D = convertHelp id
convertToSignal3D2D = convertHelp tail


varMat :: [a] -> [b] -> ([[a]], [[b]])
varMat xs ys =
  (Match.replicate ys xs, map (Match.replicate xs) ys)


convertToSignal3D ::
  T a ->
  (Sig.TC sx tx (Data ([] :> [] :> Nil) a),
   Sig.TC sy ty (Data ([] :> [] :> Nil) a),
   Sig.TC sz tz (Data ([] :> [] :> Nil) a))
convertToSignal3D (T _ ds) =
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



-- Only first column of table is used.
makeEtaFunctions2D ::
  ( Fractional d, Ord d, Show d,
    Base.BProd d d) =>
  Map String (d, d) ->
  TPT.Map d -> Map String (d -> d)
makeEtaFunctions2D sm = Map.mapWithKey f
  where f k t = Sig.fromSample .
                  Sig.interp1Lin k xsig ysig .
                  Sig.toSample
          where (xs, NonEmpty.Cons y _) = convertToSignal2D t
                (xsig, ysig) = flip (maybe (xs, y)) (Map.lookup k sm) $
                  \(a, b) -> (Sig.scale a xs, Sig.scale b y)





{-
-- Only first column of table is used.
makeEtaFunctions2D ::
  forall d. ( Fractional d, Ord d, Show d,
    Base.BProd d d) =>
  Map String (d, d) ->
  Map (idx node) (k, idx node -> idx1 node) ->
  TPT.Map d ->
  Map String (d -> d)
makeEtaFunctions2D sm etaAssign = Map.mapWithKey f
  where {-f k t = Sig.fromSample .
                  Sig.interp1Lin k xsig ysig .
                  Sig.toSample-}

         where xs, y :: Sig.PSignal [] d
               (xs, y:_) = convertToSignal2D (Just t)
               xsig = maybe xs (Sig.scale xs . fst) (Map.lookup k sm)
               ysig = maybe y (Sig.scale y . snd) (Map.lookup k sm)

-}

-- | no checkedLookup because this would require Show (a -> a)
getEtas :: Map String (a -> a) -> [String] -> [a -> a]
getEtas etaFunc = map $
  \str -> Map.findWithDefault (error $ "getEtas :" ++ str ++ " not found") str etaFunc


{-
Diese ganzen 'error's sind hier falsch.
Die Fehler muessen im Parser abgefangen werden.
-}

getPowerSignals ::
   (Functor f) =>
   Map String (TPT.T Double) ->
   f String ->
   f (Sig.TSignal [] Double, Sig.PSignal [] Double)
getPowerSignals tabPower =
   fmap
      (f . convertToSignal2D .
       flip (Map.findWithDefault (error "getPowerSignals: signal not found")) tabPower)
  where f (x, NonEmpty.Cons y []) = (x, y)
        f _ = error $ "getPowerSignals: NonEmpty.Cons contains more than one element"


getPowerSignalsWithSameTime ::
   (Functor f, Fold.Foldable f) =>
   Map String (TPT.T Double) ->
   NonEmpty.T f String ->
   (Sig.TSignal [] Double, NonEmpty.T f (Sig.PSignal [] Double))
getPowerSignalsWithSameTime tabPower signalNames = (time, fmap snd timesPowers)
   where timesPowers = getPowerSignals tabPower signalNames
         time =
            case fmap fst timesPowers of
               NonEmpty.Cons x xs ->
                  if Fold.all (x==) xs
                    then x
                    else error $
                            "getPowerSignalsWithSameTime: " ++
                            "differing time vectors on imported signals"
