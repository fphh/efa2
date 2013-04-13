module HalfPlaneMap where

import qualified Data.List.Key as Key
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tuple.HT (mapPair, )
import Data.Maybe.HT (toMaybe, )
import Data.Ord.HT (comparing, )
import Data.Maybe (catMaybes, )

import Prelude hiding (lines)


data T x a = End (Maybe a) | Cut (Line x) (T x a) (T x a)

data Line x = Line (Point x) (Point x) deriving (Eq)
data Point x = Point x x deriving (Eq)


{-
I don't want to declare an Ord instance on points
because I think this is more dangerous than helpful.
Also the instance Ord Line is not really nice
but the alternative would a wrapper for sets
and this seems to become cumbersome.
-}
instance Ord x => Ord (Line x) where
   compare =
      comparing $ \(Line (Point x0 x1) (Point y0 y1)) -> ((x0,x1), (y0,y1))


lookup :: Real x => Point x -> T x a -> Maybe a
lookup x =
   let go (End ma) = ma
       go (Cut (Line y z) l r) =
          if signedArea x y z < 0
            then go l
            else go r
   in  go

signedArea :: Num x => Point x -> Point x -> Point x -> x
signedArea x y z = det2 y z + det2 z x + det2 x y

det2 :: Num x => Point x -> Point x -> x
det2 (Point x0 x1) (Point y0 y1) = x0*y1 - x1*y0


data Triangle x = Triangle (Point x) (Point x) (Point x)
type TriangleSet x a = [(Triangle x, a)]

{- |
The triangle set needs not to be connected but it must not overlap.
If triangles overlap then both building the map and lookup
will not be efficient
and lookup will return an arbitrary triangle where a point lies in.
-}
fromTriangleSet :: (Real x) => TriangleSet x a -> T x a
fromTriangleSet =
   let go _lines [] = End Nothing
       go lines tt@((_t,(a,edges)):ts) =
          let descent line@(Line x y) (left, right) =
                 case Set.insert (Line y x) $ Set.insert (Line x y) lines of
                    newLines ->
                       Cut line (go newLines left) (go newLines right)
          in  case ts of
                 [] ->
                    case Set.toList $ Set.difference edges lines of
                       [] -> End $ Just a
                       e:_ -> descent e (splitTriangleSet e tt)
                 _ -> uncurry descent $ optimalSplitTriangleSet tt
   in  go Set.empty .
       map (\(t@(Triangle x y z), a) ->
              (t, (a, Set.fromList [Line x y, Line y z, Line z x])))

{- |
This tries to split a set of triangles
into roughly equally big sets of triangles
with respect to a line from one of the triangles.
To this end we look for the median of the x coordinates
and the median of the y coordinates,
then split the triangle set along adjacent triangle lines.
From the four candidate splits we choose the most balanced one.

However, for non-Delaunay triangulations
this selection may still be highly unbalanced.
Think of a thin ring made of narrow triangles.
None of the triangle edges
will be able to divide the set of all triangles into equally big sets.
In order to split optimally in this case
we rotate an triangle edge by 90 degrees and check a split along this line.

Be aware that a triangle can lie on both sides of a line.
-}
optimalSplitTriangleSet ::
   (Real x) =>
   TriangleSet x a ->
   (Line x, (TriangleSet x a, TriangleSet x a))
optimalSplitTriangleSet ts =
   let getMedian =
          (\m -> snd $ Map.elemAt (div (Map.size m) 2) m) .
          {-
          Don't call (Map.unionsWith (++)) !
          I do not like to preserve all median values,
          because that could mean to hold almost all values.
          -}
          Map.unions
   in  {-
       could be done more cleanly using a Pair type
       and an according ArgMin semigroup
       -}
       Key.minimum (\(_line, (tas, tbs)) -> max (length tas) (length tbs)) .
       map (\line -> (line, splitTriangleSet line ts)) .
       uncurry (++) . mapPair (getMedian, getMedian) . unzip .
       map
          (\(Triangle x@(Point x0 x1) y@(Point y0 y1) z@(Point z0 z1)) ->
             let xLines = rotate90 x y : [Line x y, Line z x]
                 yLines = rotate90 y z : [Line x y, Line y z]
                 zLines = rotate90 z x : [Line z x, Line y z]
             in  (Map.fromListWith (++) $
                     (x0, xLines) :
                     (y0, yLines) :
                     (z0, zLines) :
                     [],
                  Map.fromListWith (++) $
                     (x1, xLines) :
                     (y1, yLines) :
                     (z1, zLines) :
                     [])) .
       map fst $ ts

rotate90 :: (Num x) => Point x -> Point x -> Line x
rotate90 x@(Point x0 x1) (Point y0 y1) =
   Line x $ Point (x0-x1+y1) (x0+x1-y0)

splitTriangleSet ::
   (Real x) =>
   Line x -> TriangleSet x a ->
   (TriangleSet x a, TriangleSet x a)
splitTriangleSet (Line q r) =
   mapPair (catMaybes, catMaybes) . unzip .
   map
      (\t@(Triangle x y z, _a) ->
         let f p =
                if signedArea p q r < 0
                  then (True, False)
                  else (False, True)
             g = flip toMaybe t . or
         in  mapPair (g, g) $ unzip [f x, f y, f z])
