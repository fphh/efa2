

module Main where



{-

mit fingertree.intervalmap schneller das dreieck suchen.

<a,u>=1
<a,v>=1
<a,w>=1

a^T*(u,v,w) = (1,1,1)

(u,v,w)^T * a = (1,1,1)^T

<a,x>=1

a0*x0+a1*x1+a2*x2 = 1
a2*x2 = 1 - a0*x0 - a1*x1
x2 = (1 - a0*x0 - a1*x1) / a2

a = (u,v,w)^T^-1 * (1,1,1)^T

(p,q,r) = transpose (u,v,w)
a0 = det3 ones q r / detA; a1 = det3 p ones r / detA; a2 = det3 p q ones / detA


x2 = (det3 p q r - det3 ones q r * x0 - det3 p ones r * x1) / det3 p q ones
-}


import Graphics.Triangulation.Delaunay (triangulate)
import Data.Vector.V2 (Vector2(Vector2))

import qualified Data.Map as M
import qualified Data.List as L

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import Control.Applicative (Applicative, pure, (<*>), liftA2, liftA3)
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)

import Data.List.HT (outerProduct)
import Data.Tuple.HT (uncurry3)

import qualified Prelude as P
import Prelude hiding (unwords)


type Pt3 = Triple Double
type Pt2 = Pair Double

data Pair a = Pair a a deriving (Eq, Show)
data Triple a = Triple a a a deriving (Eq, Show)

instance Functor Pair   where fmap = Trav.fmapDefault
instance Functor Triple where fmap = Trav.fmapDefault

instance Foldable Pair   where foldMap = Trav.foldMapDefault
instance Foldable Triple where foldMap = Trav.foldMapDefault

instance Traversable Pair where
   traverse f (Pair x y) = liftA2 Pair (f x) (f y)

instance Traversable Triple where
   traverse f (Triple x y z) = liftA3 Triple (f x) (f y) (f z)

instance Applicative Pair where
   pure x = Pair x x
   Pair fx fy <*> Pair x y = Pair (fx x) (fy y)

instance Applicative Triple where
   pure x = Triple x x x
   Triple fx fy fz <*> Triple x y z = Triple (fx x) (fy y) (fz z)


mapCyclic3 :: (a -> a -> b) -> Triple a -> Triple b
mapCyclic3 f (Triple x y z) = Triple (f y z) (f z x) (f x y)

scalarProduct ::
   (Num a, Applicative f, Foldable f) =>
   f a -> f a -> a
scalarProduct x y = Fold.sum $ liftA2 (*) x y

det3 :: Triple Pt3 -> Double
det3 tri =
   let head3 (Triple x _ _) = x
       tail3 (Triple _ y z) = Pair y z
   in  scalarProduct (fmap head3 tri) (mapCyclic3 det2 $ fmap tail3 tri)

det2 :: Pt2 -> Pt2 -> Double
det2 (Pair a00 a01) (Pair a10 a11) =
   a00*a11 - a01*a10

positiveOrientation :: Pt2 -> Pt2 -> Pt2 -> Bool
positiveOrientation a b c =
   signedArea (Triple a b c) > 0

-- cf. HalfPlaneMap
signedArea :: Triple Pt2 -> Double
signedArea = Fold.sum . mapCyclic3 det2

signedAreaT :: Pt3 -> Pt3 -> Double
signedAreaT x y = signedArea $ liftA2 Pair x y

isInTriangle :: Pt2 -> Triple Pt2 -> Bool
isInTriangle a tri = Fold.and bs || Fold.all not bs
  where bs = mapCyclic3 (positiveOrientation a) tri

data Plane a = Plane {planeGradient :: Pair a, planeOffset :: a}

planeFromTriangle :: Triple Pt3 -> Plane Double
planeFromTriangle tri =
    Plane (Pair (xa / za) (ya / za)) (det3 tri / za)
  where (Triple xa ya za) = mapCyclic3 signedAreaT $ Trav.sequenceA tri

getZ :: Plane Double -> Pt2 -> Double
getZ (Plane g d) p = d - scalarProduct g p

projectToTriangle :: Plane Double -> Pt2 -> Pt3
projectToTriangle pln p@(Pair x y) = Triple x y (getZ pln p)


mkKennfeld :: [Double] -> [[Pt3]]
mkKennfeld xs = outerProduct f xs xs
  -- where f x y = Triple (x + 3*sin y) (y-4*sin(x*0.7)) (3 + sin (y/5) * sin (x/4))
  where f x y = Triple x y 0

kennfeld :: [[Pt3]]
kennfeld = mkKennfeld [-20, -17 .. 20]

project :: Pt3 -> Pt2
project (Triple x y _) = Pair x y

ptToVec2 :: Pt2 -> Vector2
ptToVec2 (Pair x y) = Vector2 x y

vecToPt2 :: Vector2 -> Pt2
vecToPt2 (Vector2 x y) = Pair x y


type TriangleMap = [(Triple Pt2, Plane Double)]

vec2planes ::
  [Pt3] -> [(Vector2, Vector2, Vector2)] -> TriangleMap
vec2planes pts =
  map
    ((\tri -> (fmap vecToPt2 tri, planeFromTriangle $ fmap unproject tri)) .
     uncurry3 Triple)
  where unproject (Vector2 x y) = Triple x y (m M.! (x,y))
        m = M.fromList $ map (\(Triple x y z) -> ((x,y), z)) pts

delaunay :: [Pt3] -> TriangleMap
delaunay pts = vec2planes pts $ triangulate $ map (ptToVec2 . project) pts

plotTriangulation :: [Triple Pt2] -> String
plotTriangulation = L.intercalate "\n" . map f
  where f (Triple a b c) = g a ++ g b ++ g c ++ g a
        g p = showWords p ++ "\n"


-- set object 1 polygon from "tri.txt" to "tri.txt" fc rgb "blue"

plotTri :: [Triple Pt2] -> String
plotTri = L.intercalate "\n" . zipWith f [1..]
  where f n (Triple a b c) =
          "set obj " ++ show n ++ " polygon from "
          ++ L.intercalate " to " [g a, g b, g c, g a] ++ "\n"
          ++ "set object " ++ show (n::Integer)
          ++ " fc rgb \"#eeeeff\" fillstyle solid 1.0 border lt -1 lw 1\n"
        g (Pair x y) = show x ++ "," ++ show y ++ ",1"


triangles :: TriangleMap
triangles = delaunay (concat kennfeld)

{-
isInTriangle :: Pt -> (Pt, Pt, Pt) -> Bool
isInTriangle (s1, s2, _) ((a1, a2, _), (b1, b2, _), (c1, c2, _)) =
  if not (ac == ab) || not (bc /= ab)
     then False
     else not (bc /= ab)
            -- then False
            -- else True
  where ax = s1 - a1
        ay = s2 - a2
        ab = (b1 - a1)*ay - (b2-a2)*ax >= 0
        ac = (c1 - a1)*ay - (c2-a2)*ax >= 0
        bc = (c1 - b1) * (s2 - b2) - (c2 - b2) * (s1-b1) >= 0
-}


getTriangle :: TriangleMap -> Pt2 -> Plane Double
getTriangle ts p =
  case dropWhile (not . isInTriangle p . fst) ts of
       t:_ -> snd t
       _ -> error ("no triangle found: " ++ show p)

{-
plane :: (Num a, Fractional a, Show a) => (a, a, a, a) -> a -> a -> a
plane (a, b, c, d) x y = d/c - (a*x)/c - (b*y)/c


-- bloeder Versuch, Zeilen zu vertauschen
getZ :: Pt -> Pt -> Pt -> Double -> Double -> Double
getZ u@(x1, y1, z1) v@(x2, y2, z2) w@(x3, y3, z3) =
  case (x1, x2, x3) of
       (0, 0, 0) -> error "senkrecht oder so"
       (0, 0, _) -> getZHelp w u v
       (0, _, 0) -> getZHelp w u v
       (0, _, _) -> getZHelp w v u
       (_, _, _) -> getZHelp u v w
-}


{-
-- numerischer krampf -> Divisionen durch 0
getZHelp :: Pt -> Pt -> Pt -> Double -> Double -> Double
getZHelp u@(x1, y1, z1) v@(x2, y2, z2) w@(x3, y3, z3) = plane (a, b, c, d)
  where d = 1
        a = (-c * z1 - b * y1 + d) / x1
        b = (- (c * x1 * z2) + (c * x2 * z1) - x2 + x1) / (x1 * y2 - x2 * y1)
        c = ((x2 - x1) * y3 + (- x3 + x1) * y2 + (x3 - x2) * y1) / ((x1 * y2 - x2 * y1) * z3 + (- x1 * y3 + x3 * y1) * z2 + (x2 * y3 - x3 * y2) * z1)
-}

range :: [Double]
range =  [-19, -18.5 .. 16]


interpolation :: IO ()
interpolation = do
  let del = delaunay (concat kennfeld)
  writeFile "interpolation.txt" $ showMesh $
    map (map (\p -> projectToTriangle (getTriangle del p) p)) $
    outerProduct Pair range range




kennf :: IO ()
kennf = do
  writeFile "kennfeld.txt" $ showMesh kennfeld


showMesh :: (Show a, Foldable f) => [[f a]] -> String
showMesh =
  L.intercalate "\n\n" .
  map (L.intercalate "\n" . map showWords)

showWords :: (Foldable f, Show a) => f a -> String
showWords = P.unwords . map show . Fold.toList



triangulation :: IO ()
triangulation = writeFile "tri.txt" $ plotTri $ map fst triangles


main :: IO ()
main = kennf >> interpolation >> triangulation
