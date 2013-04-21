

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

import Data.List.HT (outerProduct)


type Pt = (Double, Double, Double)
type Pt2 = (Double, Double)

transpose :: Pt -> Pt -> Pt -> (Pt, Pt, Pt)
transpose (u0, u1, u2) (v0, v1, v2) (w0, w1, w2) =
  ((u0, v0, w0), (u1, v1, w1), (u2, v2, w2))

det3 :: Pt -> Pt -> Pt -> Double
det3 u v w =
   let subDet (x,_,_) (_,y0,y1) (_,z0,z1) = x * det2 (y0,y1) (z0,z1)
   in  subDet u v w + subDet v w u + subDet w u v

det2 :: Pt2 -> Pt2 -> Double
det2
   (a00, a01)
   (a10, a11) =
      a00*a11 - a01*a10

positiveOrientation :: Pt2 -> Pt2 -> Pt2 -> Bool
positiveOrientation a b c =
   signedArea a b c > 0

-- cf. HalfPlaneMap
signedArea :: Pt2 -> Pt2 -> Pt2 -> Double
signedArea a b c =
   det2 b c + det2 c a + det2 a b

signedAreaT :: Pt -> Pt -> Double
signedAreaT (a0,b0,c0) (a1,b1,c1) =
   signedArea (a0,a1) (b0,b1) (c0,c1)

isInTriangle :: Pt2 -> (Pt2, Pt2, Pt2) -> Bool
isInTriangle a (b, c, d) = (x && y && z) || (not x && not y && not z)
  where x = positiveOrientation a b c
        y = positiveOrientation a d b
        z = positiveOrientation a c d

getZ :: Pt -> Pt -> Pt -> Double -> Double -> Double
getZ u v w x y =
  (det3 p q r - signedAreaT q r * x - signedAreaT r p * y) / signedAreaT p q
  where (p, q, r) = transpose u v w



mkKennfeld :: [Double] -> [[Pt]]
mkKennfeld xs = outerProduct f xs xs
  -- where f x y = (x+3*sin(y), (y-4*sin(x*0.7)), 3 + sin (y/5) * sin (x/4))
  where f x y = (x, y, 0)

kennfeld :: [[Pt]]
kennfeld = mkKennfeld [-20, -17 .. 20]

project :: Pt -> Pt2
project (x, y, _) = (x, y)

projectTriangle :: (Pt, Pt, Pt) -> (Pt2, Pt2, Pt2)
projectTriangle (u, v, w) = (project u, project v, project w)

pts2vec :: [Pt] -> [Vector2]
pts2vec = map f
  where f (x, y, _) = Vector2 x y

vec2pts :: [Pt] -> [(Vector2, Vector2, Vector2)] -> [(Pt, Pt, Pt)]
vec2pts pts =
    map (\(u, v, w) -> (unproject u, unproject v, unproject w))
  where m = M.fromList $ map (\(x, y, z) -> ((x, y), z)) pts
        unproject (Vector2 x y) = (x, y, m M.! (x, y))

delaunay :: [Pt] -> [(Pt, Pt, Pt)]
delaunay pts = vec2pts pts $ triangulate $ pts2vec pts

plotTriangulation :: [(Pt2, Pt2, Pt2)] -> String
plotTriangulation = L.intercalate "\n" . map f
  where f (a, b, c) = g a ++ g b ++ g c ++ g a
        g (x, y) = show x ++ " " ++ show y ++ "\n"


-- set object 1 polygon from "tri.txt" to "tri.txt" fc rgb "blue"

plotTri :: [(Pt, Pt, Pt)] -> String
plotTri = L.intercalate "\n" . zipWith f [1..]
  where f n (a, b, c) =
          "set obj " ++ show n ++ " polygon from "
          ++ L.intercalate " to " [g a, g b, g c, g a] ++ "\n"
          ++ "set object " ++ show (n::Integer)
          ++ " fc rgb \"#eeeeff\" fillstyle solid 1.0 border lt -1 lw 1\n"
        g (x, y, _) = show x ++ "," ++ show y ++ ",1"


triangles :: [(Pt, Pt, Pt)]
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


getTriangle :: [(Pt, Pt, Pt)] -> Double -> Double -> (Pt, Pt, Pt)
getTriangle ts x y =
  case dropWhile (not . isInTriangle (x, y) . projectTriangle) ts of
       t:_ -> t
       _ -> error ("no triangle found: " ++ show x ++ " " ++ show y)

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
      tr = outerProduct (\x y -> h (getTriangle del x y) x y) range range
      h (u, v, w) x y = show x ++ " " ++ show y ++ " " ++ show (getZ u v w x y)
  writeFile "interpolation.txt" (L.intercalate "\n\n" (map (L.intercalate "\n") tr))




kennf :: IO ()
kennf = do
  let h (x, y, z) = show x ++ " " ++ show y ++ " " ++ show z
  writeFile "kennfeld.txt" (L.intercalate "\n\n" (map (L.intercalate "\n" . map h) kennfeld))






triangulation :: IO ()
triangulation = writeFile "tri.txt" $ plotTri triangles


main :: IO ()
main = kennf >> interpolation >> triangulation
