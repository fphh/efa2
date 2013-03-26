

module Main where

import Control.Monad (liftM2)

import qualified Data.Map as M
import qualified Data.List as L

import Graphics.Triangulation.Delaunay (triangulate)
import Data.Vector.V2 (Vector2(Vector2))

import Debug.Trace


type Pt = (Double, Double, Double)

mkKennfeld :: [Double] -> [[Pt]]
mkKennfeld xs = map (\x -> map (\y -> f x y) xs) xs
 where f x y = (x+sin(y), (y-sin(x*0.7)), 3 + sin (y/5) * sin (x/4))

kennfeld :: [[Pt]]
kennfeld = mkKennfeld [-20, -17 .. 20]

pts2vec :: [Pt] -> [Vector2]
pts2vec = map f
  where f (x, y, _) = Vector2 x y

vec2pts :: [Pt] -> [(Vector2, Vector2, Vector2)] -> [(Pt, Pt, Pt)]
vec2pts pts vs = map g vs
  where m = M.fromList (map f pts)
        f (x, y, z) = ((x, y), z)
        g (Vector2 x1 y1, Vector2 x2 y2, Vector2 x3 y3) =
          ( (x1, y1, m M.! (x1, y1)), 
            (x2, y2, m M.! (x2, y2)),
            (x3, y3, m M.! (x3, y3)) )

delaunay :: [Pt] -> [(Pt, Pt, Pt)]
delaunay pts = vec2pts pts $ triangulate $ pts2vec pts

plotTriangulation :: [(Pt, Pt, Pt)] -> String
plotTriangulation = L.intercalate "\n" . map f
  where f (a, b, c) = g a ++ g b ++ g c ++ g a
        g (x, y, _) = show x ++ " " ++ show y ++ "\n"


-- set object 1 polygon from "tri.txt" to "tri.txt" fc rgb "blue"

plotTri :: [(Pt, Pt, Pt)] -> String
plotTri = L.intercalate "\n" . zipWith f [1..]
  where f n (a, b, c) =
          "set obj " ++ show n ++ " polygon from "
          ++ L.intercalate " to " [g a, g b, g c, g a] ++ "\n"
          ++ "set object " ++ show n 
          ++ " fc rgb \"#eeeeff\" fillstyle solid 1.0 border lt -1 lw 1\n"
        g (x, y, _) = show x ++ "," ++ show y ++ ",1"

        
tri :: [(Pt, Pt, Pt)]
tri = delaunay (concat kennfeld)

isInTriangle :: Pt -> (Pt, Pt, Pt) -> Bool
isInTriangle (s1, s2, _) ((a1, a2, _), (b1, b2, _), (c1, c2, _)) =
  if (ac == ab)
     then False
     else if (bc /= ab)
             then False
             else True
  where ax = s1 - a1
        ay = s2 - a2
        ab = (b1 - a1)*ay - (b2-a2)*ax >= 0
        ac = (c1 - a1)*ay - (c2-a2)*ax >= 0
        bc = (c1 - b1) * (s2 - b2) - (c2 - b2) * (s1-b1) >= 0


getTriangle :: [(Pt, Pt, Pt)] -> Double -> Double -> (Pt, Pt, Pt)
getTriangle ts x y =
  case dropWhile (not . isInTriangle (x, y, 0)) ts of
       t:_ -> t
       _ -> error ("no triangle found: " ++ show x ++ " " ++ show y)


plane :: (Num a, Fractional a, Show a) => (a, a, a, a) -> a -> a -> a
plane (a, b, c, d) x y = d/c - (a*x)/c - (b*y)/c

{-
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

getZ u v w = getZHelp c b a
  where [a, b, c] = L.sort [u, v, w]

-- numerischer krampf -> Divisionen durch 0
getZHelp :: Pt -> Pt -> Pt -> Double -> Double -> Double
getZHelp u@(x1, y1, z1) v@(x2, y2, z2) w@(x3, y3, z3) = plane (a, b, c, d)
  where d = 1
        a = (-c * z1 - b * y1 + d) / x1
        b = (- (c * x1 * z2) + (c * x2 * z1) - x2 + x1) / (x1 * y2 - x2 * y1)
        c = ((x2 - x1) * y3 + (- x3 + x1) * y2 + (x3 - x2) * y1) / ((x1 * y2 - x2 * y1) * z3 + (- x1 * y3 + x3 * y1) * z2 + (x2 * y3 - x3 * y2) * z1)


range :: [Double]
range =  [1, 1.5 .. 16]


interpolation :: IO ()
interpolation = do
  let del = delaunay (concat kennfeld)
      tri = map (\x -> map (\y -> h (getTriangle del x y) x y) range) range
      h (u, v, w) x y = show x ++ " " ++ show y ++ " " ++ show (getZ u v w x y)
  writeFile "interpolation.txt" (L.intercalate "\n\n" (map (L.intercalate "\n") tri))




kennf :: IO ()
kennf = do
  let h (x, y, z) = show x ++ " " ++ show y ++ " " ++ show z
  writeFile "kennfeld.txt" (L.intercalate "\n\n" (map (L.intercalate "\n" . map h) kennfeld))






triangulation :: IO ()
triangulation = writeFile "tri.txt" (plotTri tri)


main :: IO ()
main = kennf >> interpolation >> triangulation
