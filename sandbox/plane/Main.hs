{-# LANGUAGE TupleSections #-}


module Main where

import qualified Data.List as L
import Data.Ord (comparing)
import Control.Monad (liftM2)

import Debug.Trace


mkKennfeld :: (Monad m, Num a) => m a -> m (a, a, a)
mkKennfeld xs = liftM2 (\x y -> (x, y, x*y)) xs xs

kennfeld :: (Num a, Enum a, Fractional a) => [Pt a]
kennfeld = mkKennfeld [-4, -3.2..4]


type Pt a = (a, a, a)

{-
getTriangle :: (Num a, Ord a) => [Pt a] -> a -> a -> (Pt a, Pt a, Pt a)
getTriangle pts@(_:_:_:_) x y = (u, v, w)
  where as = map f pts
        f c@(a, b, _) = ((a-x)^2 + (b-y)^2, c)
        (_, u):(_, v):(_, w):_ = L.sortBy (comparing fst) as
getTriangle _ _ _ = error "getTriangle: minimum three points"
-}

getTriangle :: (Num a, Ord a) => [Pt a] -> a -> a -> (Pt a, Pt a, Pt a)
getTriangle pts@(_:_:_:_) x y = (u', v', w')
  where as = map f pts
        f c@(a, b, _) = ([(a-x)^2 + (b-y)^2], c)
        (_, t):(_, u):(_, v):(_, w):_ = L.sortBy (comparing fst) as
        u':v':w':_ = L.sortBy (comparing g) [t, u, v, w]
        g (a, b, _) = a^2 + b^2
getTriangle _ _ _ = error "getTriangle: minimum three points"




plane :: (Num a, Fractional a) => (a, a, a, a) -> a -> a -> a
plane (a, b, c, d) x y = d/c - (a*x)/c - (b*y)/c


p1 :: Double -> Double -> Double
p1 = plane (1, -2, 3, 5)

coords :: [(Double, Double)]
coords = liftM2 (,) [-4..4] [-4..4]


-- bloeder Versuch, Zeilen zu vertauschen
g :: (Eq a, Num a, Fractional a, Show a) => Pt a -> Pt a -> Pt a -> a -> a -> a
g u@(x1, y1, z1) v@(x2, y2, z2) w@(x3, y3, z3) =
  case (x1, x2, x3) of
       (0, 0, 0) -> error "senkrecht oder so"
       (0, 0, _) -> f w u v
       (0, _, 0) -> g u w v
       (0, _, _) -> f w v u
       (_, _, _) -> f u v w

-- numerischer krampf
f :: (Num a, Fractional a, Show a) => Pt a -> Pt a -> Pt a -> a -> a -> a
f u@(x1, y1, z1) v@(x2, y2, z2) w@(x3, y3, z3) = plane (a 1, b 1, c 1, 1)
  where a d = (d - (b d) * y1 - (c d) * z1) * (1/x1)
        b d = (((d) + (-(((x2) * (1/(x1))) * (d)))) - (c d) * ((z2) + (-(((x2) * (1/(x1))) * (z1))))) / ((y2) + (-(((x2) * (1/(x1))) * (y1))))
        c d = (((d) + (-(((x3) * (1/(x1))) * (d)))) + (-((((y3) + (-(((x3) * (1/(x1))) * (y1)))) * (1/((y2) + (-(((x2) * (1/(x1))) * (y1)))))) * ((d) + (-(((x2) * (1/(x1))) * (d))))))) / (((z3) + (-(((x3) * (1/(x1))) * (z1)))) + (-((((y3) + (-(((x3) * (1/(x1))) * (y1)))) * (1/((y2) + (-(((x2) * (1/(x1))) * (y1)))))) * ((z2) + (-(((x2) * (1/(x1))) * (z1)))))))


xy :: (Enum a, Num a, Fractional a) => [(a, a)]
xy = liftM2 (,) range range
  where range = [-3.9, -3.8 .. 3.9]

range =  [-3.9, -3.8 .. 3.9]

-- t :: [Double]
t = map (\y -> map (\x -> x*y) range) range


-- interpolation
main :: IO ()
main = do
  let tri = map (\x -> map (\y -> h (getTriangle kennfeld x y) x y) range) range
      h (u, v, w) x y = show x ++ " " ++ show y ++ " " ++ show (g u v w x y)
  putStrLn (L.intercalate "\n\n" (map (L.intercalate "\n") tri))



{-
-- original
main :: IO ()
main = do
  let 
      p = g (0, 1, 1) (1, 0, 0) (1, 1, 0)
      zs = map (\(x, y) -> show x ++ " " ++ show y ++ " " ++ show (p x y)) coords
      xs = map h kennfeld
      h (x, y, _) = show x ++ " " ++ show y ++ " " ++ show (x*y)
  putStrLn (L.intercalate "\n" xs)
-}



data Term = V String
          | Recip Term
          | Minus Term
          | Term :+ Term
          | Term :* Term

instance Show Term where
         show (V str) = str
         show (Minus t) = "-(" ++ show t ++ ")"
         show (Recip t) = "1/(" ++ show t ++ ")"
         show (t :+ s) = "(" ++ show t ++ ") + (" ++ show s ++ ")"
         show (t :* s) = "(" ++ show t ++ ") * (" ++ show s ++ ")"


instance Num Term where
         (+) = (:+)
         (*) = (:*)
         abs = undefined
         negate = Minus
         signum = undefined
         fromInteger = V . show

instance Fractional Term where
         fromRational = undefined
         recip = Recip



gauss :: Fractional a => [[a]] -> [[a]]
gauss = 
	let
		smul :: Fractional a => a -> [a] -> [a]
		smul val = map (val*)

		vecsub :: Fractional a => [a] -> [a] -> [a]
		vecsub = zipWith (-)

		eliminate :: Fractional a => [[a]] -> [[a]]
		eliminate mat | length mat > 1 = eliminate' mat
		              | otherwise      = mat

		eliminate' :: Fractional a => [[a]] -> [[a]]
		eliminate' ((a:r):mr) = (a:r) : (map (0:) (eliminate $ map (reduce (a:r)) mr))

		reduce :: Fractional a => [a] -> [a] -> [a]
		reduce (l:lr) (r:rr) = vecsub rr (smul (r / l) lr)
	in
		eliminate



x n = V ("x" ++ show n)
y n = V ("y" ++ show n)
z n = V ("z" ++ show n)

a = V "a"
b = V "b"
c = V "c"
d = V "d"

m = [ [x 1, y 1, z 1, d],
      [x 2, y 2, z 2, d],
      [x 3, y 3, z 3, d] ]

{-
[[x1,y1,z1,d],[0,(y2) + (-(((x2) * (1/(x1))) * (y1))),(z2) + (-(((x2) * (1/(x1))) * (z1))),(d) + (-(((x2) * (1/(x1))) * (d)))],[0,0,((z3) + (-(((x3) * (1/(x1))) * (z1)))) + (-((((y3) + (-(((x3) * (1/(x1))) * (y1)))) * (1/((y2) + (-(((x2) * (1/(x1))) * (y1)))))) * ((z2) + (-(((x2) * (1/(x1))) * (z1)))))),((d) + (-(((x3) * (1/(x1))) * (d)))) + (-((((y3) + (-(((x3) * (1/(x1))) * (y1)))) * (1/((y2) + (-(((x2) * (1/(x1))) * (y1)))))) * ((d) + (-(((x2) * (1/(x1))) * (d))))))]]
-}


{-
[[x1,y1,z1,d],[0,

(a d) * x1 + (b d) * y1 + (c d) * z1 = d

a d = (d - (b d) * y1 - (c d) * z1) / x1

b d = (((d) + (-(((x2) * (1/(x1))) * (d)))) - (c d) * ((z2) + (-(((x2) * (1/(x1))) * (z1))))) / ((y2) + (-(((x2) * (1/(x1))) * (y1))))


c d = (((d) + (-(((x3) * (1/(x1))) * (d)))) + (-((((y3) + (-(((x3) * (1/(x1))) * (y1)))) * (1/((y2) + (-(((x2) * (1/(x1))) * (y1)))))) * ((d) + (-(((x2) * (1/(x1))) * (d))))))) / (((z3) + (-(((x3) * (1/(x1))) * (z1)))) + (-((((y3) + (-(((x3) * (1/(x1))) * (y1)))) * (1/((y2) + (-(((x2) * (1/(x1))) * (y1)))))) * ((z2) + (-(((x2) * (1/(x1))) * (z1)))))))
-}

{-
a d = (d - (b d)*y1 + (c d)*z1)/x1



b d = ((d) + (-(((x2) * (1/(x1))) * (d)))) / ((y2) + (-(((x2) * (1/(x1))) * (y1))) + (c d) * (z2) + (-(((x2) * (1/(x1))) * (z1))))


c d = (((d) + (-(((x3) * (1/(x1))) * (d)))) + (-((((y3) + (-(((x3) * (1/(x1))) * (y1)))) * (1/((y2) + (-(((x2) * (1/(x1))) * (y1)))))) * ((d) + (-(((x2) * (1/(x1))) * (d))))))) / (((z3) + (-(((x3) * (1/(x1))) * (z1)))) + (-((((y3) + (-(((x3) * (1/(x1))) * (y1)))) * (1/((y2) + (-(((x2) * (1/(x1))) * (y1)))))) * ((z2) + (-(((x2) * (1/(x1))) * (z1)))))))

-}


m2 = [ [x 1, y 1, z 1, Minus d, 0],
       [x 2, y 2, z 2, Minus d, 0],
       [x 3, y 3, z 3, Minus d, 0] ]
