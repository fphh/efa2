module HalfPlaneMap where

data T x a = End (Maybe a) | Cut (Line x) (T x a) (T x a)

data Line x = Line (Point x) (Point x)
data Point x = Point x x


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
