import "hint" HLint.Default

import qualified Data.NonEmpty as NonEmpty
import qualified Data.NonEmpty.Mixed as NonEmptyMixed
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Maybe.HT (toMaybe)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)
import Data.Tuple.HT (mapSnd)
import Data.List (sort, sortBy, groupBy, maximum, maximumBy, minimum, minimumBy)
import Control.Functor.HT (void)
import Control.Monad (join, liftM, liftM2, liftM3, liftM4)

{-
HLint.Default:
error  = x == Nothing  ==>  isNothing x
error  = Nothing == x  ==>  isNothing x

Should add a note, that this saves an Eq constraint.
-}

ignore "Eta reduce"
-- warn = f | x < y = lt | x == y = eq | x > y = gt ==> case compare x y of LT -> lt; EQ -> eq; GT -> gt
warn = (case f of _ | x < y -> lt | x == y -> eq | x > y -> gt) ==> case compare x y of LT -> lt; EQ -> eq; GT -> gt

warn "Use integer number literal" = 0.0 ==> 0 where note = "0 is Num and thus more general than 0.0, which is Fractional"
warn "Use integer number literal" = 1.0 ==> 1 where note = "1 is Num and thus more general than 1.0, which is Fractional"
warn "Use integer number literal" = 2.0 ==> 2 where note = "2 is Num and thus more general than 2.0, which is Fractional"

warn "groupBy" = groupBy ==> NonEmptyMixed.groupBy
warn "cycle" = cycle ==> NonEmpty.cycle
warn "head" = head ==> NonEmpty.head
warn "tail" = tail ==> NonEmpty.tail
warn "last" = last ==> NonEmpty.last
warn "init" = init ==> NonEmpty.init
warn "foldl1" = foldl1 ==> NonEmpty.foldl1
warn "foldr1" = foldr1 ==> NonEmpty.foldr1
warn "maximum" = maximum ==> NonEmpty.maximum
warn "minimum" = minimum ==> NonEmpty.minimum
warn "maximumBy" = maximumBy ==> NonEmpty.maximumBy
warn "minimumBy" = minimumBy ==> NonEmpty.minimumBy
warn "fromJust" = fromJust ==> fromMaybe (error "location of call")

warn "head . sort" = head (sort xs) ==> minimum xs
warn "last . sort" = last (sort xs) ==> maximum xs
warn "head . sortBy" = head (sortBy f xs) ==> minimumBy f xs
warn "last . sortBy" = last (sortBy f xs) ==> maximumBy f xs
-- In Horn wird head $ L.sortBy uebersehen - warum?

warn "take length" = take (length xs) ys ==> Match.take xs ys where note = "Match.take is lazy"
warn "equal length" = length xs == length ys ==> Match.equalLength xs ys where note = "this comparison is lazy"
-- Sequence: check = length keys == length xs   -- wird nicht erkannt, warum?
warn "compare length" = compare (length xs) (length ys) ==> Match.compareLength xs ys where note = "this comparison is lazy"
-- warn "compare length" = f (length xs) (length ys) ==> f (void xs) (void ys) where note = "this comparison is lazy"; _ = eq y compare || eq y (<) || eq y (>) || eq y (<=) || eq y (>=) || eq y (==) || eq y (/=)

warn "Use dropWhileRev" = reverse (dropWhile f (reverse xs))  ==>  ListHT.dropWhileRev f xs

warn "Use toMaybe" = (if c then Just x else Nothing)  ==>  toMaybe c x
warn "Use toMaybe" = (if c then Nothing else Just x)  ==>  toMaybe (not c) x
warn "Use toMaybe" = (guard c >> Just x)  ==>  toMaybe c x

warn "Use join" = (case m of Nothing -> Nothing; Just x -> x)  ==>  join m
warn "Use join" = (maybe Nothing id)  ==>  join

warn "Use liftM" = (do x <- m; return (f x))  ==>  liftM f m
warn "Use liftM2" = (do x <- mx; y <- my; return (f x y))  ==>  liftM2 f mx my
warn "Use liftM3" = (do x <- mx; y <- my; z <- mz; return (f x y z))  ==>  liftM3 f mx my mz
warn "Use liftM4" = (do x <- mx; y <- my; z <- mz; w <- mw; return (f x y z w))  ==>  liftM4 f mx my mz mw

{-
These warnings do not seem to improve much,
but I suspect that subsequent operations can be simplified
using the suggest replacements,
since the lists should have been tuples anyway.
-}
warn "Use liftM2" = sequence [mx,my]  ==>  liftM2 (\(x,y) -> [x,y]) mx my
warn "Use liftM3" = sequence [mx,my,mz]  ==>  liftM3 (\(x,y,z) -> [x,y,z]) mx my mz
warn "Use liftM4" = sequence [mx,my,mz,mw]  ==>  liftM4 (\(x,y,z,w) -> [x,y,z,w]) mx my mz mw

warn "Use null" = xs == []  ==>  null xs where note = "saves an Eq constraint"
warn "Use null" = xs /= []  ==>  not (null xs) where note = "saves an Eq constraint"
warn "length always non-negative" = length x >= 0 ==> True
warn "Use null" = length x > 0 ==> not (null x) where note = "increases laziness"
warn "Use null" = length x >= 1 ==> not (null x) where note = "increases laziness"
warn "Use drop and null" = length x > n ==> not (null (drop n x)) where _ = notEq n 0
warn "Use drop and null" = length x >= n ==> not (null (drop (n-1) x)) where _ = notEq n 0 && notEq n 1

warn "comparing" = compare (f x) (f y) ==> comparing f x y
warn "on compare" = on compare f ==> comparing f
warn "equating" = f x == f y ==> equating f x y
warn "not equating" = f x /= f y ==> not (equating f x y)
warn "on (==)" = on (==) f ==> equating f
warn "on (/=)" = on (/=) f ==> not (equating f)
{-
This one suggests lots of funny replacements,
but they hardly increase readability:

warn "on" = g (f x) (f y) ==> (g `on` f) x y
-}

warn "Use Map" = groupBy (equating fst) (sortBy (comparing fst) xs) ==> Map.toAscList (Map.fromListWith (++) (map (mapSnd (:[])) xs))
warn "Use Map.fromListWith" = Map.fromList ==> Map.fromListWith (error "multiple keys") where note = "Map.fromList silently drops colliding keys - is this wanted?"
warn "Use Map.toAscList" = Map.toList ==> Map.toAscList where note = "Map.toList returns keys in any order - are you sure that you do not expect ascending order?"
warn "Use Map.elems" = map snd (Map.toList m) ==> Map.elems m
warn "Use Foldable.foldl" = foldl f x (Map.elems m) ==> Fold.foldl f x m
warn "Use Foldable.foldr" = foldr f x (Map.elems m) ==> Fold.foldr f x m
warn "Use Foldable.length" = length (Map.elems m) ==> Fold.length m
warn "Use Foldable.sum" = sum (Map.elems m) ==> Fold.sum m
warn "Use Foldable.product" = product (Map.elems m) ==> Fold.product m
warn "Use Foldable.maximum" = maximum (Map.elems m) ==> Fold.maximum m
warn "Use Foldable.minimum" = minimum (Map.elems m) ==> Fold.minimum m
warn "Use Foldable.and" = and (Map.elems m) ==> Fold.and m
warn "Use Foldable.or" = or (Map.elems m) ==> Fold.or m
warn "Use Foldable.all" = all f (Map.elems m) ==> Fold.all f m
warn "Use Foldable.any" = any f (Map.elems m) ==> Fold.any f m

error "Redundant if" = (if x then False else y) ==> not x && y where _ = notEq y True
error "Redundant if" = (if x then y else True) ==> not x || y where _ = notEq y False
error "Redundant not" = not (not x) ==> x where note = "increases laziness"

-- I would like to extend this to arbitrary case expressions.
error "Too strict if" = (if c then f x else f y) ==> f (if c then x else y) where note = "increases laziness"
error "Too strict maybe" = maybe (f x) (f . g) ==> f . maybe x g where note = "increases laziness"

error "Too strict case" = (case m of Just x -> f y; Nothing -> f z) ==> f (case m of Just x -> y; Nothing -> z) where note = "May increase laziness"
error "Too strict case" = (case m of Just x -> f y w; Nothing -> f z w) ==> f (case m of Just x -> y; Nothing -> z) w where note = "May increase laziness"
error "Too strict case" = (case m of Left x -> f y; Right z -> f w) ==> f (case m of Left x -> y; Right z -> w) where note = "May increase laziness"
error "Too strict case" = (case m of Left x -> f y w; Right z -> f v w) ==> f (case m of Left x -> y; Right z -> v) w where note = "May increase laziness"

error "Use Foldable.forM_" = (case m of Nothing -> return (); Just x -> f x) ==> Fold.forM_ m f
error "Use Foldable.forM_" = when (isJust m) (f (fromJust m)) ==> Fold.forM_ m f
error "Use Foldable.mapM_" = maybe (return ()) ==> Fold.mapM_
