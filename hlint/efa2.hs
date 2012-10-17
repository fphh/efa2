import "hint" HLint.Default

import qualified Data.NonEmpty as NonEmpty
import qualified Data.NonEmpty.Mixed as NonEmptyMixed
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.Map as Map
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)
import Data.Tuple.HT (mapSnd)
import Data.List (sort, sortBy, groupBy, maximum, maximumBy, minimum, minimumBy)
import Control.Functor.HT (void)

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
warn "compare length" = f (length xs) (length ys) ==> f (void xs) (void ys) where note = "this comparison is lazy"; _ = eq y compare || eq y (<) || eq y (>) || eq y (<=) || eq y (>=) || eq y (==) || eq y (/=)

warn "Use dropWhileRev" = reverse (dropWhile f (reverse xs))  ==>  ListHT.dropWhileRev f xs

warn "Use null" = xs == []  ==>  null xs
warn "Use null" = xs /= []  ==>  not (null xs)
warn "length always non-negative" = length x >= 0 ==> True
warn "Use null" = length x > 0 ==> not (null x)
warn "Use null" = length x >= 1 ==> not (null x)
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

error "Redundant if" = (if x then False else y) ==> not x && y where _ = notEq y True
error "Redundant if" = (if x then y else True) ==> not x || y where _ = notEq y False
error "Redundant not" = not (not x) ==> x
