module EFA2.Utils.Graph where

import qualified Data.List as L
import Data.Maybe (fromJust, listToMaybe)
import Data.Tuple.HT (fst3)

import Data.Graph.Inductive


type InOutGraphFormat a = ([a], a, [a])

mkInOutGraphFormat :: Graph gr => (LNode a -> c) -> gr a b -> [InOutGraphFormat c]
mkInOutGraphFormat f g = zip3 (map (map f) ins) (map f ns) (map (map f) outs)
 where ns = labNodes g
       ins = map (h pre) ns
       outs = map (h suc) ns
       h next (n, _) = map (\p -> (p, fromJust (lab g p))) (next g n)

getInOutGraphFormatFromNode :: Graph gr => (LNode a -> c) -> gr a b -> Node -> InOutGraphFormat c
getInOutGraphFormatFromNode f g n = (map f lins, f l, map f louts)
  where l = (n, fromJust $ lab g n)
        ins = pre g n
        lins = zip ins (map (fromJust . lab g) ins)
        outs = suc g n
        louts = zip outs (map (fromJust . lab g) outs)

-- | Breadth first.
mkInOutGraphFormatBfs :: Graph gr => (LNode a -> c) -> gr a b -> [InOutGraphFormat c]
mkInOutGraphFormatBfs f g = map (getInOutGraphFormatFromNode f g) ns
  where ns = bfs n g
        ((_, n, _, _), _) = matchAny g



foldGraph :: Graph gr => (a -> InOutGraphFormat (LNode b) -> a) -> a -> gr b c -> a
foldGraph f start g = L.foldl' f start (mkInOutGraphFormat id g)

foldGraphNodes :: Graph gr => (a -> InOutGraphFormat Node -> a) -> a -> gr b c -> a
foldGraphNodes f start g = L.foldl' f start (mkInOutGraphFormat fst g)

foldGraphLabels :: Graph gr => (a -> InOutGraphFormat b -> a) -> a -> gr b c -> a
foldGraphLabels f start g = L.foldl' f start (mkInOutGraphFormat snd g)

mapGraph :: Graph gr => (InOutGraphFormat (LNode b) -> a) -> gr b c -> [a]
mapGraph f g = map f (mkInOutGraphFormat id g)

mapGraphNodes :: Graph gr => (InOutGraphFormat Node -> a) -> gr b c -> [a]
mapGraphNodes f g = map f (mkInOutGraphFormat fst g)

mapGraphLabels :: Graph gr => (InOutGraphFormat b -> a) -> gr b c -> [a]
mapGraphLabels f g = map f (mkInOutGraphFormat snd g)

getLEdge :: (Graph gr) => gr a b -> Node -> Node -> Maybe (LEdge b)
getLEdge g from to =
  listToMaybe $ filter ((from ==) . fst3) $ inn g to

transClose :: Gr a b -> Gr a ()
transClose = efilter (\(x, y, _) -> x /= y) . trc

