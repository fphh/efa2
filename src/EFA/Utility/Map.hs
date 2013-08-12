module EFA.Utility.Map where

import EFA.Utility (myShowList)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Tuple.HT (swap)


-- | New improved ugly version with caller function name
type Caller = String


checkedLookup ::
  (Ord k, Show k) => Caller -> Map k v -> k -> v
checkedLookup c m k =
  case Map.lookup k m of
    Nothing ->
      error $ "CheckedLookup error in " ++ c ++ "\n"
              ++ "key: " ++ show k  ++ "\n"
              ++ "keys in map:\n" ++ myShowList (Map.keys m) ++ "\n"
    Just x -> x


reverse :: (Ord b) => Map a b -> Map b a
reverse = Map.fromList . map swap . Map.toList

-- Map.fromSet is available from containers-0.5
fromSet ::
   (Ord key) => (key -> a) -> Set key -> Map key a
fromSet f = Map.fromAscList . map (\k -> (k, f k)) . Set.toAscList

differenceSet ::
   (Ord key) => Map key a -> Set key -> Map key a
differenceSet m s = Map.difference m (fromSet (const ()) s)

intersectionSet ::
   (Ord key) => Map key a -> Set key -> Map key a
intersectionSet m s = Map.intersection m (fromSet (const ()) s)
