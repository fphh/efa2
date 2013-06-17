module EFA.Utility.Map where

import EFA.Utility (myShowList)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.Tuple.HT (swap)


checkedLookup :: (Ord k, Show k, Show v) => Map k v -> k -> v
checkedLookup m k =
  case Map.lookup k m of
    Nothing -> error $ "checkedLookup: " ++ show k  ++ "\n" ++ "Keys in Map:" ++ "\n" ++ (show $ Map.keys m)
    Just x -> x


-- | New improved ugly version with caller function name
type Caller = String

checkedLookup2 :: (Ord k, Show k, Show v) => Caller -> Map k v -> k -> v
checkedLookup2 c m k =
  case Map.lookup k m of
    Nothing -> error $ "Error in checkedLookup called by function " ++ show c ++
               " with the key: " ++ show k  ++ "\n" ++ "Keys in Map:" ++ "\n" ++
               (myShowList $ Map.keys m)
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
