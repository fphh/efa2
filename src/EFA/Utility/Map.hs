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
  (Ord k, Show k, Show v) => Caller -> Map k v -> k -> v
checkedLookup c m k =
  case Map.lookup k m of
    Nothing -> error $ "Error in checkedLookup called by function " ++ show c ++
               " with the key: " ++ show k  ++ "\n" ++ "Keys in Map:" ++ "\n" ++
               (myShowList $ Map.keys m)
    Just x -> x

{- |
The set of keys must be equal and this is checked dynamically.
-}
checkedZipWith ::
  (Ord k) =>
  Caller ->
  (a -> b -> c) ->
  Map k a -> Map k b -> Map k c
checkedZipWith caller f ma mb =
  if Map.keysSet ma == Map.keysSet mb
    then Map.intersectionWith f ma mb
    else error $
            "checkedZipWith called by function " ++ caller ++
            ": key sets differ"


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
