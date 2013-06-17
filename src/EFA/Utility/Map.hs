module EFA.Utility.Map where

import EFA.Utility (myShowList)

import qualified Data.Set as Set
import qualified Data.Map as M
import Data.Set (Set)
import Data.Tuple.HT (swap)


checkedLookup :: (Ord k, Show k, Show v) => M.Map k v -> k -> v
checkedLookup m k =
  case M.lookup k m of
    Nothing -> error $ "checkedLookup: " ++ show k  ++ "\n" ++ "Keys in Map:" ++ "\n" ++ (show $ M.keys m)
    Just x -> x


-- | New improved ugly version with caller function name
type Caller = String

checkedLookup2 :: (Ord k, Show k, Show v) => Caller -> M.Map k v -> k -> v
checkedLookup2 c m k =
  case M.lookup k m of
    Nothing -> error $ "Error in checkedLookup called by function " ++ show c ++
               " with the key: " ++ show k  ++ "\n" ++ "Keys in Map:" ++ "\n" ++
               (myShowList $ M.keys m)
    Just x -> x


reverse :: (Ord b) => M.Map a b -> M.Map b a
reverse = M.fromList . map swap . M.toList

-- M.fromSet is available from containers-0.5
fromSet ::
   (Ord key) => (key -> a) -> Set key -> M.Map key a
fromSet f = M.fromAscList . map (\k -> (k, f k)) . Set.toAscList

differenceSet ::
   (Ord key) => M.Map key a -> Set key -> M.Map key a
differenceSet m s = M.difference m (fromSet (const ()) s)

intersectionSet ::
   (Ord key) => M.Map key a -> Set key -> M.Map key a
intersectionSet m s = M.intersection m (fromSet (const ()) s)
