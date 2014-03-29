{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Utility.List where

vhead :: String -> [a] -> a
vhead _ (x:_) = x
vhead caller _ = error $ "vhead, " ++ caller ++ ": empty list"


vlast :: String -> [a] -> a
vlast _ xs@(_:_) = last xs
vlast caller _ = error $ "vlast, " ++ caller ++ ": empty list"


-- | Example
--
-- >>> takeUntil (>5) [1..10]
-- [1,2,3,4,5,6]
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = go
  where go (x:xs) =  x : if not (p x) then go xs else []
        go [] = []
