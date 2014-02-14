{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Utility.List where

import EFA.Flow.Part.Index (State)
import Data.Time.Clock (UTCTime)

vhead :: String -> [a] -> a
vhead _ (x:_) = x
vhead caller _ = error $ "vhead, " ++ caller ++ ": empty list"


vlast :: String -> [a] -> a
vlast caller xs =
  if null xs
     then error $ "vlast, " ++ caller ++ ": empty list"
     else last xs

