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


class Filename a where
  filename :: a -> String


instance Filename String where
  filename = map f
    where f ' ' = '_'
          f x = x

instance Filename State where
  filename = map f . show
    where f ' ' = '_'
          f x = x


instance Filename UTCTime where
  filename = map f . show
    where f ' ' = '_'
          f x = x

instance (Filename a, Filename b) => Filename (a, b) where
  filename (x, y) = filename x ++ "-" ++ filename y

