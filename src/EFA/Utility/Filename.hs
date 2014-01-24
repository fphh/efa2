{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Utility.Filename where

import EFA.Flow.Part.Index (State)
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified Data.List as List
import Data.Time.Clock (UTCTime)

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

instance Filename Double where
  filename = show

instance Filename [Double] where
  filename =
    ('[':) . (++ "]") . List.intercalate "_" . map filename


instance (Filename node) => Filename (TopoIdx.Position node) where
  filename (TopoIdx.Position f t) = filename f ++ "->" ++ filename t


instance (Filename node) => Filename [TopoIdx.Position node] where
  filename =
    ('[':) . (++ "]") . List.intercalate "_" . map filename
