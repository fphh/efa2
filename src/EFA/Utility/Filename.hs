{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}

module EFA.Utility.Filename where

import EFA.Utility(Caller,merror,ModuleName(..),FunctionName, genCaller)

import EFA.Flow.Part.Index (State)
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified Data.List as List
import Data.Time.Clock (UTCTime)

import qualified System.FilePath.Posix as SFP

import qualified Data.List.Split as Split

modul :: ModuleName
modul = ModuleName "EFA.Utility.Filename"

nc :: FunctionName -> Caller
nc = genCaller modul

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



data Abs
data Rel
newtype Directory = Directory String deriving (Show,Eq,Ord)
newtype FileName = FileName String deriving (Show,Eq,Ord)


data FPath typ = FPath [Directory] FileName deriving (Show,Eq,Ord)
data DirPath typ = DirPath [Directory] deriving (Show,Eq,Ord)

class ToString a where
  toString :: a -> String

instance ToString Directory where
  toString (Directory str) = str ++ "/"

instance ToString FileName where
  toString (FileName str) = str

instance ToString (FPath Abs) where
  toString (FPath xs y) =  "/" ++ concatMap toString xs ++ toString y

instance ToString (DirPath Abs) where
  toString (DirPath xs) =  "/" ++  concatMap toString xs

instance ToString (FPath Rel) where
  toString (FPath xs y) =  concatMap toString xs ++ toString y

instance ToString (DirPath Rel) where
  toString (DirPath xs) =  concatMap toString xs




class FromString a where
  fromString :: Caller -> String -> a

instance FromString Directory where
  fromString caller str =
     if SFP.isValid str && not (elem '.' str) && not (elem '/'  str)
     then Directory str
     else merror caller modul "fromString" "Directory name not valid"

instance FromString FileName where
  fromString caller str = if SFP.isValid str && not (elem '/'  str)
     then FileName str
     else merror caller modul "fromString" "File name not valid"

instance FromString (DirPath Abs) where
  fromString caller str | length str >= 2 = if SFP.isValid str && last str =='/' && head str == '/'
        then DirPath $ map Directory $ filter (/=[]) $ Split.splitOn "/" str
        else merror caller modul "fromString" ("Path not correct: " ++ show str)
  fromString caller str = merror caller modul "fromString" ("Path too short: " ++ show str)

instance FromString (FPath Abs) where
  fromString caller str | length str >= 2 = if SFP.isValid str && head str == '/'
        then let xs = filter (/=[]) $ Split.splitOn "/" str in FPath (map Directory $ init xs) (FileName $ last xs)
        else merror caller modul "fromString" ("Path not correct: " ++ show str)
  fromString caller str = merror caller modul "fromString" ("Path too short: " ++ show str)

instance FromString (FPath Rel) where
  fromString caller str | length str >= 2 = if SFP.isValid str
        then let xs = filter (/=[]) $ Split.splitOn "/" str in FPath (map Directory $ init xs) (FileName $ last xs)
        else merror caller modul "fromString" ("Path not correct: " ++ show str)
  fromString caller str = merror caller modul "fromString" ("Path too short: " ++ show str)



class Append a x b y where
  (+++) :: a x -> b y -> b x

instance Append DirPath Abs FPath Rel where
  (+++) (DirPath xs) (FPath ys fn) = (FPath (xs ++ ys) fn)

instance Append DirPath Abs DirPath Rel where
  (+++) (DirPath xs) (DirPath ys) = (DirPath (xs ++ ys))

instance Append DirPath Rel FPath Rel where
  (+++) (DirPath xs) (FPath ys fn) = (FPath (xs ++ ys) fn)

instance Append DirPath Rel DirPath Rel where
  (+++) (DirPath xs) (DirPath ys) = (DirPath (xs ++ ys))


getLastDir :: DirPath Abs -> DirPath Rel
getLastDir (DirPath xs) = DirPath [last xs]


class GetFileName a where
  getFileName :: FPath a -> FileName

instance GetFileName Abs where
  getFileName (FPath _ x) = x

instance GetFileName Rel where
  getFileName (FPath _ x) = x


class MakeRelative a where
  makeRelativeTo :: Caller -> DirPath Abs -> a Abs -> a Rel

instance MakeRelative DirPath where
  makeRelativeTo caller (DirPath xs) (DirPath ys) =
    let n = ly - lx
        ly = length ys
        lx = length xs
    in DirPath $ if ly > lx && (take n ys) == xs then drop n ys
                 else merror caller modul "makeRelativeTo"
                      "First Path not contained in Path to make relative"

instance MakeRelative FPath where
  makeRelativeTo caller (DirPath xs) (FPath ys f) =
    let n = ly - lx
        ly = length ys
        lx = length xs
    in if ly > lx && (take n ys) == xs then FPath (drop n ys) f
     else merror caller modul "makeRelativeTo"
          "First Path not contained in Path to make relative"
      