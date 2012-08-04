{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.List as L
import qualified Data.Set as S
import Data.Graph.Inductive


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData
import EFA2.Utils.Utils
import EFA2.Display.DrawGraph

import Debug.Trace

topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, NoRestriction), (1, NoRestriction), (2, Crossing), (3, NoRestriction), (4, NoRestriction)]
        edges = [(0, 2, defaultELabel), (1, 2, defaultELabel), (2, 3, defaultELabel), (2, 4, defaultELabel)]


data DirEdge = Node :-> Node
             | Node :<- Node
             | Node :-: Node deriving (Eq, Show)


solutions :: [Edge] -> [[DirEdge]]
solutions = map fst . filter (good . snd) . foldr expand []

expand :: Edge -> [([DirEdge], ValOfSol)] -> [([DirEdge], ValOfSol)]
expand x = filter (ok . snd) . uncurry zip . cross (extend x, modify x) . unzip

cross :: (a -> x, b -> y) -> (a, b) -> (x, y)
cross (f, g) (x, y) = (f x, g y)

good :: ValOfSol -> Bool
good x = True


ok :: ValOfSol -> Bool
ok _ = True


extend :: Edge -> [[DirEdge]] -> [[DirEdge]]
extend (x, y) [] = [[x :-> y], [x :<- y], [x :-: y]]
extend (x, y) ds = fw ++ bw ++ nd
  where fw = map ((x :-> y):) ds
        bw = map ((x :<- y):) ds
        nd = map ((x :-: y):) ds

data ValOfSol = ValOfSol Int Int deriving (Show)


modify :: Edge -> [ValOfSol] -> [ValOfSol]
modify (x, y) [] = [ValOfSol x y, ValOfSol x y, ValOfSol x y]
modify (x, y) xs = map f xs ++ map f xs ++ map f xs
  where f _ = ValOfSol x y

main :: IO ()
main = do
  let es = edges topo
      sol = solutions es
  print es
  putStrLn (L.intercalate "\n" $ map show sol)
  print (length sol)
