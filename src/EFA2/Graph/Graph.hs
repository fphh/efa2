
module EFA2.Graph.Graph where

import Data.Graph.Inductive
import qualified Data.Map as M

import EFA2.Utils.Utils


-----------------------------------------------------------------------------------
-- Topology Graph
-- | This is the main topology graph representation.

--data TopoGraph = Graph NodeTyp deriving (Show, Eq, Ord)
--data NodeTyp = Storage | Sink | Source | Crossing deriving (Show, Ord, Eq)

data NLabel = NLabel Int deriving (Show)
data ELabel = ELabel Int Int deriving (Show)


mkLEdge :: Int -> Int -> LEdge ELabel
mkLEdge x y = (x, y, ELabel x y)

flipLEdge :: LEdge ELabel -> LEdge ELabel
flipLEdge (x, y, ELabel u v) = (y, x, ELabel v u)

mkLNode :: Int -> LNode NLabel
mkLNode x = (x, NLabel x)


makeEdges :: [Int] -> [LEdge ELabel]
makeEdges no = map (uncurry mkLEdge) (pairs no)

makeNodes :: [Int] -> [LNode NLabel]
makeNodes no = map mkLNode no


----------------------------------------------------------------------------------
-- Classes to allow indexing of power positions, etas and nodes

data NodeIdx = NodeIdx !Int deriving (Show, Ord, Eq)
data EtaIdx = EtaIdx !Int !Int deriving  (Show)
data PowerIdx = PowerIdx !Int !Int deriving (Show, Ord, Eq)
data XIdx = XIdx !Int !Int deriving (Show, Ord, Eq)

type NodeEnv a = M.Map NodeIdx a
type EtaEnv a = M.Map EtaIdx a
type PowerEnv a = M.Map PowerIdx a
type XEnv a = M.Map XIdx a

instance Eq EtaIdx where
         (EtaIdx a b) == (EtaIdx x y) = f a b == f x y
           where f u v = if u < v then [u, v] else [v, u]

instance Ord EtaIdx where
         compare as@(EtaIdx a b) bs@(EtaIdx x y)
           | as == bs = EQ
           | otherwise = compare (a, b) (x, y)

mkNodeIdx x = NodeIdx x
mkEtaIdx x y = EtaIdx x y
mkPowerIdx x y = PowerIdx x y
