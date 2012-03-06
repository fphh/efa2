

module EFA2.Graph.Graph where

import Data.Graph.Inductive

import EFA2.Utils.Utils

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


{-
no :: [Int]
no = [0..4]

no2 = [5..6]
-}

--g :: Gr NLabel ELabel

-- line
--g = mkGraph (ns no) (es no)
--g = mkGraph (ns no) (es no) where no = reverse [0..3]

-- loop
--g = mkGraph (ns no ++ ns no2) (es no ++ (es (2:no2)) ++ [mkLEdge 5 3])

-- circular
--g = mkGraph (ns no ++ ns no2) (es no ++ (map flipLEdge (es (2:no2))) ++ [mkLEdge 3 5])
