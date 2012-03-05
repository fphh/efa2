

module EFA2.Graph.Graph where

import Data.Graph.Inductive



data NLabel = NLabel Int deriving (Show)

data ELabel = ELabel Int Int deriving (Show)

mkLEdge :: Int -> Int -> LEdge ELabel
mkLEdge x y = (x, y, ELabel x y)

flipLEdge :: LEdge ELabel -> LEdge ELabel
flipLEdge (x, y, ELabel u v) = (y, x, ELabel v u)

mkLNode :: Int -> LNode NLabel
mkLNode x = (x, NLabel x)


pairs :: [a] -> [(a, a)]
pairs xs = zipWith (,) xs (tail xs)

no :: [Int]
no = [0..4]

no2 = [5..6]

es :: [Int] -> [LEdge ELabel]
es no = map (uncurry mkLEdge) (pairs no)

ns :: [Int] -> [LNode NLabel]
ns no = map mkLNode no

g :: Gr NLabel ELabel

-- line
--g = mkGraph (ns no) (es no)
--g = mkGraph (ns no) (es no) where no = reverse [0..3]

-- dreibein
g = mkGraph (ns no ++ ns no2) (es no ++ (es (2:no2)))

-- loop
--g = mkGraph (ns no ++ ns no2) (es no ++ (es (2:no2)) ++ [mkLEdge 5 3])

-- circular
--g = mkGraph (ns no ++ ns no2) (es no ++ (map flipLEdge (es (2:no2))) ++ [mkLEdge 3 5])
