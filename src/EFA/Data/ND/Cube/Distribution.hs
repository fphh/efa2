-- | Module to Provide an ND-Distribution Datatype
module EFA.Data.ND.Distribution where

newtype Class typ dim a = ND.Data dim a

-- A Distribution is often sparsely populated 
-- A Data.Map is the best datatype 

-- | Module for randomly spaced ND-data of a Map 
data Distribution typ dim a b = Distribution (Grid label typ dim a) Map (Class typ dim a) b  


-- TODO:: Methods to create clever grids and convert between mid and edge

{-
even :: (P.RealFrac a) => a -> a -> a -> Class a
even interval offs x =
  Class (P.fromIntegral((P.round ((x P.+ offs) P./ interval))::P.Integer) P.* interval P.- offs)


class ClassificationVector where
  midFromEdge ::  EdgeVec vec [a] -> MidVec vec [a]
  edgeFromMid :: MidVec vec [a] -> EdgeVec vec [a]



-}

