module EFA.Data.ND.Cloud where

-- | Module for randomly spaced ND-data of a Map 
data Cloud dim a b = Cloud Map (ND.Point dim a) b  

-- | Rest TODO