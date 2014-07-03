{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.OrdData where

--import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)
--import qualified Data.Map as Map
import qualified Prelude as P
import Prelude hiding (map)

type family Data a
--type instance Data  



-- | Phanton Type to distinguish between Edge Vectors and Mid Vector
-- | A Timesignal is an Edge Vector a Flow Signal a Mid Vector
-- | This Thantom Type can be applied to axis, grids, datavectors and timevectors
-- | the type parameter instance allows to put a stamp to a certain ord-data instance like global, State.Idx 0, ... etc
-- | OrdData and UseData then can be combined accordingly
-- | !! There should be only one instance of ordData of each instance type or all instances of ordData have to be exact copies !!!

data Edge instance' -- deriving (Show,Eq)
data Mid instance' -- deriving (Show,Eq)

data Global


