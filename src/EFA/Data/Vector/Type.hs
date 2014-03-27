-- | Phanton Type to distinguish between Edge Vectors and Mid Vector
-- | A Timesignal is an Edge Vector a Flow Signal a Mid Vector
module EFA.Data.Vector.Type where

import qualified Data.NonEmpty as NonEmpty
import qualified EFA.Data.Vector as DV

import qualified Data.Vector as V

data Edge
data Mid
