module EFA.Data.OD.Signal where

import qualified Data.NonEmpty as NonEmpty

data Samples vec a = Samples (NonEmpty.T vec a) deriving Show