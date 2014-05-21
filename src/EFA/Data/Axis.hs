{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module EFA.Data.Axis where

import EFA.Utility(Caller,
                   --merror,
                   ModuleName(..),FunctionName, genCaller)
--import qualified EFA.Data.Vector as DV

--import qualified EFA.Reference.Base as Ref

--import qualified Data.Map as Map

import qualified EFA.Value.Type as Type
import qualified EFA.Value as Value

m :: ModuleName
m = ModuleName "Data.Axis"

nc :: FunctionName -> Caller
nc = genCaller m

class GetInfo axis vec a where
  getLabel :: (axis :: * -> * -> (* -> *) -> * -> *) typ label vec a -> label
  getVector :: axis typ label vec a -> vec a
  getType :: axis typ label vec a -> Type.Dynamic
  getRange :: axis typ label vec a -> Value.Range a


  