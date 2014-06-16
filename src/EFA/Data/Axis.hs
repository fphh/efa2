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
  getLabel :: axis inst label vec a -> label
  getVector :: axis inst label vec a -> vec a
  getType :: axis inst label vec a -> Type.Dynamic
  getRange :: axis inst label vec a -> Value.Range a
  getLength :: axis inst label vec a -> Int


class GetInfoNE (axis:: * -> * -> * -> (* -> *) -> * -> *) (ne::(* -> *) -> * -> *) vec a where
  getLabelNE :: axis inst label ne vec a -> label
  getVectorNE :: axis inst label ne vec a -> ne vec a
  getTypeNE :: axis inst label ne vec a -> Type.Dynamic
  getRangeNE :: axis inst label ne vec a -> Value.Range a
  getLengthNE :: axis inst label ne vec a -> Int

