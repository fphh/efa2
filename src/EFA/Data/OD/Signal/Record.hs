{-# LANGUAGE  FlexibleContexts #-}

module EFA.Data.OD.Signal.Record where

import qualified EFA.Value as Value
import qualified EFA.Data.Vector as DV
import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty
import qualified Data.NonEmpty.Class as NonEmptyClass


import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

data NTupleWithTime label a vec b = NTupleWithTime (Map.Map label NTupleIndex) a (NonEmpty.T vec b)

newtype NTupleIndex = NTupleIndex Int

-- TODO - Module obsolete ??

lookupMaybeNTupleWithTime ::
  (Ord label,
 NonEmptyClass.Cons vec,
 DV.LookupUnsafe vec (Maybe b)) =>
 label ->
 NTupleWithTime label a vec (Maybe b) ->
 Maybe b
lookupMaybeNTupleWithTime label (NTupleWithTime m _ tuple) = case Map.lookup label m of 
  (Just idx) ->  indexNTupleWithTimeUnsafe idx tuple 
  Nothing -> Nothing
  

indexNTupleWithTimeUnsafe ::
  (NonEmptyClass.Cons vec,
   DV.LookupUnsafe vec a) =>
  NTupleIndex ->
  NonEmpty.T vec a -> a 
indexNTupleWithTimeUnsafe (NTupleIndex idx) tuple = DV.lookupUnsafe (NonEmpty.flatten tuple) idx

data Row key inst label sig vec a b = Row (Map.Map key (sig inst label vec a b))

data Column key inst label sig vec a b = Column label (NTupleWithTime key a vec b)

