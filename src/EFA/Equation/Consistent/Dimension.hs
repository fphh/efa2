{-# LANGUAGE Rank2Types #-}
module EFA.Equation.Consistent.Dimension where

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import Data.Foldable (Foldable)

import qualified Prelude as P
import Prelude hiding (recip, filter, showsPrec)


class
   (NonEmptyC.Show idx, NonEmpty.RemoveEach idx, Foldable idx) =>
      C idx where
   switch ::
      f Empty.T ->
      (forall didx. C didx => f (NonEmpty.T didx)) ->
      f idx

instance C Empty.T where
   switch x _ = x

instance (C idx) => C (NonEmpty.T idx) where
   switch _ x = x
