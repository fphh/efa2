module EFA.Flow.Storage.Index where

import qualified EFA.Flow.Part.Index as Idx
import EFA.Flow.Part.Index
          (Augmented,
           Init(Init, NoInit), allowInit,
           Exit(Exit, NoExit), allowExit)

import qualified EFA.Utility.TypeConstructor as TC

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)

import Prelude hiding (flip)


class Flip edge where
   flip :: edge -> edge


instance Flip (X part) where
   flip (X x) = X $ flip x

instance Flip (Bond part) where
   flip (Bond p0 p1) = Bond p1 p0



newtype Content = Content Idx.Boundary deriving (Show, Ord, Eq)

{- |
Energy variables for hypothetical outgoing energies.
At carry edges they describe the maximum energy
that a storage could deliver.
-}
newtype MaxEnergy = MaxEnergy (Edge Idx.Section) deriving (Show, Ord, Eq)

newtype Energy sec = Energy (Edge sec) deriving (Show, Ord, Eq)

newtype X sec = X (Bond sec) deriving (Show, Ord, Eq)

data InSum sec = InSum (Idx.Exit sec) deriving (Show, Ord, Eq)

data OutSum sec = OutSum (Idx.Init sec) deriving (Show, Ord, Eq)


{- |
A storage edge is always directed from an early to a later section.
However, a splitting factor exists both in chronological and reversed order.
On the other hand in the future we may use chronological order exclusively
and register two split factors per edge.
-}
data Edge sec = Edge (Init sec) (Exit sec)
   deriving (Show, Eq, Ord)

data Bond sec = Bond (Augmented sec) (Augmented sec)
   deriving (Show, Eq, Ord)

instance TC.Eq Edge where eq = (==)
instance TC.Eq Bond where eq = (==)

instance TC.Ord Edge where cmp = compare
instance TC.Ord Bond where cmp = compare

instance TC.Show Edge where showsPrec = showsPrec
instance TC.Show Bond where showsPrec = showsPrec


bondFromEdge :: Edge part -> Bond part
bondFromEdge (Edge s0 s1) =
   Bond (allowExit s0) (allowInit s1)

withEdgeFromBond ::
   Ord part =>
   (Edge part -> a) ->
   (Edge part -> a) ->
   Bond part -> a
withEdgeFromBond fIn fOut (Bond stFrom stTo) =
   case (stFrom, stTo) of
      (NoExit from, Exit) ->
         fOut $ Edge from Exit
      (NoExit Init, NoExit (NoInit to)) ->
         fOut $ Edge Init (NoExit to)

      (Exit, NoExit from) ->
         fIn $ Edge from Exit
      (NoExit (NoInit to), NoExit Init) ->
         fIn $ Edge Init (NoExit to)

      (NoExit (NoInit x), NoExit (NoInit y)) ->
         case compare x y of
            LT -> fOut $ Edge (NoInit x) (NoExit y)
            GT -> fIn  $ Edge (NoInit y) (NoExit x)
            EQ -> error "storage loop in section"

      (NoExit Init, NoExit Init) ->
         error "storage loop at Init"
      (Exit, Exit) ->
         error "storage loop at Exit"


class Identifier idx where
   identifier :: Format output => idx -> output

instance Identifier MaxEnergy where identifier _ = Format.maxEnergy
instance Identifier (Energy sec) where identifier _ = Format.energy
instance Identifier (X sec) where identifier _ = Format.xfactor
