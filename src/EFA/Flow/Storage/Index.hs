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


newtype Content = Content Idx.Boundary deriving (Show, Ord, Eq)

{- |
Energy variables for hypothetical outgoing energies.
At carry edges they describe the maximum energy
that a storage could deliver.
-}
newtype MaxEnergy = MaxEnergy (Edge Idx.Section) deriving (Show, Ord, Eq)

newtype Energy sec = Energy (Edge sec) deriving (Show, Ord, Eq)

newtype X sec = X (Position sec) deriving (Show, Ord, Eq)

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

data Position sec = Position (Augmented sec) (Augmented sec)
   deriving (Show, Eq, Ord)

instance TC.Eq Edge where eq = (==)
instance TC.Eq Position where eq = (==)

instance TC.Ord Edge where cmp = compare
instance TC.Ord Position where cmp = compare

instance TC.Show Edge where showsPrec = showsPrec
instance TC.Show Position where showsPrec = showsPrec


outPosFromEdge, inPosFromEdge :: Edge part -> Position part
outPosFromEdge (Edge s0 s1) =
   Position (allowExit s0) (allowInit s1)
inPosFromEdge (Edge s0 s1) =
   Position (allowInit s1) (allowExit s0)

withEdgeFromPosition ::
   Ord part =>
   (Edge part -> a) ->
   (Edge part -> a) ->
   Position part -> a
withEdgeFromPosition fIn fOut (Position stFrom stTo) =
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
