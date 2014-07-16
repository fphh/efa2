{-# LANGUAGE FlexibleInstances #-}

module EFA.Flow.Part.Index where

import qualified EFA.Report.Format as Format

import Data.Word (Word)

import qualified Prelude as P
import Prelude hiding (init, flip)

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

newtype AbsoluteState = AbsoluteState Integer deriving (Show, Eq, Ord)

instance Atom.C (Maybe AbsoluteState) where
  
instance Tuple.C (Maybe AbsoluteState) where
  text (Just (AbsoluteState x)) = Tuple.text x
  text Nothing = Tuple.text (0/0::Double)


newtype State = State Word deriving (Show, Eq, Ord)

instance Enum State where
   toEnum n =
      if n >=0
        then State $ fromIntegral n
        else error "State.toEnum: negative number"
   fromEnum (State n) =
      if n <= fromIntegral (maxBound::Int)
        then fromIntegral n
        else error "State.fromEnum: number too big"

type InitOrState = Init State
type StateOrExit = Exit State


newtype Section = Section Word deriving (Show, Eq, Ord)

instance Enum Section where
   toEnum n =
      if n >=0
        then Section $ fromIntegral n
        else error "Section.toEnum: negative number"
   fromEnum (Section n) =
      if n <= fromIntegral (maxBound::Int)
        then fromIntegral n
        else error "Section.fromEnum: number too big"

data Init a = Init | NoInit a deriving (Show, Eq, Ord)
data Exit a = NoExit a | Exit deriving (Show, Eq, Ord)

type InitOrSection = Init Section
type SectionOrExit = Exit Section
type Augmented sec = Exit (Init sec)
type AugmentedSection = Augmented Section
type AugmentedState   = Augmented State


instance Functor Init where
   fmap _ Init = Init
   fmap f (NoInit a) = NoInit $ f a

instance Functor Exit where
   fmap _ Exit = Exit
   fmap f (NoExit a) = NoExit $ f a


section :: MaybeSection sec => Word -> sec
section = fromSection . Section

class MaybeSection sec where
   fromSection :: Section -> sec

instance MaybeSection Section where
   fromSection = id

instance MaybeSection sec => MaybeSection (Init sec) where
   fromSection = NoInit . fromSection

instance MaybeSection sec => MaybeSection (Exit sec) where
   fromSection = NoExit . fromSection


class MaybeSection sec => MaybeInit sec where
   initSection :: sec

instance MaybeSection sec => MaybeInit (Init sec) where
   initSection = Init

instance MaybeInit sec => MaybeInit (Exit sec) where
   initSection = NoExit initSection


class MaybeSection sec => MaybeExit sec where
   exitSection :: sec

instance MaybeSection sec => MaybeExit (Exit sec) where
   exitSection = Exit

instance MaybeExit sec => MaybeExit (Init sec) where
   exitSection = NoInit exitSection


switchAugmented ::
   a -> a -> (sec -> a) ->
   Augmented sec -> a
switchAugmented init exit secf aug =
   case aug of
      Exit -> exit
      NoExit Init -> init
      NoExit (NoInit s) -> secf s

fromAugmentedSection ::
   (MaybeSection sec, MaybeInit sec, MaybeExit sec) =>
   AugmentedSection -> sec
fromAugmentedSection =
   switchAugmented initSection exitSection fromSection


class ToAugmentedSection sec where
   augmentSection :: sec -> AugmentedSection

instance ToAugmentedSection Section where
   augmentSection = NoExit . NoInit

instance ToSection sec => ToAugmentedSection (Init sec) where
   augmentSection = NoExit . fmap toSection

instance ToInitOrSection sec => ToAugmentedSection (Exit sec) where
   augmentSection = fmap initOrSection


class ToInitOrSection sec where
   initOrSection :: sec -> InitOrSection

instance ToInitOrSection Section where
   initOrSection = NoInit

instance ToSection sec => ToInitOrSection (Init sec) where
   initOrSection = fmap toSection


class ToSectionOrExit sec where
   sectionOrExit :: sec -> SectionOrExit

instance ToSectionOrExit Section where
   sectionOrExit = NoExit

instance ToSection sec => ToSectionOrExit (Exit sec) where
   sectionOrExit = fmap toSection


class ToSection sec where
   toSection :: sec -> Section

instance ToSection Section where
   toSection = id


allowInit :: Exit sec -> Augmented sec
allowInit = fmap NoInit

allowExit :: Init sec -> Augmented sec
allowExit = NoExit

augment :: sec -> Augmented sec
augment = NoExit . NoInit


maybeInit :: Augmented sec -> Maybe (Exit sec)
maybeInit =
   switchAugmented Nothing (Just Exit) (Just . NoExit)

maybeExit :: Augmented sec -> Maybe (Init sec)
maybeExit aug =
   case aug of
      Exit -> Nothing
      NoExit sec -> Just sec


boundaryFromAugSection :: AugmentedSection -> Maybe Boundary
boundaryFromAugSection =
   fmap Following . maybeExit

augSectionFromBoundary :: Boundary -> AugmentedSection
augSectionFromBoundary (Following bnd) = allowExit bnd

sectionFromBoundary :: Boundary -> Maybe Section
sectionFromBoundary (Following bnd) =
   case bnd of
      Init -> Nothing
      NoInit sec -> Just sec


newtype Boundary = Following (Init Section) deriving (Show, Eq, Ord)

instance Enum Boundary where
   toEnum n =
      if n == -1
        then Following Init
        else Following $ NoInit $ toEnum n
   fromEnum (Following Init) = -1
   fromEnum (Following (NoInit n)) = fromEnum n

initial :: Boundary
initial = Following Init

afterSection :: Section -> Boundary
afterSection = Following . NoInit

beforeSection :: Section -> Boundary
beforeSection s =
   if s == section0
     then Following Init
     else Following (NoInit (pred s))


section0 :: Section
section0 = Section 0

state0 :: State
state0 = State 0



formatBoundary :: Format.Format output => Boundary -> output
formatBoundary (Following Init) = Format.initial
formatBoundary (Following (NoInit s)) = format s

class Format part where
   format :: Format.Format output => part -> output

instance Format Section where
   format (Section n) = Format.integer $ fromIntegral n

instance Format State where
   format (State n) = Format.integer $ fromIntegral n

formatInitOrOther :: (Format part, Format.Format output) => Init part -> output
formatInitOrOther (Init) = Format.initial
formatInitOrOther (NoInit s) = format s

formatOtherOrExit :: (Format part, Format.Format output) => Exit part -> output
formatOtherOrExit (NoExit s) = format s
formatOtherOrExit (Exit) = Format.exit

formatAugmented :: (Format part, Format.Format output) => Augmented part -> output
formatAugmented = switchAugmented Format.initial Format.exit format
