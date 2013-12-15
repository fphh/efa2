module EFA.Report.Format where

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Mix as Mix
import qualified EFA.Utility.FixedLength as FixedLength

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Map (Map)

import Data.Bool.HT (if')
import Data.List (intercalate)
import Data.Ratio (Ratio, numerator, denominator)
import Data.Foldable (Foldable, foldr1)

import Text.Printf (PrintfArg, printf)

import qualified Prelude as P
import Prelude hiding (words, lines, sum, negate, foldr1)


-- * special Unicode characters

deltaChar :: Char
deltaChar = '\x2206'

heartChar :: Char
heartChar = '\x2665'


-- * common output types

newtype ASCII = ASCII { unASCII :: String }

newtype Unicode = Unicode { unUnicode :: String }

newtype Latex = Latex { unLatex :: String }


-- * class for unified handling of ASCII, Unicode and LaTeX output

{-
We need the ConstOne function in the solver
in order to create a signal for the sum of split factors,
whose length matches the length of other signals.
A different approach would be to manage the length of signals
as quantity for the solver,
but then we would need a length function.
-}
data Function = Absolute | Signum | ConstOne
   deriving (Eq, Ord, Show)


class Format output where
   literal :: String -> output
   integer :: Integer -> output
   real :: (RealFrac a, PrintfArg a, Ord a) => a -> output
   ratio :: (Integral a, Show a) => Ratio a -> output
   subscript :: output -> output -> output
   connect :: output -> output -> output
   link :: output -> output -> output
   list :: [output] -> output
   undetermined :: output
   empty :: output
   words, lines :: [output] -> output
   assign :: output -> output -> output
   pair :: output -> output -> output

   nodeInt :: output -> output -> output
   nodeString :: output -> output -> output
   nodeStorage :: output
   nodeSink :: output
   nodeAlwaysSink :: output
   nodeSource :: output
   nodeAlwaysSource :: output
   nodeCrossing :: output
   nodeDeadNode :: output
   nodeNoRestriction :: output

   function :: Function -> output -> output
   integral :: output -> output
   recordDelta :: RecIdx.Delta -> output -> output
   mixComponent :: output -> output -> output
   mixPair :: output -> output -> output
   mix :: Foldable f => output -> NonEmpty.T f output -> output
   initial, exit :: output
   sectionNode :: output -> output -> output
   directionIn, directionOut :: output
   delta :: output -> output
   energy, maxEnergy, power, xfactor, eta :: output
   dtime, signalSum, scalarSum, storage :: output
   parenthesize, negate, recip :: output -> output
   plus, minus, multiply :: output -> output -> output
   intPower :: output -> Integer -> output
   showRaw :: output -> String

instance Format ASCII where
   -- may need some escaping for non-ASCII characters
   literal = ASCII
   integer = ASCII . show
   real = ASCII . realExp
   ratio r = ASCII $ show (numerator r) ++ "/" ++ show (denominator r)
   subscript (ASCII t) (ASCII s) = ASCII $ t ++ "_" ++ s
   connect (ASCII t) (ASCII s) = ASCII $ t ++ "_" ++ s
   link (ASCII t) (ASCII s) = ASCII $ t ++ ":" ++ s
   list = ASCII . ("["++) . (++"]") . intercalate "," . map unASCII
   undetermined = ASCII "?"
   empty = ASCII ""
   lines = ASCII . unlines . map unASCII
   words = ASCII . unwords . map unASCII
   assign (ASCII lhs) (ASCII rhs) =
      ASCII $ lhs ++ " = " ++ rhs
   pair (ASCII lhs) (ASCII rhs) =
      ASCII $ "(" ++ lhs ++ ", " ++ rhs ++ ")"

   nodeInt (ASCII typ) (ASCII num) = ASCII $ typ++num
   nodeString (ASCII typ) (ASCII num) = ASCII $ typ++"-"++num
   nodeStorage = ASCII "St"
   nodeSink = ASCII "Si"
   nodeAlwaysSink = ASCII "ASi"
   nodeSource = ASCII "So"
   nodeAlwaysSource = ASCII "ASo"
   nodeCrossing = ASCII "C"
   nodeDeadNode = ASCII "D"
   nodeNoRestriction = ASCII "Any"

   function f (ASCII rest) =
      ASCII $
      case f of
         Absolute -> "|" ++ rest ++ "|"
         Signum -> "sgn(" ++ rest ++ ")"
         ConstOne -> "constone(" ++ rest ++ ")"
   integral (ASCII x) = ASCII $ "integrate(" ++ x ++ ")"
   recordDelta d (ASCII rest) =
      ASCII $ (++rest) $
      case d of
         RecIdx.Before -> "[0]"
         RecIdx.After -> "[1]"
         RecIdx.Delta -> "d"
   mixComponent (ASCII c) (ASCII x) = ASCII $ c ++ "?" ++ x
   mixPair (ASCII x) (ASCII y) = ASCII $ x ++ "," ++ y
   mix (ASCII s) xs =
      case foldr1 mixPair xs of
         ASCII v -> ASCII $ s ++ " [" ++ v ++ "]"
   initial = ASCII "init"
   exit = ASCII "exit"
   sectionNode (ASCII s) (ASCII x) = ASCII $ s ++ "." ++ x

   directionIn = ASCII shortIn
   directionOut = ASCII shortOut
   delta (ASCII s) = ASCII $ 'd':s
   energy = ASCII "E"
   maxEnergy = ASCII "Em"
   power = ASCII "P"
   xfactor = ASCII "x"
   eta = ASCII "n"
   dtime = ASCII "dt"
   signalSum = ASCII "SS"
   scalarSum = ASCII "Ss"
   storage = ASCII "s"

   parenthesize (ASCII x) = ASCII $ "(" ++ x ++ ")"
   negate (ASCII x) = ASCII $ '-' : x
   recip (ASCII x) = ASCII $ "1/(" ++ x ++ ")"
   plus (ASCII x) (ASCII y) = ASCII $ x ++ " + " ++ y
   minus (ASCII x) (ASCII y) = ASCII $ x ++ " - " ++ y
   multiply (ASCII x) (ASCII y) = ASCII $ x ++ " * " ++ y
   intPower (ASCII x) n = ASCII $ x ++ "^" ++ showsPrec 10 n ""
   showRaw (ASCII x) = x

instance Format Unicode where
   literal = Unicode
   integer = Unicode . show
   real = Unicode . realExp
   ratio r =
      Unicode $
      Map.findWithDefault
         (show (numerator r) ++ "/" ++ show (denominator r))
         r ratioCharMap

   subscript (Unicode t) (Unicode s) = Unicode $ t ++ "_" ++ s
   connect (Unicode t) (Unicode s) = Unicode $ t ++ "_" ++ s
   link (Unicode t) (Unicode s) = Unicode $ t ++ ":" ++ s
   list = Unicode . ("["++) . (++"]") . intercalate "," . map unUnicode
   undetermined = Unicode [heartChar]
   empty = Unicode ""
   lines = Unicode . unlines . map unUnicode
   words = Unicode . unwords . map unUnicode
   assign (Unicode lhs) (Unicode rhs) =
      Unicode $ lhs ++ " = " ++ rhs
   pair (Unicode lhs) (Unicode rhs) =
      Unicode $ "(" ++ lhs ++ ", " ++ rhs ++ ")"

   nodeInt (Unicode typ) (Unicode num) = Unicode $ typ++num
   nodeString (Unicode typ) (Unicode num) = Unicode $ typ++"-"++num
   nodeStorage = Unicode "St"
   nodeSink = Unicode "Si"
   nodeAlwaysSink = Unicode "ASi"
   nodeSource = Unicode "So"
   nodeAlwaysSource = Unicode "ASo"
   nodeCrossing = Unicode "C"
   nodeDeadNode = Unicode "D"
   nodeNoRestriction = Unicode "Any"

   function f (Unicode rest) =
      Unicode $
      case f of
         Absolute -> "|" ++ rest ++ "|"
         Signum -> "sgn(" ++ rest ++ ")"
         ConstOne -> "\x2474(" ++ rest ++ ")"
   integral (Unicode x) = Unicode $ "\x222B(" ++ x ++ ")"
   recordDelta d (Unicode rest) =
      Unicode $ (++rest) $
      case d of
         RecIdx.Before -> "\x2070"
         RecIdx.After -> "\xb9"
         RecIdx.Delta -> [deltaChar]
   mixComponent (Unicode c) (Unicode x) = Unicode $ c ++ "?" ++ x
   mixPair (Unicode x) (Unicode y) = Unicode $ x ++ "," ++ y
   mix (Unicode s) xs =
      case foldr1 mixPair xs of
         Unicode v -> Unicode $ s ++ " [" ++ v ++ "]"
   initial = Unicode "init"
   exit = Unicode "exit"
   sectionNode (Unicode s) (Unicode x) = Unicode $ s ++ "." ++ x

   directionIn = Unicode shortIn
   directionOut = Unicode shortOut
   delta (Unicode s) = Unicode $ deltaChar:s
   energy = Unicode "E"
   maxEnergy = Unicode "\xCA"
   power = Unicode "P"
   xfactor = Unicode "x"
   eta = Unicode "\x03B7"
   dtime = Unicode "dt"
   signalSum = Unicode "\x03A3"
   scalarSum = Unicode "\x03C3"
   storage = Unicode "s"

   parenthesize (Unicode x) = Unicode $ "(" ++ x ++ ")"
   negate (Unicode x) = Unicode $ '-' : x
   recip (Unicode x) = Unicode $ "\x215f(" ++ x ++ ")"
   plus (Unicode x) (Unicode y) = Unicode $ x ++ " + " ++ y
   minus (Unicode x) (Unicode y) = Unicode $ x ++ " - " ++ y
   multiply (Unicode x) (Unicode y) = Unicode $ x ++ "\xb7" ++ y
   intPower (Unicode x) n =
      -- writing many digits in superscript looks ugly in a monospace font
      let super c =
             case c of
                '0' -> '\x2070'
                '1' -> '\xb9'
                '2' -> '\xb2'
                '3' -> '\xb3'
                '4' -> '\x2074'
                '5' -> '\x2075'
                '6' -> '\x2076'
                '7' -> '\x2077'
                '8' -> '\x2078'
                '9' -> '\x2079'
                '-' -> '\x207B'
                _ -> c
      in  Unicode $ x ++ map super (show n)
   showRaw (Unicode x) = x

ratioCharMap :: Integral a => Map (Ratio a) String
ratioCharMap =
   let xys =
          fmap (:[]) $
          Map.fromList $
          (1/4, '\xbc') :
          (1/2, '\xbd') :
          (3/4, '\xbe') :
          (1/7, '\x2150') :
          (1/9, '\x2151') :
          (1/10,'\x2152') :
          (1/3, '\x2153') :
          (2/3, '\x2154') :
          (1/5, '\x2155') :
          (2/5, '\x2156') :
          (3/5, '\x2157') :
          (4/5, '\x2158') :
          (1/6, '\x2159') :
          (5/6, '\x215A') :
          (1/8, '\x215B') :
          (3/8, '\x215C') :
          (5/8, '\x215D') :
          (7/8, '\x215E') :
          []
   in  Map.union xys (fmap ('-':) $ Map.mapKeys P.negate xys)


instance Format Latex where
   -- we may use the 'latex' package for escaping non-ASCII characters
   literal = Latex
   integer = Latex . show
   real = Latex . printf "%.6f"
   ratio r = Latex $ "\\frac{" ++ show (numerator r) ++ "}{" ++ show (denominator r) ++ "}"
   subscript (Latex t) (Latex s) = Latex $ t ++ "_{" ++ s ++ "}"
   connect (Latex t) (Latex s) = Latex $ t ++ "." ++ s
   link (Latex t) (Latex s) = Latex $ t ++ ":" ++ s
   list = Latex . ("["++) . (++"]") . intercalate ", " . map unLatex
   undetermined = Latex "\\heartsuit "
   empty = Latex ""
   lines = Latex . intercalate "\\\\\n" . map unLatex
   words = Latex . unwords . map unLatex
   assign (Latex lhs) (Latex rhs) =
      Latex $ lhs ++ " = " ++ rhs
   pair (Latex lhs) (Latex rhs) =
      Latex $ "(" ++ lhs ++ ", " ++ rhs ++ ")"

   nodeInt (Latex typ) (Latex num) = Latex $ typ++num
   nodeString (Latex typ) (Latex num) = Latex $ typ++"-"++num
   nodeStorage = Latex "St"
   nodeSink = Latex "Si"
   nodeAlwaysSink = Latex "ASi"
   nodeSource = Latex "So"
   nodeAlwaysSource = Latex "ASo"
   nodeCrossing = Latex "C"
   nodeDeadNode = Latex "D"
   nodeNoRestriction = Latex "Any"

   function f (Latex rest) =
      Latex $
      case f of
         Absolute -> "\\abs{" ++ rest ++ "}"
         Signum -> "\\sgn{\\left(" ++ rest ++ "\\right)}"
         ConstOne -> "\\mathbb{1}(" ++ rest ++ ")"
   integral (Latex x) = Latex $ "\\int\\left(" ++ x ++ "\\right)"
   recordDelta d (Latex rest) =
      Latex $
      case d of
         {-
         http://math.mit.edu/~ssam/latex
         \newcommand{\leftexp}[2]{{\vphantom{#2}}^{#1}{#2}}
         alternatively use packages leftidx or tensor:
         http://tex.stackexchange.com/questions/11542/left-and-right-subscript
         -}
         RecIdx.Before -> "\\leftexp{0}{" ++ rest ++ "}"
         RecIdx.After -> "\\leftexp{1}{" ++ rest ++ "}"
         RecIdx.Delta -> "\\Delta " ++ rest
   mixComponent (Latex c) (Latex x) = Latex $ c ++ "?" ++ x
   mixPair (Latex x) (Latex y) = Latex $ x ++ "," ++ y
   mix (Latex s) xs =
      case foldr1 mixPair xs of
         Latex v -> Latex $ s ++ " [" ++ v ++ "]"
   initial = Latex "\\mbox{init}"
   exit = Latex "\\mbox{exit}"
   sectionNode (Latex s) (Latex x) = Latex $ s ++ ":" ++ x

   directionIn = Latex shortIn
   directionOut = Latex shortOut
   delta (Latex s) = Latex $ "\\Delta " ++ s
   energy = Latex "E"
   maxEnergy = Latex "\\^E"
   power = Latex "P"
   xfactor = Latex "x"
   eta = Latex "\\eta"
   dtime = Latex "\\dif t"
   signalSum = Latex "\\Sigma"
   scalarSum = Latex "\\sigma"
   storage = Latex "s"

   parenthesize (Latex x) = Latex $ "(" ++ x ++ ")"
   negate (Latex x) = Latex $ '-' : x
   recip (Latex x) = Latex $ "\\frac{1}{" ++ x ++ "}"
   plus (Latex x) (Latex y) = Latex $ x ++ " + " ++ y
   minus (Latex x) (Latex y) = Latex $ x ++ " - " ++ y
   multiply (Latex x) (Latex y) = Latex $ x ++ " \\cdot " ++ y
   intPower (Latex x) n = Latex $ x ++ "^{" ++ show n ++ "}"
   showRaw (Latex x) = x


ratioAuto :: (Integral a, Show a, Format output) => Ratio a -> output
ratioAuto r =
   if denominator r == 1
     then integer $ toInteger $ numerator r
     else ratio r



-- | actual though -- for usable figures
realExp :: (RealFrac a, PrintfArg a) => a -> String
realExp x =
   case round x of
      xi ->
         if' (abs x < 100 && x == fromInteger xi) (show xi) $
         if' (abs x < 1) (printf "%.5f" x) $
         if' (abs x < 10) (printf "%.4f" x) $
         if' (abs x < 100) (printf "%.3f" x) $
         if' (abs x < 1000) (printf "%.2f" x) $
         if' (abs x < 10000) (printf "%.1f" x) $
         if' (abs x < 1e6) (printf "%.0f" x) $
         if' (abs x < 1e9) (printf "%.3f E6" (x*1e-6)) $
         if' (abs x < 1e12) (printf "%.3f E9" (x*1e-9)) $
         if' (abs x < 1e15) (printf "%.3f E12" (x*1e-12)) $
         (printf "%.e" x)


class Record record where
   record :: Format output => record -> output -> output

instance Record RecIdx.Absolute where
   record RecIdx.Absolute = id

instance Record RecIdx.Delta where
   record = recordDelta

instance Record rec => Record (RecIdx.ExtDelta rec) where
   record (RecIdx.ExtDelta d r) = recordDelta d . record r


class MixRecord len where
   mixRecord ::
      (Mix.Direction dir, Format output) =>
      RecIdx.Mix dir len -> output -> output

instance (FixedLength.C list) => MixRecord (FixedLength.WrapPos list) where
   mixRecord RecIdx.MixTotal = id
   mixRecord (RecIdx.MixComponent pos) =
      mixComponent (integer $ fromIntegral $ FixedLength.numFromPos pos)

instance (Mix.Direction dir, MixRecord len) => Record (RecIdx.Mix dir len) where
   record = mixRecord

instance
   (Mix.Direction dir, MixRecord len, Record rec) =>
      Record (RecIdx.ExtMix dir len rec) where
   record (RecIdx.ExtMix m r) = mixRecord m . record r



shortIn, shortOut :: String
shortIn = "i" -- Verwirrung mit initial aus der Format-Klasse?
shortOut = "o"
