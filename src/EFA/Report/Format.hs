module EFA.Report.Format where

import qualified EFA.Graph.Topology.Index as Idx

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Bool.HT (if')
import Data.List (intercalate)
import Data.Ratio (Ratio, numerator, denominator)

import Text.Printf (PrintfArg, printf)

import qualified Prelude as P
import Prelude hiding (words, lines, sum, negate)


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

data EdgeVar = Energy | MaxEnergy | Power | Eta | X

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

   function :: Function -> output -> output
   integral :: output -> output
   recordDelta :: Idx.Delta -> output -> output
   initial, exit :: output
   section :: Idx.Section -> output
   state :: Idx.State -> output
   sectionNode :: output -> output -> output
   direction :: Idx.Direction -> output
   delta :: output -> output
   edgeIdent :: EdgeVar -> output
   dtime, signalSum, scalarSum, storage :: output
   parenthesize, negate, recip :: output -> output
   plus, minus, multiply :: output -> output -> output
   power :: output -> Integer -> output
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
         Idx.Before -> "[0]"
         Idx.After -> "[1]"
         Idx.Delta -> "d"
   initial = ASCII "init"
   exit = ASCII "exit"
   section (Idx.Section s) = ASCII $ show s
   state (Idx.State s) = ASCII $ show s
   sectionNode (ASCII s) (ASCII x) = ASCII $ s ++ "." ++ x

   direction = ASCII . directionShort
   delta (ASCII s) = ASCII $ 'd':s
   edgeIdent e =
      ASCII $
      case e of
         Energy -> "E"
         MaxEnergy -> "Em"
         Power -> "P"
         X -> "x"
         Eta -> "n"
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
   power (ASCII x) n = ASCII $ x ++ "^" ++ showsPrec 10 n ""
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
         Idx.Before -> "\x2070"
         Idx.After -> "\xb9"
         Idx.Delta -> [deltaChar]
   initial = Unicode "init"
   exit = Unicode "exit"
   section (Idx.Section s) = Unicode $ show s
   state (Idx.State s) = Unicode $ show s
   sectionNode (Unicode s) (Unicode x) = Unicode $ s ++ "." ++ x

   direction = Unicode . directionShort
   delta (Unicode s) = Unicode $ deltaChar:s
   edgeIdent e =
      Unicode $
      case e of
         Energy -> "E"
         MaxEnergy -> "\xCA"
         Power -> "P"
         X -> "x"
         Eta -> "\x03B7"
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
   power (Unicode x) n =
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
         Idx.Before -> "\\leftexp{0}{" ++ rest ++ "}"
         Idx.After -> "\\leftexp{1}{" ++ rest ++ "}"
         Idx.Delta -> "\\Delta " ++ rest
   initial = Latex "\\mbox{init}"
   exit = Latex "\\mbox{exit}"
   section (Idx.Section s) = Latex $ show s
   state (Idx.State s) = Latex $ show s
   sectionNode (Latex s) (Latex x) = Latex $ s ++ ":" ++ x

   direction = Latex . directionShort
   delta (Latex s) = Latex $ "\\Delta " ++ s
   edgeIdent e =
      Latex $
      case e of
         Energy -> "E"
         MaxEnergy -> "\\^E"
         Power -> "P"
         X -> "x"
         Eta -> "\\eta"
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
   power (Latex x) n = Latex $ x ++ "^{" ++ show n ++ "}"
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

instance Record Idx.Absolute where
   record Idx.Absolute = id

instance Record Idx.Delta where
   record = recordDelta

instance Record rec => Record (Idx.ExtDelta rec) where
   record (Idx.ExtDelta d r) = recordDelta d . record r


class EdgeIdx idx where edgeVar :: idx node -> EdgeVar

instance StructureIdx idx => EdgeIdx (Idx.InSection idx) where
   edgeVar = structureVar

class StructureIdx idx where structureVar :: Idx.InSection idx node -> EdgeVar
instance StructureIdx Idx.Energy where structureVar _ = Energy
instance StructureIdx Idx.Power where structureVar _ = Power
instance StructureIdx Idx.Eta where structureVar _ = Eta
instance StructureIdx Idx.X where structureVar _ = X

instance StorageIdx idx => EdgeIdx (Idx.ForNode idx) where
   edgeVar = storageVar

class StorageIdx idx where storageVar :: Idx.ForNode idx node -> EdgeVar
instance StorageIdx Idx.MaxEnergy where storageVar _ = MaxEnergy
instance StorageIdx (Idx.StEnergy sec) where storageVar _ = Energy
instance StorageIdx (Idx.StX sec) where storageVar _ = X


directionShort :: Idx.Direction -> String
directionShort d =
   case d of
      Idx.In -> "i" -- Verwirrung mit initial aus der Format-Klasse?
      Idx.Out -> "o"

boundary :: Format output => Idx.Boundary -> output
boundary (Idx.Following Idx.Init) = initial
boundary (Idx.Following (Idx.NoInit s)) = section s

class Part sec where part :: Format output => sec -> output
instance Part Idx.Section where part = section
instance Part Idx.State where part = state

initOrOther :: (Part sec, Format output) => Idx.Init sec -> output
initOrOther (Idx.Init) = initial
initOrOther (Idx.NoInit s) = part s

otherOrExit :: (Part sec, Format output) => Idx.Exit sec -> output
otherOrExit (Idx.NoExit s) = part s
otherOrExit (Idx.Exit) = exit

augmented :: (Part sec, Format output) => Idx.Augmented sec -> output
augmented =
   Idx.switchAugmented initial exit part
