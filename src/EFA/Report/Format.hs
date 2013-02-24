module EFA.Report.Format where

import qualified EFA.Graph.Topology.Index as Idx

import qualified Data.Map as M

import Data.List (intercalate)
import Data.Ratio (Ratio, numerator, denominator)

import Text.Printf (PrintfArg, printf)

import Prelude hiding (words, lines)


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

data EdgeVar = Energy | MaxEnergy | Power | Eta | X | Y

class Format output where
   literal :: String -> output
   integer :: Integer -> output
   real :: (Floating a, PrintfArg a) => a -> output
   ratio :: (Integral a, Show a) => Ratio a -> output
   subscript :: output -> output -> output
   connect :: output -> output -> output
   list :: [output] -> output
   undetermined :: output
   empty :: output
   words, lines :: [output] -> output
   assign :: output -> output -> output

   recordDelta :: Idx.Delta -> output -> output
   section :: Idx.Section -> output
   sectionNode :: output -> output -> output
   direction :: Idx.Direction -> output
   delta :: output -> output
   edgeIdent :: EdgeVar -> output
   time, var, storage :: output
   parenthesize, minus, recip :: output -> output
   plus, multiply :: output -> output -> output
   power :: output -> Integer -> output

instance Format ASCII where
   -- may need some escaping for non-ASCII characters
   literal = ASCII
   integer = ASCII . show
   -- real = ASCII . show
   real = ASCII . printf "%.6f"
   ratio r = ASCII $ show (numerator r) ++ "/" ++ show (denominator r)
   subscript (ASCII t) (ASCII s) = ASCII $ t ++ "_" ++ s
   connect (ASCII t) (ASCII s) = ASCII $ t ++ "_" ++ s
   list = ASCII . ("["++) . (++"]") . intercalate "," . map unASCII
   undetermined = ASCII "?"
   empty = ASCII ""
   lines = ASCII . unlines . map unASCII
   words = ASCII . unwords . map unASCII
   assign (ASCII lhs) (ASCII rhs) =
      ASCII $ lhs ++ " = " ++ rhs

   recordDelta d (ASCII rest) =
      ASCII $ (++rest) $
      case d of
         Idx.Before -> "[0]"
         Idx.After -> "[1]"
         Idx.Delta -> "d"
   section (Idx.Section s) = ASCII $ show s
   sectionNode (ASCII s) (ASCII x) = ASCII $ s ++ "." ++ x

   direction = ASCII . show
   delta (ASCII s) = ASCII $ 'd':s
   edgeIdent e =
      ASCII $
      case e of
         Energy -> "e"
         MaxEnergy -> "me"
         Power -> "p"
         X -> "x"
         Y -> "y"
         Eta -> "n"
   time = ASCII "t"
   var = ASCII "v"
   storage = ASCII "s"

   parenthesize (ASCII x) = ASCII $ "(" ++ x ++ ")"
   minus (ASCII x) = ASCII $ '-' : x
   recip (ASCII x) = ASCII $ "1/(" ++ x ++ ")"
   plus (ASCII x) (ASCII y) = ASCII $ x ++ " + " ++ y
   multiply (ASCII x) (ASCII y) = ASCII $ x ++ " * " ++ y
   power (ASCII x) n = ASCII $ x ++ "^" ++ showsPrec 10 n ""

instance Format Unicode where
   literal = Unicode
   integer = Unicode . show
   -- real = Unicode . show
   real = Unicode . printf "%.6f"
   ratio r =
      Unicode $
      M.findWithDefault
         (show (numerator r) ++ "/" ++ show (denominator r))
         r ratioCharMap

   subscript (Unicode t) (Unicode s) = Unicode $ t ++ "_" ++ s
   connect (Unicode t) (Unicode s) = Unicode $ t ++ "_" ++ s
   list = Unicode . ("["++) . (++"]") . intercalate "," . map unUnicode
   undetermined = Unicode [heartChar]
   empty = Unicode ""
   lines = Unicode . unlines . map unUnicode
   words = Unicode . unwords . map unUnicode
   assign (Unicode lhs) (Unicode rhs) =
      Unicode $ lhs ++ " = " ++ rhs

   recordDelta d (Unicode rest) =
      Unicode $ (++rest) $
      case d of
         Idx.Before -> "\x2070"
         Idx.After -> "\xb9"
         Idx.Delta -> [deltaChar]
   section (Idx.Section s) = Unicode $ show s
   sectionNode (Unicode s) (Unicode x) = Unicode $ s ++ "." ++ x

   direction = Unicode . show
   delta (Unicode s) = Unicode $ deltaChar:s
   edgeIdent e =
      Unicode $
      case e of
         Energy -> "e"
         MaxEnergy -> "me"
         Power -> "p"
         X -> "x"
         Y -> "y"
         Eta -> "\x03b7"
   time = Unicode "t"
   var = Unicode "v"
   storage = Unicode "s"

   parenthesize (Unicode x) = Unicode $ "(" ++ x ++ ")"
   minus (Unicode x) = Unicode $ '-' : x
   recip (Unicode x) = Unicode $ "\x215f(" ++ x ++ ")"
   plus (Unicode x) (Unicode y) = Unicode $ x ++ " + " ++ y
   multiply (Unicode x) (Unicode y) = Unicode $ x ++ "\xb7" ++ y
   power (Unicode x) n =
      Unicode $ x ++
         case n of
            1 -> "\xb9"
            2 -> "\xb2"
            3 -> "\xb3"
            4 -> "\x2074"
            5 -> "\x2075"
            6 -> "\x2076"
            7 -> "\x2077"
            8 -> "\x2078"
            9 -> "\x2079"
            _ -> "^" ++ showsPrec 10 n ""

ratioCharMap :: Integral a => M.Map (Ratio a) String
ratioCharMap =
   let xys =
          fmap (:[]) $
          M.fromList $
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
   in  M.union xys (fmap ('-':) $ M.mapKeys negate xys)


instance Format Latex where
   -- we may use the 'latex' package for escaping non-ASCII characters
   literal = Latex
   integer = Latex . show
   real = Latex . printf "%.6f"
   ratio r = Latex $ "\\frac{" ++ show (numerator r) ++ "}{" ++ show (denominator r) ++ "}"
   subscript (Latex t) (Latex s) = Latex $ t ++ "_{" ++ s ++ "}"
   connect (Latex t) (Latex s) = Latex $ t ++ "." ++ s
   list = Latex . ("["++) . (++"]") . intercalate ", " . map unLatex
   undetermined = Latex "\\heartsuit "
   empty = Latex ""
   lines = Latex . intercalate "\\\\\n" . map unLatex
   words = Latex . unwords . map unLatex
   assign (Latex lhs) (Latex rhs) =
      Latex $ lhs ++ " = " ++ rhs

   recordDelta d (Latex rest) =
      Latex $
      case d of
         {-
         http://math.mit.edu/~ssam/latex
         \newcommand{\leftexp}[2]{{\vphantom{#2}}^{#1}{#2}}
         -}
         Idx.Before -> "\\leftexp{0}{" ++ rest ++ "}"
         Idx.After -> "\\leftexp{1}{" ++ rest ++ "}"
         Idx.Delta -> "\\Delta " ++ rest
   section (Idx.Section s) = Latex $ show s
   sectionNode (Latex s) (Latex x) = Latex $ s ++ ":" ++ x

   direction = Latex . show
   delta (Latex s) = Latex $ "\\Delta " ++ s
   edgeIdent e =
      Latex $
      case e of
         Energy -> "e"
         MaxEnergy -> "me"
         Power -> "p"
         X -> "x"
         Y -> "y"
         Eta -> "\\eta"
   time = Latex "t"
   var = Latex "v"
   storage = Latex "s"

   parenthesize (Latex x) = Latex $ "(" ++ x ++ ")"
   minus (Latex x) = Latex $ '-' : x
   recip (Latex x) = Latex $ "\\frac{1}{" ++ x ++ "}"
   plus (Latex x) (Latex y) = Latex $ x ++ " + " ++ y
   multiply (Latex x) (Latex y) = Latex $ x ++ " \\cdot " ++ y
   power (Latex x) n = Latex $ x ++ "^{" ++ show n ++ "}"


class Idx.Record record => Record record where
   record :: Format output => record -> output -> output

instance Record Idx.Absolute where
   record Idx.Absolute = id

instance Record Idx.Delta where
   record d = recordDelta d
