module EFA2.Report.Format where

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Interpreter.Env as Env

import Data.List (intercalate)
import Data.Ratio (Ratio, numerator, denominator)

import Text.Printf (PrintfArg, printf)


-- * special Unicode characters

deltaChar :: Char
deltaChar = '\x2206'

heartChar :: Char
heartChar = '\x2665'


-- * common output types

newtype Plain = Plain { unPlain :: String }

newtype Latex = Latex { unLatex :: String }


-- * class for unified handling of Plain and Latex output

data Mode = Absolute | Delta
data EdgeVar = Energy | MaxEnergy | Power | Eta | X | Y

class Format output where
   integer :: Integer -> output
   real :: (Floating a, PrintfArg a) => a -> output
   ratio :: (Integral a, Show a) => Ratio a -> output
   subscript :: output -> output -> output
   connect :: output -> output -> output
   list :: [output] -> output
   undetermined :: output
   empty :: output
   newLine :: output
   assign :: output -> output -> output

   record :: Idx.Record -> output
   section :: Idx.Section -> output
   sectionNode :: Idx.SecNode -> output
   use :: Idx.Use -> output
   delta :: output -> output
   edgeIdent :: EdgeVar -> output
   time, var, storage :: output
   parenthesize, minus, recip :: output -> output
   plus, multiply :: output -> output -> output
   power :: output -> Integer -> output

instance Format Plain where
   integer = Plain . show
   -- real = Plain . show
   real = Plain . printf "%.6f"
   ratio r = Plain $ show (numerator r) ++ "/" ++ show (denominator r)
   subscript (Plain t) (Plain s) = Plain $ t ++ "_" ++ s
   connect (Plain t) (Plain s) = Plain $ t ++ "_" ++ s
   list = Plain . ("["++) . (++"]") . intercalate "," . map unPlain
   undetermined = Plain [heartChar]
   empty = Plain ""
   newLine = Plain "\n"
   assign (Plain lhs) (Plain rhs) =
      Plain $ lhs ++ " = " ++ rhs

   record (Idx.Record r) = Plain $ show r
   section (Idx.Section s) = Plain $ show s
   sectionNode (Idx.SecNode (Idx.Section s) (Idx.Node x)) =
      Plain $ show s ++ "." ++ show x

   use = Plain . show
   delta (Plain s) = Plain $ deltaChar:s
   edgeIdent = Plain . edgeString
   time = Plain "t"
   var = Plain "v"
   storage = Plain "s"

   parenthesize (Plain x) = Plain $ "(" ++ x ++ ")"
   minus (Plain x) = Plain $ '-' : x
   recip (Plain x) = Plain $ "1/(" ++ x ++ ")"
   plus (Plain x) (Plain y) = Plain $ x ++ " + " ++ y
   multiply (Plain x) (Plain y) = Plain $ x ++ " * " ++ y
   power (Plain x) n = Plain $ x ++ "^" ++ showsPrec 10 n ""

instance Format Latex where
   integer = Latex . show
   real = Latex . printf "%.6f"
   ratio r = Latex $ "\\frac{" ++ show (numerator r) ++ "}{" ++ show (denominator r) ++ "}"
   subscript (Latex t) (Latex s) = Latex $ t ++ "_{" ++ s ++ "}"
   connect (Latex t) (Latex s) = Latex $ t ++ "." ++ s
   list = Latex . ("["++) . (++"]") . intercalate ", " . map unLatex
   undetermined = Latex "\\heartsuit "
   empty = Latex ""
   newLine = Latex "\\\\\n"
   assign (Latex lhs) (Latex rhs) =
      Latex $ lhs ++ " = " ++ rhs

   record (Idx.Record r) = Latex $ show r
   section (Idx.Section s) = Latex $ show s
   sectionNode (Idx.SecNode (Idx.Section s) (Idx.Node x)) =
      Latex $ show s ++ ":" ++ show x

   use = Latex . show
   delta (Latex s) = Latex $ "\\Delta " ++ s
   edgeIdent = Latex . edgeString
   time = Latex "t"
   var = Latex "v"
   storage = Latex "s"

   parenthesize (Latex x) = Latex $ "(" ++ x ++ ")"
   minus (Latex x) = Latex $ '-' : x
   recip (Latex x) = Latex $ "\\frac{1}{" ++ x ++ "}"
   plus (Latex x) (Latex y) = Latex $ x ++ " + " ++ y
   multiply (Latex x) (Latex y) = Latex $ x ++ " \\cdot " ++ y
   power (Latex x) n = Latex $ x ++ "^{" ++ show n ++ "}"

edgeIndex ::
   Format output =>
   Idx.Record -> Idx.SecNode -> Idx.SecNode -> output
edgeIndex r x y =
   record r `connect` sectionNode x `connect` sectionNode y

edgeVar ::
   Format output =>
   Mode -> EdgeVar -> Idx.Record -> Idx.SecNode -> Idx.SecNode -> output
edgeVar mode e r x y =
   case mode of
      Absolute -> subscript (edgeIdent e) (edgeIndex r x y)
      Delta -> subscript (delta $ edgeIdent e) (edgeIndex r x y)

edgeString :: EdgeVar -> String
edgeString e =
   case e of
      Energy -> "e"
      MaxEnergy -> "me"
      Power -> "p"
      X -> "x"
      Y -> "y"
      Eta -> "n"

index :: Format output => Env.Index -> output
index idx =
   case idx of
      Env.Energy (Idx.Energy r x y) -> edgeVar Absolute Energy r x y
      Env.DEnergy (Idx.DEnergy r x y) -> edgeVar Delta Energy r x y

      Env.MaxEnergy (Idx.MaxEnergy r x y) -> edgeVar Absolute MaxEnergy r x y
      Env.DMaxEnergy (Idx.DMaxEnergy r x y) -> edgeVar Delta MaxEnergy r x y

      Env.Power (Idx.Power r x y) -> edgeVar Absolute Power r x y
      Env.DPower (Idx.DPower r x y) -> edgeVar Delta Power r x y

      Env.FEta (Idx.FEta r x y) -> edgeVar Absolute Eta r x y
      Env.DEta (Idx.DEta r x y) -> edgeVar Delta Eta r x y

      Env.X (Idx.X r x y) -> edgeVar Absolute X r x y
      Env.DX (Idx.DX r x y) -> edgeVar Delta X r x y

      Env.Y (Idx.Y r x y) -> edgeVar Absolute Y r x y
      Env.DY (Idx.DY r x y) -> edgeVar Delta Y r x y

      Env.DTime (Idx.DTime r s) ->
         subscript (delta time) (record r `connect` section s)

      Env.Var (Idx.Var r u x) ->
         subscript var $ record r `connect` use u `connect` sectionNode x

      Env.Store (Idx.Storage r x) ->
         subscript storage $ record r `connect` sectionNode x
