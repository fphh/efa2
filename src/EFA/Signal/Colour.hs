

module EFA.Signal.Colour where

import qualified Data.Map as M


import Data.Colour
import Data.Colour.Names as Colour
import Data.Colour.SRGB (sRGB24show, sRGB24read, toSRGB24, RGB(..))
import qualified Data.GraphViz.Attributes.Colors as GV

newtype C = C { unC :: String } deriving (Show)

showf :: Colour Double -> C
showf = C . sRGB24show



class ConvertColour c where
      convertColour :: C -> c

instance ConvertColour GV.Color where
         convertColour (C str) =
           case toSRGB24 $ sRGB24read str of
                RGB r g b -> GV.RGB r g b


colourMap :: (Ord a) => [a] -> M.Map a C
colourMap xs = M.fromList $ zip xs colours

defltColour :: C
defltColour = showf red

colours :: [C]
colours = cls ++ colours

cls :: [C]
cls = map showf [
  red,
  blue,
  yellow,
  green,
  orange,
  gray,
  magenta,
  cyan,
  chocolate,
  olive,
  purple,
  white,
  violet,
  lightblue,
  darkblue,
  lightcoral,
  darkcyan,
  lightcyan,
  darkgoldenrod,
  lightgoldenrodyellow,
  darkgray,
  lightgray,
  darkgreen,
  lightgreen,
  darkkhaki,
  lightpink,
  darkmagenta,
  lightsalmon,
  darkolivegreen,
  lightseagreen,
  darkorange,
  lightskyblue,
  darkorchid,
  lightslategray,
  darkred,
  lightslategrey,
  darksalmon,
  lightsteelblue,
  darkseagreen,
  lightyellow,
  darkslateblue,
  darkslategray,
  darkturquoise,
  darkviolet,
  deeppink,
  deepskyblue,
  dimgray,
  dodgerblue,
  firebrick,
  floralwhite,
  forestgreen,
  fuchsia,
  gainsboro,
  ghostwhite,
  gold,
  goldenrod,
  greenyellow,
  honeydew,
  hotpink,
  indianred,
  indigo,
  ivory,
  khaki,
  lavender,
  lavenderblush,
  lawngreen,
  lemonchiffon,
  lime,
  limegreen,
  linen,
  maroon,
  mediumaquamarine,
  mediumblue,
  mediumorchid,
  mediumpurple,
  mediumseagreen,
  mediumslateblue,
  mediumspringgreen,
  mediumturquoise,
  mediumvioletred,
  midnightblue,
  mintcream,
  mistyrose,
  moccasin,
  navajowhite,
  navy,
  oldlace,
  olivedrab,
  orangered,
  orchid,
  palegoldenrod,
  palegreen,
  paleturquoise,
  palevioletred,
  aliceblue,
  antiquewhite,
  aqua,
  aquamarine,
  azure,
  beige,
  bisque,
  black,
  blanchedalmond,
  blueviolet,
  brown,
  burlywood,
  cadetblue,
  chartreuse,
  coral,
  cornflowerblue,
  cornsilk,
  crimson,
  papayawhip,
  peachpuff,
  peru,
  pink,
  plum,
  powderblue,
  rosybrown,
  royalblue,
  saddlebrown,
  salmon,
  sandybrown,
  seagreen,
  seashell,
  sienna,
  silver,
  skyblue,
  slateblue,
  slategray,
  snow,
  springgreen,
  steelblue,
  Colour.tan,
  teal,
  thistle,
  tomato,
  turquoise,
  wheat,
  whitesmoke,
  yellowgreen ]
