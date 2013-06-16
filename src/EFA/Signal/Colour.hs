
module EFA.Signal.Colour where

import qualified Data.Colour.Names as Colour
import Data.Colour.SRGB (sRGB24show)
import Data.Colour (Colour)

import qualified Data.Stream as Stream
import Data.Stream (Stream)

import EFA.Utility (zipWithTraversable)
import Data.Traversable (Traversable)

newtype Name = Name { unpackName :: String } deriving (Show)

showf :: Colour Double -> Name
showf = Name . sRGB24show

adorn :: Traversable f => f a -> f (Name, a)
adorn = zipWithTraversable (,) colours

defltColour :: Name
defltColour = showf Colour.red

colours :: Stream Name
colours = Stream.cycle cls

cls :: [Name]
cls = map showf $
  Colour.red :
  Colour.blue :
  Colour.green :
  Colour.orange :
  Colour.gray :
  Colour.magenta :
  Colour.cyan :
  Colour.chocolate :
  Colour.olive :
  Colour.purple :
  Colour.lightblue :
  Colour.violet :
  Colour.darkblue :
  Colour.lightcoral :
  Colour.darkcyan :
  Colour.lightcyan :
  Colour.darkgoldenrod :
  Colour.lightgoldenrodyellow :
  Colour.darkgray :
  Colour.lightgray :
  Colour.darkgreen :
  Colour.lightgreen :
  Colour.darkkhaki :
  Colour.lightpink :
  Colour.darkmagenta :
  Colour.lightsalmon :
  Colour.darkolivegreen :
  Colour.lightseagreen :
  Colour.darkorange :
  Colour.lightskyblue :
  Colour.darkorchid :
  Colour.lightslategray :
  Colour.darkred :
  Colour.lightslategrey :
  Colour.darksalmon :
  Colour.lightsteelblue :
  Colour.darkseagreen :
  Colour.lightyellow :
  Colour.darkslateblue :
  Colour.darkslategray :
  Colour.darkturquoise :
  Colour.darkviolet :
  Colour.deeppink :
  Colour.deepskyblue :
  Colour.dimgray :
  Colour.dodgerblue :
  Colour.firebrick :
  Colour.floralwhite :
  Colour.forestgreen :
  Colour.fuchsia :
  Colour.gainsboro :
  Colour.ghostwhite :
  Colour.gold :
  Colour.goldenrod :
  Colour.greenyellow :
  Colour.honeydew :
  Colour.hotpink :
  Colour.indianred :
  Colour.indigo :
  Colour.ivory :
  Colour.khaki :
  Colour.lavender :
  Colour.lavenderblush :
  Colour.lawngreen :
  Colour.lemonchiffon :
  Colour.lime :
  Colour.limegreen :
  Colour.linen :
  Colour.maroon :
  Colour.mediumaquamarine :
  Colour.mediumblue :
  Colour.mediumorchid :
  Colour.mediumpurple :
  Colour.mediumseagreen :
  Colour.mediumslateblue :
  Colour.mediumspringgreen :
  Colour.mediumturquoise :
  Colour.mediumvioletred :
  Colour.midnightblue :
  Colour.mintcream :
  Colour.mistyrose :
  Colour.moccasin :
  Colour.navajowhite :
  Colour.navy :
  Colour.oldlace :
  Colour.olivedrab :
  Colour.orangered :
  Colour.orchid :
  Colour.palegoldenrod :
  Colour.palegreen :
  Colour.paleturquoise :
  Colour.palevioletred :
  Colour.aliceblue :
  Colour.antiquewhite :
  Colour.aqua :
  Colour.aquamarine :
  Colour.azure :
  Colour.beige :
  Colour.bisque :
  Colour.black :
  Colour.blanchedalmond :
  Colour.blueviolet :
  Colour.brown :
  Colour.burlywood :
  Colour.cadetblue :
  Colour.chartreuse :
  Colour.coral :
  Colour.cornflowerblue :
  Colour.cornsilk :
  Colour.crimson :
  Colour.papayawhip :
  Colour.peachpuff :
  Colour.peru :
  Colour.pink :
  Colour.plum :
  Colour.powderblue :
  Colour.rosybrown :
  Colour.royalblue :
  Colour.saddlebrown :
  Colour.salmon :
  Colour.sandybrown :
  Colour.seagreen :
  Colour.seashell :
  Colour.sienna :
  Colour.silver :
  Colour.skyblue :
  Colour.slateblue :
  Colour.slategray :
  Colour.snow :
  Colour.springgreen :
  Colour.steelblue :
  Colour.tan :
  Colour.teal :
  Colour.thistle :
  Colour.tomato :
  Colour.turquoise :
  Colour.wheat :
  Colour.whitesmoke :
  Colour.yellowgreen :
  Colour.yellow :
  Colour.white :
  []
