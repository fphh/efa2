import EFA.Signal.Map.Cube (Cube)
import qualified EFA.Signal.Map.Cube as Cube

import EFA.Signal.Map.Dimension (Dim2)
import EFA.Utility (newCaller)
import qualified EFA.Signal.Map.Dimension as Dim
import qualified EFA.Signal.Map.Coordinate as Coord
import qualified Data.Vector as V
import qualified EFA.Signal.Interp as Interp

interpFunction :: 
  (Double, Double) -> (Double, Double) -> 
  Double -> Interp.Val Double
interpFunction = Interp.dim1 "main" Interp.Linear Interp.ExtrapLinear

main :: IO()
main = do
  let caller = newCaller "Main" "main" 
  let x = V.fromList ["x1","x2"]
  let y = V.fromList ["y1","y2"]
  let z = V.fromList ["z11","z12","z21","z22"]
  let o = Cube.create caller [x,y] z :: Cube Dim2 V.Vector String String
  let z11 = Cube.lookupLin caller o (Cube.LinIdx 0)
  let z12 = Cube.lookupLin caller o (Cube.LinIdx 1)
  let z21 = Cube.lookupLin caller o (Cube.LinIdx 2)
  let z22 = Cube.lookupLin caller o (Cube.LinIdx 3)
  let z11' = Cube.lookUp caller (Dim.Data $ map Coord.Idx [0,0]) o
  let z12' = Cube.lookUp caller (Dim.Data $ map Coord.Idx [0,1]) o
  let z21' = Cube.lookUp caller (Dim.Data $ map Coord.Idx [1,0]) o
  let z22' = Cube.lookUp caller (Dim.Data $ map Coord.Idx [1,1]) o

  let x1 = V.fromList [1,2]
  let y1 = V.fromList [3,4]
  let z1 = V.fromList [11,12,21,22]
  let o1 = Cube.create caller [x1,y1] z1 :: Cube Dim2 V.Vector Double Double
  let zInt = Cube.interpolate caller interpFunction o1 (Dim.Data [1,3])
  let zInt2 = Cube.interpolate caller interpFunction o1 (Dim.Data [1,4])
  let zInt3 = Cube.interpolate caller interpFunction o1 (Dim.Data [2,3])
  let zInt4 = Cube.interpolate caller interpFunction o1 (Dim.Data [2,4])
  let zInt5 = Cube.interpolate caller interpFunction o1 (Dim.Data [1.5,3])
  let zInt6 = Cube.interpolate caller interpFunction o1 (Dim.Data [1.5,4])
  let zInt7 = Cube.interpolate caller interpFunction o1 (Dim.Data [1,3.5])
  let zInt8 = Cube.interpolate caller interpFunction o1 (Dim.Data [2,3.5])
  let zInt9 = Cube.interpolate caller interpFunction o1 (Dim.Data [1.5,3.5])

  print o
  print z11
  print z12
  print z21
  print z22
  print z11'
  print z12'
  print z21'
  print z22'

  print ""
  print zInt
  print zInt2
  print zInt3
  print zInt4
  print zInt5
  print zInt6
  print zInt7
  print zInt8
  print zInt9


