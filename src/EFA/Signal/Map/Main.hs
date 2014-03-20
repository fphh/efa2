import EFA.Signal.Map.Ortho (Ortho)
import qualified EFA.Signal.Map.Ortho as Ortho

import EFA.Signal.Map.Dimension (Dim2)
import qualified EFA.Signal.Map.Dimension as Dim
import qualified EFA.Signal.Map.Axes as Axes
import qualified Data.Vector as V
import qualified EFA.Signal.Interp as Interp

interpFunction = Interp.dim1 "main" Interp.Linear Interp.ExtrapLinear

main = do
  let x = V.fromList ["x1","x2"]
  let y = V.fromList ["y1","y2"]
  let z = V.fromList ["z11","z12","z21","z22"]
  let o = Ortho.create "main" [x,y] z :: Ortho Dim2 V.Vector String String
  let z11 = Ortho.lookupLin "Main" o (Ortho.LinIdx 0)
  let z12 = Ortho.lookupLin "Main" o (Ortho.LinIdx 1)
  let z21 = Ortho.lookupLin "Main" o (Ortho.LinIdx 2)
  let z22 = Ortho.lookupLin "Main" o (Ortho.LinIdx 3)
  let z11' = Ortho.lookUp "Main" (Dim.Data $ map Axes.Idx [0,0]) o
  let z12' = Ortho.lookUp "Main" (Dim.Data $ map Axes.Idx [0,1]) o
  let z21' = Ortho.lookUp "Main" (Dim.Data $ map Axes.Idx [1,0]) o
  let z22' = Ortho.lookUp "Main" (Dim.Data $ map Axes.Idx [1,1]) o

  let x1 = V.fromList [1,2]
  let y1 = V.fromList [3,4]
  let z1 = V.fromList [11,12,21,22]
  let o1 = Ortho.create "main" [x1,y1] z1 :: Ortho Dim2 V.Vector Double Double
  let zInt = Ortho.interpolate "Main" interpFunction o1 (Dim.Data [1,3])
  let zInt2 = Ortho.interpolate "Main" interpFunction o1 (Dim.Data [1,4])
  let zInt3 = Ortho.interpolate "Main" interpFunction o1 (Dim.Data [2,3])
  let zInt4 = Ortho.interpolate "Main" interpFunction o1 (Dim.Data [2,4])
  let zInt5 = Ortho.interpolate "Main" interpFunction o1 (Dim.Data [1.5,3])
  let zInt6 = Ortho.interpolate "Main" interpFunction o1 (Dim.Data [1.5,4])
  let zInt7 = Ortho.interpolate "Main" interpFunction o1 (Dim.Data [1,3.5])
  let zInt8 = Ortho.interpolate "Main" interpFunction o1 (Dim.Data [2,3.5])
  let zInt9 = Ortho.interpolate "Main" interpFunction o1 (Dim.Data [1.5,3.5])

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


