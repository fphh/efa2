import EFA.Signal.Map.Ortho (Ortho)
import qualified EFA.Signal.Map.Ortho as Ortho

import EFA.Signal.Map.Dimension (Dim2)
import qualified EFA.Signal.Map.Dimension as Dim
import qualified EFA.Signal.Map.Axes as Ax
import qualified Data.Vector as V


main = do
  let x = V.fromList ["x1","x2"]
  let y = V.fromList ["y1","y2"]
  let z = V.fromList ["z11","z12","z21","z22"]    
  let o = Ortho.create "main" [x,y] z :: Ortho Dim2 V.Vector String String
  let z11 = Ortho.lookupLin "Main" o (Ortho.LinIdx 0)
  let z12 = Ortho.lookupLin "Main" o (Ortho.LinIdx 1)
  let z21 = Ortho.lookupLin "Main" o (Ortho.LinIdx 2)
  let z22 = Ortho.lookupLin "Main" o (Ortho.LinIdx 3)
  let z11' = Ortho.lookup "Main" (Ax.DimIdx (Dim.Data [0,0])) o
  let z12' = Ortho.lookup "Main" (Ax.DimIdx (Dim.Data [0,1])) o
  let z21' = Ortho.lookup "Main" (Ax.DimIdx (Dim.Data [1,0])) o
  let z22' = Ortho.lookup "Main" (Ax.DimIdx (Dim.Data [1,1])) o
      
  print o
  print z11
  print z12
  print z21
  print z22
  print z11'
  print z12'
  print z21'
  print z22'
