import EFA2.Signal.Vector2
import EFA2.Signal.Base
import EFA2.Display.DispVector
import EFA2.Display.DispBase

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV



val1 = DC $ DVal 1 :: DC D0 (DVal Val)
val2 = DC $ DVal 2 :: DC D0 (DVal Val)

l1 = [1..3]
l2 = [0..2]

u1 = DC $  UV.fromList l1 :: DC D1 (UVec Val)
u2 = DC $  UV.fromList l2 :: DC D1 (UVec Val)

v1 = DC  $ V.fromList l1 :: DC D1 (Vec Val)
v2 = DC  $ V.fromList l2 :: DC D1 (Vec Val)

-- rv1 = emap sign u1  -- :: EVec Sign
rv2 = dfmap sign u1
rv1 = dfmap fb v1

ru1 = dfmap f u1 
ru2 = dfmap f v1

f :: Unboxed -> Val -> Val 
f _ x = x+1

fb :: Boxed -> Val -> Val 
fb _ x = x+1

fzu :: Unboxed -> Val -> Val -> Val
fzu _ x y = x + y

fzb :: Boxed -> Val -> Val -> Val
fzb _ x y = x + y

zu1 = dzipWith fzu u1 u2  
zu2 = dzipWith fzu v1 u2  
zu3 = dzipWith fzu u1 v2 
zu4 = dzipWith fzu v1 v2 

zau1 = dzipWith fzu val1 u1 
zau2 = dzipWith fzu val1 v1 

zav1 = dzipWith fzb val1 u1 
zav2 = dzipWith fzb val1 v1 



-- zu5 = emap ((flip fzu) 1) u1

zb1 = dzipWith fzb u1 u2 
zb2 = dzipWith fzb v1 u2 
zb3 = dzipWith fzb u1 v2 
zb4 = dzipWith fzb v1 v2 

-- emap ((flip f) x) y

-- d3 = dmult undefined val1 False :: Val

u10 = dzipWith dmult u1 u2
-- u12 = 

u11 = u1 .* u2


main = do 
  putStrLn ("Demo Data-Arith")
--  putStrLn (ddisp d3)
  
  putStrLn ("Demo emap")
  putStrLn (ddisp rv1)
  putStrLn (ddisp rv2)
  putStrLn (ddisp ru1)
  putStrLn (ddisp ru2)
  
  putStrLn ("Demo ezipWith - unboxed")
  putStrLn (ddisp zu1)
  putStrLn (ddisp zu2)
  putStrLn (ddisp zu3)
  putStrLn (ddisp zu4)
  
  putStrLn ("Demo ezipWith - boxed")
  putStrLn (ddisp zb1)
  putStrLn (ddisp zb2)
  putStrLn (ddisp zb3)
  putStrLn (ddisp zb4)
  
  putStrLn ("Demo ezipWith val - unboxed/boxed")
  putStrLn (ddisp zau1)
  putStrLn (ddisp zau2)
  putStrLn (ddisp zav1)
  putStrLn (ddisp zav2)

  
  -- putStrLn (ddisp rv2)
  -- putStrLn (ddisp ru1)
  -- putStrLn (ddisp ru2)
  
  
  
