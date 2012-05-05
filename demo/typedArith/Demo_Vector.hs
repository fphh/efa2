import EFA2.Signal.Vector

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

val1 = DC $ DVal 1 :: DC H (DVal Val)
val2 = DC $ DVal 2 :: DC H (DVal Val)

l1 = [1..3]
l2 = [0..2]

u1 = DC $ UVec $ UV.fromList l1 :: DC H (UVec Val)
u2 = DC $ UVec $ UV.fromList l2 :: DC H (UVec Val)

v1 = DC  $ Vec $ V.fromList l1 :: DC H (Vec Val)
v2 = DC  $ Vec $ V.fromList l2 :: DC H (Vec Val)

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
--  putStrLn (show d3)
  
  putStrLn ("Demo emap")
  putStrLn (show rv1)
  putStrLn (show rv2)
  putStrLn (show ru1)
  putStrLn (show ru2)
  
  putStrLn ("Demo ezipWith - unboxed")
  putStrLn (show zu1)
  putStrLn (show zu2)
  putStrLn (show zu3)
  putStrLn (show zu4)
  
  putStrLn ("Demo ezipWith - boxed")
  putStrLn (show zb1)
  putStrLn (show zb2)
  putStrLn (show zb3)
  putStrLn (show zb4)
  
  putStrLn ("Demo ezipWith val - unboxed/boxed")
  putStrLn (show zau1)
  putStrLn (show zau2)
  putStrLn (show zav1)
  putStrLn (show zav2)

  
  -- putStrLn (show rv2)
  -- putStrLn (show ru1)
  -- putStrLn (show ru2)
  
  
  
