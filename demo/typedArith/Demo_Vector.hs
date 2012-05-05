import EFA2.Signal.Vector


d1 = 1 :: Val
d2 = 2 :: Val

val1 = EVal d1
val2 = EVal d2

l1 = [1..3]
l2 = [0..2]

u1 = toEC l1 :: EUVec Val
u2 = toEC l2 :: EUVec Val

v1 = toEC l1 :: EVec Val
v2 = toEC l2 :: EVec Val

-- rv1 = emap sign u1  -- :: EVec Sign
rv2 = emap sign u1
rv1 = emap fb v1

ru1 = emap f u1 
ru2 = emap f v1

f :: Unboxed -> Val -> Val 
f _ x = x+1

fb :: Boxed -> Val -> Val 
fb _ x = x+1

fzu :: Unboxed -> Val -> Val -> Val
fzu _ x y = x + y

fzb :: Boxed -> Val -> Val -> Val
fzb _ x y = x + y

zu1 = ezipWith fzu u1 u2  
zu2 = ezipWith fzu v1 u2  
zu3 = ezipWith fzu u1 v2 
zu4 = ezipWith fzu v1 v2 

zau1 = ezipWith fzu val1 u1 
zau2 = ezipWith fzu val1 v1 

zav1 = ezipWith fzb val1 u1 
zav2 = ezipWith fzb val1 v1 



-- zu5 = emap ((flip fzu) 1) u1

zb1 = ezipWith fzb u1 u2 
zb2 = ezipWith fzb v1 u2 
zb3 = ezipWith fzb u1 v2 
zb4 = ezipWith fzb v1 v2 

-- emap ((flip f) x) y

d3 = dmult undefined d1 False :: Val

u10 = ezipWith dmult u1 u2


main = do 
  putStrLn ("Demo Data-Arith")
  putStrLn (show d3)
  
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
  
  
  
