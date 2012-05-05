import EFA2.Signal.Vector


s1 = toEC [0..1] :: EUVec Val
s2 = toEC [0..3] :: EUVec Val
v1 = toEC [0..2] :: EVec Val
v2 = toEC [0..1] :: EVec Val

s3 = emap sign s1 :: EVec Sign
s4 = ezipWith (.*.) v1 v2 :: EVec Val

v5 = v1 .* v2 :: EVec Val

data Unbox

f :: (DMult d1 d2 d3) => Unbox -> d1 -> d2 -> d3
f _ x y = x.*.y

x1 = 1 :: Val
x2 = 2 :: Val 
z = f undefined x1 x2  


main = do 
  putStrLn (show s2)
  putStrLn (show s4)
  putStrLn (show s3)
  putStrLn (show z) 