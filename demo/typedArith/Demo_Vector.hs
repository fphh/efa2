import EFA2.Signal.Vector2
import EFA2.Signal.Base
import EFA2.Report.Vector
import EFA2.Report.Base

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- | Define Input Elements to play with
val1 = DC $ DVal 1 :: DC D0 (DVal Val)
val2 = DC $ DVal 2 :: DC D0 (DVal Val)

-- Lists to be used below
l1 = [1..3]
l2 = [0..2]

-- Unboxed Vectors
u1 = DC $  UV.fromList l1 :: DC D1 (UVec Val)
u2 = DC $  UV.fromList l2 :: DC D1 (UVec Val)

-- Boxed Vectors
v1 = DC  $ V.fromList l1 :: DC D1 (Vec Val)
v2 = DC  $ V.fromList l2 :: DC D1 (Vec Val)

-- List Vectors
vl1 = DC l1
vl2 = DC l2

-- | Testing fmap
fu :: Unboxed -> Val -> Val 
fu _ x = x

fb = sign

rv1 = dfmap fb u1  
rv2 = dfmap fb v1

ru1 = dfmap fu u1 
ru2 = dfmap fu v1


-- | Testing zipWith

fzb :: Boxed -> Val -> Val -> Val
fzb _ x y = (..*) undefined x y

fzu :: Unboxed -> Val -> Val -> Val
fzu _ x y = (..*) undefined x y

-- Val Boxed and Unboxed
zau1 = dzipWith fzu val1 u1 :: DC D1 (UVec Val) -- ## Problem 1:: dzipWith only Works with Type Signature on output
zau2 = dzipWith fzu val1 v1 :: DC D1 (Vec Val)
zav1 = dzipWith fzb val1 u1 :: DC D1 (UVec Val) -- ## Problem2 :: Context reduction stack overflow -- see Message
zav2 = dzipWith fzb val1 v1 :: DC D1 (Vec Val)

    -- Compiler Message:
    -- Context reduction stack overflow; size = 21
    -- Use -fcontext-stack=N to increase stack size to N
    --   $dSFunctor :: SFunctor Unboxed UV.Vector Vec Double Double
    --   $dSFunctor :: SFunctor Boxed UV.Vector UVec Double Double

-- Vectors boxed and unboxed
-- zu1 = dzipWith fzu u1 u2  :: DC D1 (UVec Val)
zu1 = u1 .* u2
--zu2 = dzipWith fzu v1 u2  :: DC D1 (UVec Val)
zu2 = v1 .* u1
--zu3 = dzipWith fzu u1 v2  :: DC D1 (UVec Val)
zu3 = u1 .* v2
--zu4 = dzipWith fzu v1 v2   :: DC D1 (UVec Val)
zu4 = v1.*v2

zb1 = dzipWith fzb u1 u2 :: DC D1 (Vec Val)
zb2 = dzipWith fzb v1 u2 :: DC D1 (Vec Val)
zb3 = dzipWith fzb u1 v2 :: DC D1 (Vec Val)
zb4 = dzipWith fzb v1 v2 :: DC D1 (Vec Val)

main = do 
  
  putStrLn ("Demo Data-Arith")
  putStrLn ("Demo fmap")
  putStrLn (ddisp rv1)
  putStrLn (ddisp rv2)
  putStrLn (ddisp ru1)
  
  
  putStrLn (ddisp ru2)
  

  putStrLn ("Demo dzipWith - unboxed")
  putStrLn (ddisp zu1) 
  putStrLn (ddisp zu2)
  putStrLn (ddisp zu3)
  putStrLn (ddisp zu4) 
  
  putStrLn ("Demo dzipWith - boxed")
  putStrLn (ddisp zb1)
  putStrLn (ddisp zb2)
  putStrLn (ddisp zb3)
  putStrLn (ddisp zb4)
  
  putStrLn ("Demo dzipWith Val - unboxed/boxed")
  putStrLn (ddisp zau1)
  putStrLn (ddisp zau2)
  putStrLn (ddisp zav1)
  putStrLn (ddisp zav2)

  

  
