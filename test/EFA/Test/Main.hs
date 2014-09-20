module Main where

import qualified EFA.Data.Axis.Strict as Strict 
import qualified EFA.Data.Vector as DV 
import qualified EFA.Data.OD.Curve as Curve 
import qualified EFA.Data.Interpolation as Interp

import qualified EFA.Value.Type as Type 
import qualified EFA.Action.EtaFunctions as EtaFunctions
import qualified EFA.Equation.Arithmetic as Arith

import EFA.Utility(Caller,
--                   merror,
                   -- (|>),
                   ModuleName(..),FunctionName, genCaller)


import Debug.Trace(trace)

modul::ModuleName
modul=ModuleName "EFA.Test.EtaFunctions"

nc :: FunctionName -> Caller
nc = genCaller modul


data Base

axis :: Strict.Axis Base String [] Double 
axis = Strict.Axis "MyCurve" Type.P $ DV.fromList [0.1,1]  

curve = Curve.Curve axis $ DV.fromList [0.11,1]

dwnCurve = EtaFunctions.reverseCurve EtaFunctions.DownStream curve 
upCurve = EtaFunctions.reverseCurve EtaFunctions.UpStream curve 

curveWithMethods,curveWithMethods2 :: ((Interp.Method Double,Interp.ExtrapMethod Double),Curve.Curve Base String [] Double Double)
curveWithMethods = ((Interp.Linear,Interp.ExtrapLinear), curve)
curveWithMethods2 = ((Interp.Linear,Interp.ExtrapNone), curve)

caller =  (nc "etaFunct") 

etaFunct, etaFunctDwn, etaFunctUp:: Interp.Val Double -> Interp.Val Double
etaFunct = EtaFunctions.etaFunctionWithOneCurve (nc "etaFunct") curveWithMethods 
etaFunctDwn = EtaFunctions.etaFunctionWithOneCurve (nc "etaFunct") (EtaFunctions.rev EtaFunctions.DownStream curveWithMethods)     
etaFunctUp = EtaFunctions.etaFunctionWithOneCurve (nc "etaFunct") (EtaFunctions.rev EtaFunctions.UpStream curveWithMethods)     

etaFunctIn = EtaFunctions.etaFunctionWithOneCurveAlt (nc "in") EtaFunctions.PowerIn curveWithMethods2
etaFunctInOpp = EtaFunctions.etaFunctionWithOneCurveAltOpp (nc "inOp") EtaFunctions.PowerIn "Reverse" curveWithMethods2

etaFunctOut = EtaFunctions.etaFunctionWithOneCurveAlt (nc "out") EtaFunctions.PowerOut curveWithMethods2
etaFunctOutOpp = EtaFunctions.etaFunctionWithOneCurveAltOpp (nc "outOp") EtaFunctions.PowerOut "Reverse" curveWithMethods2

prop_ReversableDwn :: Interp.Val Double -> Bool
prop_ReversableDwn pin = trace str $ Arith.abs (pinRev Arith.~- pin) < Interp.Inter (Arith.fromRational (10^^(-6::Integer)))
  where
    str = "Pin: " ++ show pin ++ " | n: " ++ show n ++ " | pout: " ++ show pout ++ " | nrev: " ++ show nrev ++ 
          " | pinRev: " ++ show pinRev ++ "| diff: " ++ show diff 
    n = etaFunct pin
    pout = pin Arith.~* n
    nrev = etaFunctDwn pout
    pinRev = pout Arith.~/ nrev
    diff = pinRev Arith.~- pin

prop_ReversableDwn2 :: Interp.Val Double -> Bool
prop_ReversableDwn2 pin = trace str $ Arith.abs (pinRev Arith.~- pin) < Interp.Inter (Arith.fromRational (10^^(-6::Integer)))
  where
    str = "Pin: " ++ show pin ++ " | n: " ++ show n ++ " | pout: " ++ show pout ++ " | nrev: " ++ show nrev ++ 
          " | pinRev: " ++ show pinRev ++ "| diff: " ++ show diff 
    n = etaFunctIn pin
    pout = pin Arith.~* n
    nrev = etaFunctInOpp pout
    pinRev = pout Arith.~/ nrev
    diff = pinRev Arith.~- pin
    
prop_ReversableUp :: Interp.Val Double -> Bool
prop_ReversableUp pout = trace str $ Arith.abs (poutRev Arith.~- pout) < Interp.Inter (Arith.fromRational (10^^(-16::Integer)))
  where
    str = "Pout: " ++ show pout ++ " | n: " ++ show n ++ " | pin: " ++ show pin ++ " | nrev: " ++ show nrev ++ 
          " | poutRev: " ++ show poutRev ++ " | diff: " ++ show diff 
    n = etaFunct pout
    pin = pout Arith.~/ n
    nrev = etaFunctUp pin
    poutRev = pin Arith.~* nrev
    diff = poutRev Arith.~- pout
    
prop_ReversableUp2 :: Interp.Val Double -> Bool
prop_ReversableUp2 pout = trace str $ Arith.abs (poutRev Arith.~- pout) < Interp.Inter (Arith.fromRational (10^^(-16::Integer)))
  where
    str = "Pout: " ++ show pout ++ " | n: " ++ show n ++ " | pin: " ++ show pin ++ " | nrev: " ++ show nrev ++ 
          " | poutRev: " ++ show poutRev ++ " | diff: " ++ show diff 
    n = etaFunctOut pout
    pin = pout Arith.~/ n
    nrev = etaFunctOutOpp pin
    poutRev = pin Arith.~* nrev
    diff = poutRev Arith.~- pout

main :: IO ()
main = do 
  
  print dwnCurve
  
  print "DOWN ############"
--  mapM_ (\x -> print $ prop_ReversableDwn $ Interp.Inter x) [0.001,0.01 ..1.1]
  mapM_ (\x -> print $ prop_ReversableDwn2 $ Interp.Inter x) [0.1,0.2 .. 1]   --[0.001,0.01 ..1.1]
  
  print upCurve 
  print "Up ############"
--  mapM_ (\x -> print $ prop_ReversableUp $ Interp.Inter x) [0.001,0.01 ..1.1]
  mapM_ (\x -> print $ prop_ReversableUp2 $ Interp.Inter x) [0.1,0.2 .. 1] -- [0.001,0.01 ..1.1]
