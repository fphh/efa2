module EFA.Utility.Trace where

import qualified EFA.Utility as Ut

import Debug.Trace (trace)


-- | A debugging function for displaying arguments in pointless style.
showarg :: (Show a) => a -> a
showarg x = trace (show x) x



debugLevel :: Integer
debugLevel = 1


simTrace :: Show a => String -> a -> a
simTrace msg var = trace ("simTrace: " ++ show msg ++ " : " ++ show var) var
  
nTrace :: Show a => Bool -> Ut.ModuleName -> Ut.FunctionName -> String -> a -> a
nTrace isActive modu funct varName val = 
  if isActive 
  then trace ("nTrace - " ++ show modu ++ " - Funct: " ++ show funct ++ "- Var: " ++ show varName ++ "- Value: " ++ show val) val
  else val     

-- mytrace for single values
mytrace ::
   (Show a, Show b, Show c) =>
   Integer -> b -> c -> a -> a
mytrace dbgLevel function varName var =
   if dbgLevel >= debugLevel
     then trace ("myTrace: " ++ show function ++ "-" ++ show varName ++ " : " ++ show var) var
     else var

-- mytrace for lists
mytraceList ::
   (Show a, Show b, Show c) =>
   Integer -> b -> c -> [a] -> [a]
mytraceList dbgLevel function varName var =
   if dbgLevel >= debugLevel
     then trace ("myTraceList: " ++ show function ++ "-" ++ show varName ++ " : " ++ Ut.myShowList var) var
     else var
