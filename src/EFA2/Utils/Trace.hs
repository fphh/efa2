module EFA2.Utils.Trace where

import EFA2.Utils.Utils (myShowList)

import Debug.Trace (trace)


-- | A debugging function for displaying arguments in pointless style.
showarg :: (Show a) => a -> a
showarg x = trace (show x) x



debugLevel :: Integer
debugLevel = 0

-- mytrace for single values
mytrace ::
   (Show a, Show b, Show c) =>
   Integer -> b -> c -> a -> a
mytrace dbgLevel function varName var =
   if debugLevel >= dbgLevel
     then trace ("myTrace: " ++ show function ++ "-" ++ show varName ++ " : " ++ show var) var
     else var

-- mytrace for lists
mytraceList ::
   (Show a, Show b, Show c) =>
   Integer -> b -> c -> [a] -> [a]
mytraceList dbgLevel function varName var =
   if debugLevel >= dbgLevel
     then trace ("myTraceList: " ++ show function ++ "-" ++ show varName ++ " : " ++ myShowList var) var
     else var
