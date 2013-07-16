
module EFA.TestUtility where

import System.Exit (exitFailure)
import Control.Monad (when)


singleIO :: String -> IO Bool -> IO ()
singleIO msg act = do
   putStrLn msg
   success <- act
   when (not success) exitFailure

single :: String -> Bool -> IO ()
single msg success = do
   putStrLn msg
   when (not success) exitFailure
