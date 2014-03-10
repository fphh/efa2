{-# LANGUAGE FlexibleInstances #-}

module EFA.TestUtility where

import qualified Test.QuickCheck as QC

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



newtype Func a = Func { unFunc :: (a -> a) }

instance Show (Func a) where
  show _ = "Func <func>"

instance (QC.Arbitrary a, QC.CoArbitrary a) => QC.Arbitrary (Func a) where
  arbitrary = fmap Func QC.arbitrary    
