{-# LANGUAGE FlexibleInstances #-}

-- Example for error catching, error handling, and error recovery based on
-- the error monad. 

module Main where


import Control.Monad.Error
import Control.Exception
import Debug.Trace


-- Length errors...
data LengthError a = NotEqual a
                   | TooShort Int a
                   | BehebbarerFehler a deriving (Show)


-- Necessary error instance. Will not do without.
instance Error (LengthError [a])


-- Monad for length errors.
type LengthMonad a = Either (LengthError a) a


-- Monad for checking for length errors.
type CheckLengthMonad a = Either (LengthError a) ()


-- Helper

fromRight :: (Show a, Show b) => Either a b -> b
fromRight (Right x) = x
fromRight x = error $ "Not supposed to find " ++ show x ++ " here."


-- Different checks.

checkEqualLength :: [[a]] -> CheckLengthMonad [[a]]
checkEqualLength ns = when (not equalLength) $ throwError (NotEqual ns)
  where xs = map length ns
        equalLength = and $ map (== head xs) (tail xs)

checkTooShort :: [[a]] -> CheckLengthMonad [[a]]
checkTooShort ns = when (len < 2) $ throwError (TooShort len ns)
  where len = length (head ns)

checkBehebbarerFehler :: [[a]] -> CheckLengthMonad [[a]]
checkBehebbarerFehler ns = when (len < 3) $ throwError (BehebbarerFehler ns)
  where len = length (head ns)

-- Integrate all checks. Order of checking does matter!

vmapCheck :: (Integral a) => [[a]] -> LengthMonad [[a]]
vmapCheck ns = do
  checkEqualLength ns
  checkTooShort ns
  checkBehebbarerFehler ns
  return ns

-- Handle errors. Either call error or repair data.

handleError :: (Num a) => LengthError [[a]] -> LengthMonad [[a]]
handleError (NotEqual ns) = error $ "List length is not equal: " ++ show ns
handleError (TooShort len ns) = error $ "List of length " ++ show len ++ " is too short: " ++ show ns
handleError (BehebbarerFehler ns) = trace "repairing..." $ return (map (1000:) ns)

-- Check for errors, catch them, and if none, do algorithm.

vmapAlgo :: (Show a, Integral a) => (a -> a) -> [[a]] -> LengthMonad [[a]]
vmapAlgo f as = do
  as' <- vmapCheck as `catchError` handleError
  return (map (map f) as')

-- Wrap it nicely.

vmap :: (Show a, Integral a) => (a -> a) -> [[a]] -> [[a]]
vmap f xs = fromRight $ vmapAlgo f xs

-- Tests:

main :: IO ()
main = do
  -- Ok
  print (vmap (+1) [[1, 2, 3], [3, 4, 3]])

  -- NotEqual
  --print (vmap (+1) [[0, 1], [1, 2, 3]])

  -- TooShort
  --print (vmap (+1) [[1], [2]])

  -- BehebbarerFehler
  print (vmap (+1) [[20, 30], [30, 40]])
