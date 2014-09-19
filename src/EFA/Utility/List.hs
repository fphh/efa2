{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Utility.List where

import EFA.Utility(Caller,
                   merror, -- (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Utility.List"

nc :: FunctionName -> Caller
nc = genCaller modul


vhead :: String -> [a] -> a
vhead _ (x:_) = x
vhead caller _ = error $ "vhead, " ++ caller ++ ": empty list"


vlast :: String -> [a] -> a
vlast _ xs@(_:_) = last xs
vlast caller _ = error $ "vlast, " ++ caller ++ ": empty list"


-- | Example
--
-- >>> takeUntil (>5) [1..10]
-- [1,2,3,4,5,6]
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = go
  where go (x:xs) =  x : if not (p x) then go xs else []
        go [] = []

errTail :: Caller -> [a] -> [a]
errTail caller xs = if length xs >=1 then tail xs else e
  where e = merror caller modul "errTail" "empty List"
        
errHead :: Caller -> [a] -> a
errHead caller xs = if length xs >=1 then head xs else e
  where e = merror caller modul "errHead" "empty List"        