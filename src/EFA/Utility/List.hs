

module EFA.Utility.List where


vhead :: String -> [a] -> a
vhead _ (x:_) = x
vhead caller _ = error $ "vhead, " ++ caller ++ ": empty list"


vlast :: String -> [a] -> a
vlast caller xs =
  if null xs
     then error $ "vlast, " ++ caller ++ ": empty list"
     else last xs