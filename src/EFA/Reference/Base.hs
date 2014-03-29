{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module EFA.Reference.Base where

import EFA.Utility.Filename((+++),FPath(..),DirPath(..),Abs,Rel,Directory(..),FileName(..),fromString,filename)

import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)

import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.List as List

modul :: ModuleName
modul = ModuleName "Test.Reference.Base"

nc :: FunctionName -> Caller
nc = genCaller modul

{-
tmpFolder :: DirPath Abs
tmpFolder p = p +++ DirPath [Directory "tmp"] 


genTestPath ::  Group -> Test -> DirPath Rel 
genTestPath (Test name) = DirPath fromString $ filename name
-}
newtype Ref a = Ref a

type Type = String 
type Value = String
type Name = String

data Test = Test (DirPath Rel) (Map.Map (FPath Rel) Data) deriving (Eq,Show)

data Data = DataMap Type (Map.Map String Data)
          | StringData Type String 
          | NumData Type Double deriving (Eq,Show,Read)
                                         
-- | A Class to convert a Datatype to its Reference Cousin

class ToData a where
  toData :: a -> Data
  
instance ToData Double where
  toData x = NumData "Double" x 

instance ToData [Char] where
  toData x = StringData "String" x 

instance ToData (Ratio.Ratio Integer) where
  toData x = StringData "Ratio" (show x) 
  
instance (ToData a) => ToData [a] where  
  toData xs = DataMap "List" (Map.fromList $ zip (map show [0..]) $ map toData xs)
  

checkVersusRef (Ref x)  x1 = x==x1


diffTest :: Ref (Test) -> Test -> String
diffTest (Ref (Test label map)) (Test label1 map1) = if label == label1 && map == map1 then "Passed" else "NOT PASSED"


diff :: String -> Ref Data -> Data -> String

diff label (Ref (DataMap rt rv)) (DataMap t v) = label ++": " ++ result ++ "\n"
  where result = case (rt==t,rv==v) of  
          (True,True) -> "OK"
          (False,True) -> "Types Differ - Ref:"  ++ show rt ++ "Test: " ++ show t
          (True,False) -> "Values Differ - Ref: \n " ++ diffMap (Ref (DataMap rt rv)) (DataMap t v)
          (False,False) -> "Types and Values Differ - Types: Ref:" ++ show rt ++ "Test: " ++ show t ++ 
                           diffMap (Ref (DataMap rt rv)) (DataMap rt v)
          
diff label (Ref (StringData rt rv)) (StringData t v) = label ++": " ++ result ++ "\n"
  where result = case (rt==t,rv==v) of  
          (True,True) -> "OK"
          (False,True) -> "Types Differ - Ref:"  ++ show rt ++ "Test: " ++ show t
          (True,False) -> "Values Differ - Ref: " ++ show rv ++ "Test: " ++ show v    
          (False,False) -> "Types and Values Differ - Types: Ref:" ++ show rt ++ "Test: " ++ show t ++ 
                        "Value: Ref: " ++ show rv ++ "Test: " ++ show v
    
diff label (Ref (NumData rt rv)) (NumData t v) = label ++": " ++ result ++ "\n"
  where
    result = case (rt==t,rv==v) of  
      (True,True) -> "OK"
      (False,True) -> "Types Differ - Ref:"  ++ show rt ++ "Test: " ++ show t
      (True,False) -> "Values Differ - Ref: " ++ show rv ++ "Test: " ++ show v    
      (False,False) -> "Types and Values Differ - Types: Ref:" ++ show rt ++ "Test: " ++ show t ++ 
                    "Value: Ref: " ++ show rv ++ "Test: " ++ show v

diff _ _ _ = "Ref Data Types don't match \n"
    

diffMap ::  Ref (Data) -> Data -> String
diffMap  r@(Ref (DataMap rt ref)) n@(DataMap _ new) = 
  diffCommons (Ref $ DataMap rt (Map.intersection ref new)) (DataMap rt (Map.intersection new ref)) ++
  showMissing r n ++ "\n" ++
  showNew  r n ++ "\n"
  
diffCommons :: Ref Data -> Data -> String
diffCommons (Ref (DataMap rt rv)) (DataMap t v) = 
  List.intercalate "\n" $ 
  zipWith f (Map.toList rv) (Map.elems v) 
  where f (label,x) y = diff label (Ref x) y
               
showMissing :: Ref Data -> Data -> String 
showMissing (Ref (DataMap rt rv)) (DataMap t v )  = 
  "Missing Elemets: \n" ++ (List.intercalate "\n" $ map show $ Map.toList $ Map.difference rv v)

showNew :: Ref Data -> Data -> String 
showNew (Ref(DataMap rt rv)) (DataMap t v) = 
  "New Elements: \n" ++ (List.intercalate "\n" $ map show $ Map.toList $ Map.difference v rv)

