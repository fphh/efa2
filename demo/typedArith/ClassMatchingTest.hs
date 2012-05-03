import Data.Typeable

data List a = List [a]
data Scal a = Scal a

class DimOne a
instance DimOne (List a)

class  DimZero a
instance DimZero (Scal a)

{-
  
class Print a where  
  print :: a -> IO ()

instance (MatchingDim a flag, Print' flag a) => Print a
  print = print' (undefined :: flag)
  
class Print' flag a where
  print' :: flag -> a -> IO ()

class MatchingDim a flag | a -> flag where {}


instance (OneDim a) -> MatchingDim a flag
instance (OneDim a) -> MatchingDim a flag


- data TypeCast flag DimFalse

-}
data DimFalse
data Dim0
data Dim1
data Dim2
data Dim3




classString x | typeOf x `instanceOf` DimOne = "DimOne"  
              | typeOf x `instanceOf` DimTwo = "DimTwo"
l = []                                               

main = do                                                
  putStrLn (classString l)