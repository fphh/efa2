module EFA2.Signal.Index where


newtype Section = Section Int deriving (Show, Eq, Ord)

instance Enum Section where
   fromEnum (Section n) = n
   toEnum n = Section n


newtype Record = Record Int deriving (Show, Eq, Ord)
