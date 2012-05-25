{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module EFA2.Interpreter.InTerm where

-- import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env
import EFA2.Signal.Base


data InTerm a = EIdx EnergyIdx
              | DEIdx DEnergyIdx
              | PIdx PowerIdx
              | DPIdx DPowerIdx
              -- | NIdx EtaIdx
              | FNIdx FEtaIdx -- (InTerm a)
              | DNIdx DEtaIdx
              | ScaleIdx XIdx
              | DTIdx DTimeIdx
              | VIdx VarIdx
              | SIdx StorageIdx
              -- | InConst a
              | InConst Val
              | InGiven a
              | InFunc (a -> a)
              | InMinus (InTerm a)
              | InRecip (InTerm a)
              | InAdd (InTerm a) (InTerm a)
              | InMult (InTerm a) (InTerm a)
              | InEqual (InTerm a) (InTerm a) deriving (Eq, Ord, Show)


instance (Show a, Eq a) => Num (InTerm a) where
         (+) = InAdd
         (*) = InMult
         abs = undefined
         signum = undefined
         fromInteger = undefined