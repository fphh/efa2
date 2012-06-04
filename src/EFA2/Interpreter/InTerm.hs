{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeOperators #-}


module EFA2.Interpreter.InTerm where

-- import EFA2.Interpreter.Arith
import EFA2.Interpreter.Env
import EFA2.Signal.Base
import EFA2.Signal.Signal
import EFA2.Signal.Data



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
         
         
instance BProd (InTerm a) (InTerm a) (InTerm a) where
         (..*) = InMult
         x ../ y = InMult x (InRecip y)

instance BSum (InTerm a) (InTerm a) (InTerm a) where
         (..+) = InAdd
         x ..- y = InAdd x (InMinus y)
 
instance DArith0 (InTerm a) where
         neg = InMinus
         rec = InRecip

instance SConst Scalar (Data Nil) (InTerm a) where   
         toConst _ x = undefined --toScalar x 
