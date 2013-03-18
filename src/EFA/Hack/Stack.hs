module EFA.Hack.Stack where
  
import qualified EFA.Symbolic.OperatorTree as Op
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Equation.Variable as Var
import qualified EFA.Symbolic.SumProduct as SumProduct

import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)
import Data.Set as Set
import Data.Map as Map

import qualified EFA.Symbolic.Mixed as Term


{- |
Symbol equipped with a numeric value.
-}

{-
data
   Symbol node =
      Symbol {
         index :: Idx.Record Idx.Delta (Var.Index node),
         value :: Double
      } deriving Show

instance (Eq node) =>  Eq (Symbol node) where
   (==)  =  equating index

instance (Ord node) => Ord (Symbol node) where
   compare  =  comparing index

-}

data
   Symbol idx =
      Symbol {
         index :: Idx.Record Idx.Delta idx,
         value :: Double
      }

type ScalarSymbol idx = Symbol (Var.Scalar idx)
type SignalSymbol idx = Symbol (Var.Signal idx)

instance (Eq idx) => Eq (Symbol idx) where
   (==)  =  equating index

instance (Ord idx) => Ord (Symbol idx) where
   compare  =  comparing index

evaluate :: Ord idx =>
                           Term.Signal SumProduct.Term scalar (Symbol idx)
                           -> Map
                                (Set (Symbol idx)) (Op.Term (Idx.Record Idx.Delta idx), Double)
evaluate x =  fmap
             (\symbol ->
               (fmap index symbol,
                Op.evaluate value symbol)) $
             Op.group $ Op.expand $ Op.fromNormalTerm $ Term.getSignal x
