module EFA.Hack.Stack where
  
import qualified EFA.Symbolic.OperatorTree as Op
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Equation.Variable as Var
import qualified EFA.Symbolic.SumProduct as SumProduct

import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)
import Data.Set as Set
import Data.Map as Map




{- |
Symbol equipped with a numeric value.
-}
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

{-
-- evaluate :: 
evaluate xs =  fmap f Op.group $ Op.expand $ Op.fromNormalTerm xs
  where 
    f symbol = (fmap index symbol, Op.evaluate value symbol)
-}

evaluate :: Ord node =>
            SumProduct.Term (Symbol node)
            -> Map.Map
            (Set.Set (Symbol node))
            (Op.Term (Idx.Record Idx.Delta (Var.Index node)), Double)
evaluate x =  fmap
             (\symbol ->
               (fmap index symbol,
                Op.evaluate value symbol)) $
             Op.group $ Op.expand $ Op.fromNormalTerm x
