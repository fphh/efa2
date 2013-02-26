module Main where

import EFA.Example.Utility
          (edgeVar, makeEdges, constructSeqTopo, (.=))

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Symbolic.OperatorTree as Op
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Env as Env

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Monoid (mempty, (<>))
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)


sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1, node2 :: Node.Int
node0 :~ node1 :~ node2 :~ _ = Stream.enumFrom minBound


topoLinear :: TD.Topology Node.Int
topoLinear = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Crossing),
              (node2, TD.Sink)]
        es = [(node0, node1), (node1, node2)]


{- |
Symbol equipped with a numeric value.
-}
data
   Symbol =
      Symbol {
         index :: Idx.Record Idx.Delta (Var.Index Node.Int),
         value :: Double
      }

instance Eq Symbol where
   (==)  =  equating index

instance Ord Symbol where
   compare  =  comparing index


infixr 6 =<>

(=<>) ::
   (Eq (term Symbol), Num (term Symbol), Ord (t Node.Int),
    Var.MkVarC term, Var.MkIdxC t, Env.AccessMap t) =>
   (Idx.Record Idx.Delta (t Node.Int), Double) ->
   EqGen.EquationSystem Env.Delta Node.Int s (term Symbol) ->
   EqGen.EquationSystem Env.Delta Node.Int s (term Symbol)
(idx, x) =<> eqsys =
   (idx .= Var.mkVarCore (Symbol (fmap Var.mkIdx idx) x)) <> eqsys


{-
Use SumProduct.Term here since it simplifies automatically.
-}
given ::
   EqGen.EquationSystem Env.Delta Node.Int s
      (SumProduct.Term Symbol)
given =
   (Idx.delta (Idx.DTime Idx.initSection) .= 0) <>
   (Idx.delta (Idx.DTime sec0) .= 0) <>

   (Idx.before (Idx.DTime Idx.initSection) .= 1) <>
   (Idx.before (Idx.DTime sec0) .= 1) <>

   (Idx.before (edgeVar Idx.Energy sec0 node0 node1), 4) =<>
   (Idx.before (edgeVar Idx.Eta sec0 node0 node1), 0.25) =<>
   (Idx.before (edgeVar Idx.Eta sec0 node1 node2), 0.85) =<>

   (Idx.delta (edgeVar Idx.Energy sec0 node0 node1), -0.6) =<>
   (Idx.delta (edgeVar Idx.Eta sec0 node0 node1), 0.1) =<>
   (Idx.delta (edgeVar Idx.Eta sec0 node1 node2), 0.05) =<>

   mempty


main :: IO ()
main = do

   let seqTopo = constructSeqTopo topoLinear [0]
       env = EqGen.solve given seqTopo

   case Map.lookup (edgeVar Idx.Energy sec0 node2 node1) (Env.energyMap env) of
      Nothing -> error "undefined E_2_1"
      Just d ->
         case Env.after d of
            Result.Undetermined -> error "undetermined E_2_1"
            Result.Determined x ->
               Fold.forM_ (Op.group $ Op.expand $ Op.fromNormalTerm x) $ \symbol -> do
                  putStrLn $
                     (Format.unUnicode $ formatValue $ fmap index symbol)
                     ++ " = " ++
                     show (Op.evaluate value symbol)

   Draw.sequFlowGraphDeltaWithEnv seqTopo $
      fmap (fmap (fmap (SumProduct.map index))) env
