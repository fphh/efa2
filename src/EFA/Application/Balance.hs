-- | Import as Balance

module EFA.Application.Balance where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Environment as EqEnv

import qualified EFA.Utility.Map as MapU
import qualified Data.Map as Map ; import Data.Map (Map)


type Balance node a = Map node a


{-
cf. Graph.Flow.getStorageSequences, Equation.System.getStorageSequences
-}
relative ::
   (Ord node, Arith.Sum a) =>
   EqEnv.Complete node a v ->
   Map node (Map Idx.AugmentedSection a)
relative (EqEnv.Complete env _) =
   Map.unionWith
      (Map.unionWith (error "storage cannot be In and Out at the same time"))
      (sequences (\(Idx.StInSum sec) -> Idx.augmentSection sec) $
       EqEnv.stInSumMap env)
      (sequences (\(Idx.StOutSum sec) -> Idx.augmentSection sec) $
       fmap Arith.negate $ EqEnv.stOutSumMap env)

absolute ::
   (Ord node, Arith.Sum a) =>
   EqEnv.Complete node a v ->
   Map node (Map Idx.Boundary a)
absolute (EqEnv.Complete env _) =
   sequences (\(Idx.Storage bnd) -> bnd) $ EqEnv.storageMap env

sequences ::
   (Ord node, Ord sec) =>
   (idx node -> sec) ->
   Map (Idx.ForNode idx node) a -> Map node (Map sec a)
sequences sec =
   MapU.curry "Balance.sequences"
      (\(Idx.ForNode idx node) -> (node, sec idx))
