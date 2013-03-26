module Main where

import qualified Data.Map as M
import Data.Monoid (mconcat, mempty, Monoid, (<>))
import Control.Applicative (liftA2)

import EFA.Example.Utility ( edgeVar, makeEdges, constructSeqTopo )
import EFA.Example.Absolute ( (.=) )

import qualified EFA.Example.Absolute as EqGen
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Report.Format as Format

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as Rec
import EFA.Equation.Result (Result(..))


import Debug.Trace

sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

node0, node1, node2, node3 :: Node
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom $ Node 0

newtype Node = Node Int deriving (Show, Eq, Ord)

instance Enum Node where
         toEnum = Node
         fromEnum (Node n) = n

instance Node.C Node where
   display (Node 0) = Format.literal "null"
   display (Node 1) = Format.literal "eins"
   display (Node 2) = Format.literal "zwei"
   display (Node 3) = Format.literal "drei"
   display n = Format.literal $ show n

   subscript (Node n) = Format.literal $ show n
   dotId = Node.dotIdDefault


topoDreibein :: TD.Topology Node
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Sink),
              (node2, TD.Crossing),
              (node3, TD.Storage)]
        es = [(node0, node2), (node2, node1), (node2, node3)]

given :: EqGen.EquationSystem Node s Double Double
given =
   mconcat $

   (Idx.DTime sec0 .= 0.5) :
   (Idx.DTime sec1 .= 2) :
   (Idx.DTime sec2 .= 1) :

   (Idx.Storage (Idx.afterSecNode sec2 node3) .= 10.0) :


   (edgeVar Idx.Power sec0 node2 node3 .= 4.0) :

   (edgeVar Idx.X sec0 node2 node3 .= 0.32) :

   (edgeVar Idx.Power sec1 node3 node2 .= 5) :
   (edgeVar Idx.Power sec2 node3 node2 .= 6) :
   (edgeVar Idx.Power sec3 node3 node2 .= 7) :
   (edgeVar Idx.Power sec4 node3 node2 .= 8) :

   (edgeVar Idx.Eta sec0 node3 node2 .= 0.25) :
   (edgeVar Idx.Eta sec0 node2 node1 .= 0.5) :
   (edgeVar Idx.Eta sec0 node0 node2 .= 0.75) :

   (edgeVar Idx.Eta sec1 node3 node2 .= 0.25) :
   (edgeVar Idx.Eta sec1 node2 node1 .= 0.5) :
   (edgeVar Idx.Eta sec1 node0 node2 .= 0.75) :
   (edgeVar Idx.Power sec1 node1 node2 .= 4.0) :


   (edgeVar Idx.Eta sec2 node3 node2 .= 0.75) :
   (edgeVar Idx.Eta sec2 node2 node1 .= 0.5) :
   (edgeVar Idx.Eta sec2 node0 node2 .= 0.75) :
   (edgeVar Idx.Power sec2 node1 node2 .= 4.0) :


   (edgeVar Idx.Eta sec1 node2 node3 .= 0.25) :

   []


filt :: 
  (Ord node, Eq node, Show node) =>
  Gr.Graph (Idx.BndNode node) nl el -> Gr.Graph (Idx.BndNode node) nl el
filt = Gr.lefilter (TD.isStructureEdge . fst)


data RelativeDir = WithTopoDir | AgainstTopoDir deriving (Eq, Show)

getRelativeDir ::
  (Ord x) =>
  TD.Topology x -> Gr.Edge x -> RelativeDir
getRelativeDir (Gr.Graph _ es) e =
  if M.member e es
     then WithTopoDir
     else if M.member (Gr.reverseEdge e) es
             then AgainstTopoDir
             else error "getTopologyDir: edge not found"


-- Are edges in SequFlowGraph always Dir?
cumulatedEnergyFlow ::
  (Num a, Ord node, Show node) =>
  TD.Topology node ->
  Gr.Graph (Idx.BndNode node) t TD.FlowDirection ->
  Env.Complete node b (Rec.Absolute (Result a)) ->
  ( M.Map (Idx.Energy node) (Rec.Absolute (Result a)),
    M.Map (Idx.Energy node) (Rec.Absolute (Result a)) )
cumulatedEnergyFlow topo seqTopo env =
  (M.mapKeys toEn $ M.map toDet m1, M.mapKeys toEn $ M.map toDet m2)
  where em = Env.energyMap $ Env.signal env
        Gr.Graph _ es = filt seqTopo
        (m1, m2) = M.foldWithKey f (M.empty, M.empty) es
        f (Gr.Edge (Idx.BndNode (Idx.AfterSection sec) n)
                   (Idx.BndNode _ n')) TD.Dir (withDir, againstDir) =
          case getRelativeDir topo e of
               WithTopoDir -> (insert withDir, insertzero againstDir)
               AgainstTopoDir -> (insertzero withDir, insert againstDir)
          where e  = Gr.Edge n n'

                e1 = Idx.Energy (Idx.StructureEdge sec n n')
                idx1 = (n, n')

                e2 = Idx.Energy (Idx.StructureEdge sec n' n)
                idx2 = (n', n)

                insert = M.insertWith (.+) idx1 (M.lookup e1 em)
                         . M.insertWith (.+) idx2 (M.lookup e2 em)

                insertzero = M.insertWith (.+) idx1 zero
                             . M.insertWith (.+) idx2 zero
                zero = Just (Rec.Absolute (Determined 0))

                (.+) = liftA2 (liftA2 (liftA2 (+)))

        f _ _ _ = error "not a Dir edge!"

        toDet Nothing = Rec.Absolute Undetermined
        toDet (Just a) = a

        toEn (a, b) =
          Idx.Energy (Idx.StructureEdge (Idx.Section 0) a b)

reverseGraph :: (Ord a) => Gr.Graph a b c -> Gr.Graph a b c
reverseGraph (Gr.Graph ns es) = Gr.Graph ns' es'
  where ns' = fmap (\(ins, n, outs) -> (outs, n, ins)) ns
        es' = M.mapKeys (\(Gr.Edge x y) -> (Gr.Edge y x)) es


main :: IO ()
main = do

  let seqTopo = constructSeqTopo topoDreibein [1, 0, 1]
      env = EqGen.solve seqTopo given
      (withDir, againstDir) = cumulatedEnergyFlow topoDreibein seqTopo env


      withDirEnv1, withDirEnv2 ::
                   Env.Complete
                      Node 
                      (Rec.Absolute (Result Double))
                      (Rec.Absolute (Result Double))

      withDirEnv1 = mempty {
                      Env.signal = mempty { 
                        Env.energyMap = withDir,
                        Env.dtimeMap =
                          M.fromList [ (Idx.DTime (Idx.Section 0),
                                        Rec.Absolute Undetermined) ] }}
      withDirEnv2 = mempty {
                      Env.scalar = mempty {
                        Env.storageMap =
                          M.fromList [ (Idx.Storage (Idx.BndNode (Idx.AfterSection (Idx.Section 0)) node3), Rec.Absolute Undetermined)] }}

      withDirEnv = withDirEnv1 <> withDirEnv2


      againstDirEnv1, againstDirEnv2 ::
                   Env.Complete
                      Node 
                      (Rec.Absolute (Result Double))
                      (Rec.Absolute (Result Double))

      againstDirEnv1 = mempty {
                      Env.signal = mempty { 
                        Env.energyMap = againstDir,
                        Env.dtimeMap =
                          M.fromList [ (Idx.DTime (Idx.Section 0),
                                        Rec.Absolute Undetermined) ] }}
      againstDirEnv2 = mempty {
                      Env.scalar = mempty {
                        Env.storageMap =
                          M.fromList [ (Idx.Storage (Idx.BndNode (Idx.AfterSection (Idx.Section 0)) node3), Rec.Absolute Undetermined)] }}

      againstDirEnv = againstDirEnv1 <> againstDirEnv2


      revTopoDreibein = reverseGraph topoDreibein

  concurrentlyMany_ [
    Draw.sequFlowGraphAbsWithEnv "" seqTopo env,
    Draw.sequFlowGraphCumulated "" (TD.fromTopology topoDreibein) withDirEnv,
    Draw.sequFlowGraphCumulated "" (TD.fromTopology revTopoDreibein) againstDirEnv ]
