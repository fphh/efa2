{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}


module Main where

import qualified EFA.Example.Topology.TripodB as Tripod
import EFA.Example.Topology.TripodB (Node, source, crossing, sink, storage, node0, node1, node2, node3)

import qualified EFA.Application.Plot as PlotIO
import EFA.Application.Utility (identifyFlowState, dirEdge, undirEdge)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Sequence as SeqFlowPlain
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.SystemEta (detEtaSys)
import EFA.Flow.Sequence.Absolute ((.=), (=%%=), (=.=))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph; import EFA.Graph (Graph)
import EFA.Graph.Topology (FlowTopology)

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.Signal.Data as D
import EFA.Signal.Typ (Typ,A,P,Tt)
import EFA.Signal.Data (Data, Nil, (:>))

import qualified EFA.Equation.Record as EqRec
import qualified EFA.Equation.Variable as Var
import EFA.Equation.Result (Result(Determined))

import qualified EFA.Report.Format as Format

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Async (concurrentlyMany_)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

import qualified Data.NonEmpty.Mixed as NonEmptyMixed
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Eq.HT (equating)
import Data.Ord (comparing)
import Data.Monoid (mconcat, (<>))
import Data.Foldable (foldMap)
import Data.Tuple.HT (swap)

import Debug.Trace (trace)


sec0, sec1 :: Idx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ Idx.Section 0


assembleFlowTopos :: [FlowTopology Node] -> SeqFlow.Graph Node (Result a) (Result v)
assembleFlowTopos =
   SeqFlow.graphFromPlain .
   SeqFlowPlain.sequenceGraph .
   Sequ.fromList

state0, state3 :: FlowTopology Node
state0 = identifyFlowState Tripod.topology [dirEdge node1 node2, dirEdge node1 node3]
state3 = identifyFlowState Tripod.topology [dirEdge node1 node2, undirEdge node0 node1]

flowGraph :: SeqFlow.Graph Node (Result a) (Result v)
flowGraph = assembleFlowTopos [state0, state3]


lookupStCrDown :: Double -> Double
lookupStCrDown = Sig.fromSample . Sig.interp1Lin "lookupStCrDown" xs ys . Sig.toSample
  where xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 60]
        ys = Sig.fromList [0.60, 0.605 .. 1]

lookupStCrUp :: Double -> Double
lookupStCrUp = Sig.fromSample . Sig.interp1Lin "lookupStCrUp" xs ys . Sig.toSample
  where xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 60]
        ys = Sig.fromList [0.40, 0.41 .. 1]


lookupCrSi :: Double -> Double
lookupCrSi = Sig.fromSample . Sig.interp1Lin "lookupCrSi" xs ys . Sig.toSample
  where xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.9]


lookupSoCr :: Double -> Double
lookupSoCr = Sig.fromSample . Sig.interp1Lin "lookupSoCr" xs ys . Sig.toSample
  where xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 60]
        ys = Sig.fromList [0.40, 0.41 .. 1]

commonEnv :: EqSys.EquationSystemIgnore Node s Double Double
commonEnv =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (XIdx.dTime sec1 .= 1) :
   (XIdx.storage Idx.initial storage .= 0) :
   (XIdx.power sec0 storage crossing =%%= XIdx.power sec1 storage crossing) :
   []

givenSec0Mean ::
  Double -> Double -> EqSys.EquationSystemIgnore Node s Double Double
givenSec0Mean psink _ =
   (commonEnv <>) $
   mconcat $

   (XIdx.x sec0 crossing storage .= 0.2) :
   (XIdx.power sec1 sink crossing .= psink) :


   (XIdx.eta sec0 crossing sink .= 0.5) :

   (XIdx.eta sec0 source crossing .= 0.7) :

   (XIdx.eta sec0 crossing storage .= 0.6) :

   ((EqSys.variable $ XIdx.eta sec1 storage crossing) =.=
     EqSys.liftF lookupStCrUp (EqSys.variable $ XIdx.power sec1 crossing storage)) :

   ((EqSys.variable $ XIdx.eta sec1 crossing sink) =.=
     EqSys.liftF lookupCrSi (EqSys.variable $ XIdx.power sec1 sink crossing)) :

   []

givenSec1Mean ::
  Double -> Double -> EqSys.EquationSystemIgnore Node s Double Double
givenSec1Mean psink _ =
   (commonEnv <>) $
   mconcat $


   (XIdx.power sec0 sink crossing .= psink) :

   (XIdx.power sec1 sink crossing .= 2) :

   (XIdx.eta sec1 crossing sink .= 0.9) :

   (XIdx.eta sec1 storage crossing .= 0.85) :


   ((EqSys.variable $ XIdx.eta sec0 crossing storage) =.=
     EqSys.liftF lookupStCrDown (EqSys.variable $ XIdx.power sec0 storage crossing)) :

   ((EqSys.variable $ XIdx.eta sec0 crossing sink) =.=
     EqSys.liftF lookupCrSi (EqSys.variable $ XIdx.power sec0 sink crossing)) :

   ((EqSys.variable $ XIdx.eta sec0 source crossing) =.=
     EqSys.liftF lookupSoCr (EqSys.variable $ XIdx.power sec0 crossing source)) :

   []



etaSys2 ::
  (Ord (edge k), Ord k, Show k, Show (edge k), Show t2, Graph.Edge edge) =>
  (t1, Graph k edge (Node.Type t2) edgeLabel) -> t -> a
etaSys2 (_, topo) _ = trace (show sinks) undefined
  where sinks = Map.filter isSink $ Graph.nodeEdges topo
        isSink (_, el, x) =
          case el of
               Node.AlwaysSource -> Set.size x > 0
               Node.Source -> Set.size x > 0
               _ -> False

sinkRange :: [Double]
sinkRange = [0.1] -- , 0.2 .. 20]

ratioRange :: [Double]
ratioRange = [0.1] -- , 0.2 .. 0.9]

varX', varY' :: [[Double]]
(varX', varY') = CT.varMat sinkRange ratioRange

hypotheticalUsage :: Sig.PSignal [] Double
hypotheticalUsage = Sig.fromList [
  3, 2, 6, 7,
  7, 8, 8, 9, 6, 8, 5,
  3, 2, 3, 4,
  5, 5,
  4, 6, 8, 9, 10, 9,
  6, 5, 7, 8, 8,
  2, 3 ]

borderFunc ::
  (Eq b, Ord a, Show b, Show a, D.FromList c1, D.FromList c,
  D.Storage c1 d1, D.Storage c d, D.NestedList c1 d1 ~ [a],
  D.NestedList c d ~ [b]) =>
  Sig.TC s t (Data c d) -> Sig.TC s1 t1 (Data c1 d1) -> a -> b
borderFunc ss xs p =
  case dropWhile ((< p) . fst) zs of
       (_, s):_ -> s
       _ -> error $ "Power " ++ show p ++ " out of range " ++ show zs
  where zs =
           List.sortBy (comparing fst) $
           map (swap . last) $ List.groupBy (equating fst) $
           zip (Sig.toList ss) (Sig.toList xs)

sectionHU ::
  (Eq topo) =>
  (d -> topo) ->
  Sig.PSignal [] d ->
  [(topo, Sig.PSignal [] d)]
sectionHU bf =
  map (\(NonEmpty.Cons (s, w) xs) -> (s, Sig.fromList (w : map snd xs))) .
  NonEmptyMixed.groupBy (equating fst) . map (\s -> (bf s, s)) . Sig.toList


commonEnvHU ::
  [Idx.Section] ->
  EqSys.EquationSystemIgnore Node s (Data Nil Double) (Data ([] :> Nil) Double)
commonEnvHU _ =
  -- (foldMap (uncurry f) $ zip ss (tail ss))
  -- <>
  ( mconcat $
    (XIdx.storage Idx.initial storage .= D.fromList 20.0) :
    [] )
{-
  where f sec0 sec1 =
          XIdx.power sec0 storage crossing
            =%%= XIdx.power sec1 storage crossing
-}

givenEnvHUSec ::
  (Idx.Section, Sig.PSignal [] Double) ->
  EqSys.EquationSystemIgnore Node s (Data Nil Double) (Data ([] :> Nil) Double)
givenEnvHUSec (sec, Sig.TC sig) =
  mconcat $
  (XIdx.dTime sec .= D.map (const 1.0) sig) :
  (XIdx.power sec sink crossing .= sig) :

  -- Warunung: Ueberschreibt:
  (XIdx.x sec crossing sink .= D.map (const 0.3) sig) :

  ((EqSys.variable $ XIdx.eta sec crossing sink) =.=
    EqSys.liftF (D.map lookupCrSi) (EqSys.variable $ XIdx.power sec sink crossing)) :

{-
  ((EqSys.variable $ XIdx.eta sec  crossing storage) =.=
    EqSys.liftF (D.map lookupStCrDown)
                (EqSys.variable $ XIdx.power sec storage crossing)) :

-}

  -- invertieren!!! TODO!!!
  ((EqSys.variable $ XIdx.eta sec  crossing storage) =.=
    EqSys.liftF (D.map lookupStCrDown)
                (EqSys.variable $ XIdx.power sec crossing storage)) :


  ((EqSys.variable $ XIdx.eta sec  storage crossing) =.=
    EqSys.liftF (D.map lookupStCrUp)
                (EqSys.variable $ XIdx.power sec crossing storage)) :

  ((EqSys.variable $ XIdx.eta sec  source crossing) =.=
     EqSys.liftF (D.map lookupSoCr)
                 (EqSys.variable $ XIdx.power sec crossing source)) :
  []


givenEnvHU ::
  [Sig.PSignal [] Double] ->
  EqSys.EquationSystemIgnore Node s (Data Nil Double) (Data ([] :> Nil) Double)
givenEnvHU xs =
  let ys = zip (map Idx.Section [0..]) xs
  in  commonEnvHU (map fst ys)
      <>
      foldMap givenEnvHUSec ys

{-
etaSys ::
  (Show a, Num a, Fractional a, Show node, Ord node) =>
  Flow.RangeGraph node ->
  SeqFlow.Graph node b (EqRec.Absolute (Result a)) -> a
etaSys (_, topo) env = sum sinks / sum sources
  where m = Map.elems $ Graph.nodeEdges topo
        sinks = map (Set.foldl sinkEnergies 0 . fst3) $ filter isActiveSink m
        sources = map (Set.foldl sourceEnergies 0 . thd3) $ filter isActiveSource m

        isActiveSink (ns, Node.AlwaysSink, _) = p ns
        isActiveSink (ns, Node.Sink, _) = p ns
        isActiveSink _ = False

        isActiveSource (_, Node.AlwaysSource, ns) = p ns
        isActiveSource (_, Node.Source, ns) = p ns
        isActiveSource _ = False

        p = (> 0) .
            Set.size .
            Set.filter
              (\(Topo.FlowEdge (Topo.StructureEdge (Idx.InPart _ e))) -> Topo.isActive e)

        sinkEnergies acc
          (Topo.FlowEdge (Topo.StructureEdge (Idx.InPart sec
                       (Graph.EDirEdge (Graph.DirEdge a b))))) =
            acc + lookUp "etaSys" env (XIdx.energy sec b a)
        sinkEnergies = error "etaSys: sinkEnergies"

        sourceEnergies acc
          (Topo.FlowEdge (Topo.StructureEdge (Idx.InPart sec
                       (Graph.EDirEdge (Graph.DirEdge a b))))) =
            acc + lookUp "etaSys" env (XIdx.energy sec a b)
-}

lookUp ::
  (Node.C node, Show v) =>
  String ->
  SeqFlow.Graph node a (EqRec.Absolute (Result v)) ->
  XIdx.Energy node -> v
lookUp caller env n =
  case Var.checkedLookup caller SeqFlow.lookupEnergy n env of
    EqRec.Absolute (Determined x) -> x
    _ -> error (caller ++ ": undetermined " ++ Format.unUnicode (Var.formatIndex n))


etaSysHU ::
  SeqFlow.Graph
    Node
    (EqRec.Absolute (Result Double))
    (EqRec.Absolute (Result Double)) ->
  Double
etaSysHU env =
  (lu eSinkSec0 + lu eSinkSec1) / (lu eSource)
  where lu = lookUp "etaSysHU" env
        eSource = XIdx.energy sec0 source crossing
        eSinkSec0 = XIdx.energy sec0 sink crossing
        eSinkSec1 = XIdx.energy sec1 sink crossing




main :: IO ()
main = do

  let varX :: Sig.PSignal2 [] [] Double
      varX = Sig.fromList2 varX'

      varY :: Sig.XSignal2 [] [] Double
      varY = Sig.fromList2 varY'

      f0 x y = detEtaSys "sec0" $ EqSys.solve flowGraph $ givenSec0Mean x y
      f1 x y = detEtaSys "sec1" $ EqSys.solve flowGraph $ givenSec1Mean x y

      env0 = EqSys.solve flowGraph $ givenSec0Mean 4.0 0.4
      env1 = EqSys.solve flowGraph $ givenSec1Mean 3.0 0.3

      etaSys0, etaSys1 :: Sig.NSignal2 [] [] Double
      etaSys0 = Sig.fromList2 $ zipWith (zipWith f0) varX' varY'
      etaSys1 = Sig.fromList2 $ zipWith (zipWith f1) varX' varY'

      sinkRangeSig :: Sig.PSignal [] Double
      sinkRangeSig = Sig.fromList sinkRange

      sinkRangeSig2 :: Sig.TC Sig.TestRow (Typ A P Tt) (Data ([] :> Nil) Double)
      sinkRangeSig2 = Sig.fromList sinkRange

      maxEtaSys :: Sig.NSignal2 [] [] Double
      maxEtaSys = Sig.zipWith max etaSys0 etaSys1

      maxEtaSysState :: Sig.UTSignal2 [] [] Double
      maxEtaSysState = Sig.map fromIntegral $ Sig.argMax etaSys0 etaSys1

      maxEtaSys0, maxEtaSys1 :: Sig.NTestRow [] Double
      maxEtaSys0 = Sig.map2 maximum (Sig.transpose2 $ Sig.changeSignalType etaSys0)
      maxEtaSys1 = Sig.map2 maximum (Sig.transpose2 $ Sig.changeSignalType etaSys1)

      maxEtaLinear :: Sig.NTestRow [] Double
      maxEtaLinear = Sig.zipWith max maxEtaSys0 maxEtaSys1


      maxEtaSysStateLinear :: Sig.UTTestRow [] Int
      maxEtaSysStateLinear = Sig.argMax maxEtaSys0 maxEtaSys1

      bf = a . borderFunc maxEtaSysStateLinear sinkRangeSig
           where a 0 = (3, state3)
                 a 1 = (0, state0)
                 a _ = error "bf"

      optimalState = Sig.map (fst . bf) hypotheticalUsage

      (secs, secSigsHU) = unzip $ sectionHU (snd . bf) hypotheticalUsage

      envHU = EqSys.solve (assembleFlowTopos secs) $ givenEnvHU secSigsHU

      labeledEtaSys =
        PlotIO.label "Laden" etaSys0 :
        PlotIO.label "Entladen" etaSys1 :
        []

  concurrentlyMany_ [

    Draw.xterm $
      Draw.title "Section 0 Mean" $
        Draw.seqFlowGraph Draw.optionsDefault env0,
    Draw.xterm $
      Draw.title "Section 1 Mean" $
        Draw.seqFlowGraph Draw.optionsDefault env1,

    Draw.xterm $
      Draw.seqFlowGraph Draw.optionsDefault
        (flowGraph :: SeqFlow.Graph Node (Result Double) (Result Double)),

    Draw.xterm $
      Draw.title "Hypothetical Usage Sequence Flow Graph" $
      Draw.seqFlowGraph Draw.optionsDefault envHU,


    PlotIO.xy "Test" DefaultTerm.cons id sinkRangeSig2 [
      PlotIO.label "Laden"        maxEtaSys0,
      PlotIO.label "Entladen"     maxEtaSys1,
      PlotIO.label "maxEtaLinear" maxEtaLinear,
      PlotIO.label "Zustand" $ Sig.setType $
        Sig.map fromIntegral maxEtaSysStateLinear ],


    PlotIO.xy "Optimale Zustände" DefaultTerm.cons id sinkRangeSig [
      PlotIO.label "Hypothetical Usage" hypotheticalUsage,
      PlotIO.label "Optimal State" optimalState ],


    PlotIO.surface "Test" DefaultTerm.cons varX varY labeledEtaSys,
    PlotIO.surface "Systemwirkungsgrad Entladen"
                   DefaultTerm.cons varX varY etaSys0,
    PlotIO.surface "Systemwirkungsgrad Laden"
                   DefaultTerm.cons varX varY etaSys1,
    PlotIO.surface "Systemwirkungsgrad Laden und Entladen"
                   DefaultTerm.cons varX varY labeledEtaSys,
    PlotIO.surface "Maximaler Systemwirkungsgrad"
                   DefaultTerm.cons varX varY (PlotIO.label "Max" maxEtaSys),
    PlotIO.surface "Test"
                   DefaultTerm.cons varX varY (PlotIO.label "Max" maxEtaSysState) ]

{-
main2 :: IO ()
main2 = do
  let y = 1

      f e x = lookUp "f" (EqSys.solve flowGraph $ givenSec0Mean x y) e
      esc1 = XIdx.energy sec1 sink crossing
      ecs1 = XIdx.energy sec1 crossing sink
      estc1 = XIdx.energy sec1 storage crossing

      ecst0 = XIdx.energy sec0 crossing storage

      eSource = XIdx.energy sec0 source crossing

      -- sinkRangeSig :: Sig.PSignal [] Double
      sinkRangeSig = Sig.fromList sinkRange

      --esc1Sig, ecs1Sig, estc1Sig, ecst0Sig, eSourceSig :: Sig.PSignal [] Double
      esc1Sig = Sig.fromList $ map (f esc1) sinkRange
      ecs1Sig = Sig.fromList $ map (f ecs1) sinkRange
      estc1Sig = Sig.fromList $ map (f estc1) sinkRange
      ecst0Sig = Sig.fromList $ map (f ecst0) sinkRange
      eSourceSig = Sig.fromList $ map (f eSource) sinkRange

      h x = etaSys flowGraph $ EqSys.solve flowGraph $ givenSec0Mean x y
      etaSysSig = Sig.fromList $ map h sinkRange

      g 0 = "Sec 1, sink crossing"
      g 1 = "Sec 1, crossing sink"
      g 2 = "Sec 1, storage crossing"
      g 3 = "Sec 0, crossing storage"
      g 4 = "EtaSys"
      g 5 = "Sec 0, source crossing"

  concurrentlyMany_ [
    PlotIO.xy "Test" DefaultTerm.cons id g sinkRangeSig
              [ esc1Sig, ecs1Sig, estc1Sig, ecst0Sig, etaSysSig, eSourceSig],
    Draw.xterm $ Draw.seqFlowGraph flowGraph ]
-}
