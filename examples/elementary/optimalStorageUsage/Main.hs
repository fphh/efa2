{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}


module Main where

import qualified EFA.Application.Absolute as EqGen
import qualified EFA.Application.Index as XIdx
import EFA.Application.Absolute ( (.=), (=%%=), (=.=) )
import EFA.Application.EtaSys (etaSys)
import EFA.Application.Utility (makeEdges)

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.ConvertTable as Table
import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Data as D
import qualified EFA.Signal.PlotIO as PlotIO
import EFA.Signal.Data (Data, Nil, (:>))

import qualified EFA.Equation.Record as EqRec
import qualified EFA.Equation.Environment as EqEnv
import EFA.Equation.Result (Result(..))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Map (checkedLookup)
import EFA.Utility.Async (concurrentlyMany_)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Eq.HT (equating)
import Data.Ord (comparing)
import Data.Monoid (mconcat, (<>))
import Data.Foldable (foldMap)

import Debug.Trace (trace)


sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0



source, crossing, sink, storage :: Node.Int
source :~ crossing :~ sink :~ storage :~ _ = Stream.enumFrom minBound

topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.fromList ns (makeEdges es)
  where ns = [ (source, TD.Source),
               (crossing, TD.Crossing),
               (sink, TD.Sink),
               (storage, TD.Storage ()) ]
        es = [ (source, crossing),
               (crossing, sink),
               (crossing, storage) ]



select :: [topo] -> [Int] -> SD.SequData topo
select ts = SD.fromList . map (ts !!)

seqTopoFunc :: [Int] -> Flow.RangeGraph Node.Int
seqTopoFunc states = Flow.sequenceGraph (select sol states)
  where sol = StateAnalysis.advanced topoDreibein

seqTopo :: Flow.RangeGraph Node.Int
seqTopo = seqTopoFunc [0, 3]


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

commonEnv :: EqGen.EquationSystem Node.Int s Double Double
commonEnv =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (XIdx.dTime sec1 .= 1) :
   (XIdx.storage TIdx.initial storage .= 0) :
   (XIdx.power sec0 storage crossing =%%= XIdx.power sec1 storage crossing) :
   []

givenSec0Mean ::
  Double -> Double -> EqGen.EquationSystem Node.Int s Double Double
givenSec0Mean psink _ =
   (commonEnv <>) $
   mconcat $

   (XIdx.x sec0 crossing storage .= 0.2) :
   (XIdx.power sec1 sink crossing .= psink) :


   (XIdx.eta sec0 crossing sink .= 0.5) :

   (XIdx.eta sec0 source crossing .= 0.7) :

   (XIdx.eta sec0 crossing storage .= 0.6) :

   ((EqGen.variable $ XIdx.eta sec1 storage crossing) =.=
     EqGen.liftF lookupStCrUp (EqGen.variable $ XIdx.power sec1 crossing storage)) :

   ((EqGen.variable $ XIdx.eta sec1 crossing sink) =.=
     EqGen.liftF lookupCrSi (EqGen.variable $ XIdx.power sec1 sink crossing)) :

   []

givenSec1Mean ::
  Double -> Double -> EqGen.EquationSystem Node.Int s Double Double
givenSec1Mean psink _ =
   (commonEnv <>) $
   mconcat $


   (XIdx.power sec0 sink crossing .= psink) :

   (XIdx.power sec1 sink crossing .= 2) :

   (XIdx.eta sec1 crossing sink .= 0.9) :

   (XIdx.eta sec1 storage crossing .= 0.85) :


   ((EqGen.variable $ XIdx.eta sec0 crossing storage) =.=
     EqGen.liftF lookupStCrDown (EqGen.variable $ XIdx.power sec0 storage crossing)) :

   ((EqGen.variable $ XIdx.eta sec0 crossing sink) =.=
     EqGen.liftF lookupCrSi (EqGen.variable $ XIdx.power sec0 sink crossing)) :

   ((EqGen.variable $ XIdx.eta sec0 source crossing) =.=
     EqGen.liftF lookupSoCr (EqGen.variable $ XIdx.power sec0 crossing source)) :

   []



etaSys2 ::
  (Ord (edge k), Ord k, Show k, Show (edge k), Show t2, Gr.Edge edge) =>
  (t1, Gr.Graph k edge (TD.NodeType t2) edgeLabel) -> t -> a
etaSys2 (_, topo) _ = trace (show sinks) undefined
  where sinks = Map.filter isSink $ Gr.nodeEdges topo
        isSink (_, el, x) =
          case el of
               TD.AlwaysSource -> Set.size x > 0
               TD.Source -> Set.size x > 0
               _ -> False



sinkRange :: [Double]
sinkRange = [0.1] -- , 0.2 .. 20]

ratioRange :: [Double]
ratioRange = [0.1] -- , 0.2 .. 0.9]

varX', varY' :: [[Double]]
(varX', varY') = Table.varMat sinkRange ratioRange

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
  where ts = zip (Sig.toList ss) (Sig.toList xs)
        ys = map f (List.groupBy (\x y -> fst x == fst y) ts)
        f as = case last as of (a, b) -> (b, a)
        zs = List.sortBy (comparing fst) ys

sectionHU ::
  Sig.PSignal [] d -> (d -> Int) -> [(Int, Sig.PSignal [] d)]
sectionHU ss bf = ws
  where ts = Sig.toList ss
        us = List.groupBy (equating fst) $ zip (map bf ts) ts
        ws = map f us
        f ((s, w):xs) = (s, Sig.fromList (w:(map snd xs)))
        f _ = error "sectionHU: Unbekannter Fehler!"


commonEnvHU ::
  [TIdx.Section] ->
  EqGen.EquationSystem Node.Int s (Data Nil Double) (Data ([] :> Nil) Double)
commonEnvHU _ =
  -- (foldMap (uncurry f) $ zip ss (tail ss))
  -- <>
  ( mconcat $
    (XIdx.storage TIdx.initial storage .= D.fromList 20.0) :
    [] )
{-
  where f sec0 sec1 =
          XIdx.power sec0 storage crossing
            =%%= XIdx.power sec1 storage crossing
-}

givenEnvHUSec ::
  (TIdx.Section, Sig.PSignal [] Double) ->
  EqGen.EquationSystem Node.Int s (Data Nil Double) (Data ([] :> Nil) Double)
givenEnvHUSec (sec, Sig.TC sig) =
  mconcat $
  (XIdx.dTime sec .= D.map (const 1.0) sig) :
  (XIdx.power sec sink crossing .= sig) :

  -- Warunung: Ueberschreibt:
  (XIdx.x sec crossing sink .= D.map (const 0.3) sig) :

  ((EqGen.variable $ XIdx.eta sec crossing sink) =.=
    EqGen.liftF (D.map lookupCrSi) (EqGen.variable $ XIdx.power sec sink crossing)) :

{-
  ((EqGen.variable $ XIdx.eta sec  crossing storage) =.=
    EqGen.liftF (D.map lookupStCrDown)
                (EqGen.variable $ XIdx.power sec storage crossing)) :

-}

  -- invertieren!!! TODO!!!
  ((EqGen.variable $ XIdx.eta sec  crossing storage) =.=
    EqGen.liftF (D.map lookupStCrDown)
                (EqGen.variable $ XIdx.power sec crossing storage)) :


  ((EqGen.variable $ XIdx.eta sec  storage crossing) =.=
    EqGen.liftF (D.map lookupStCrUp)
                (EqGen.variable $ XIdx.power sec crossing storage)) :

  ((EqGen.variable $ XIdx.eta sec  source crossing) =.=
     EqGen.liftF (D.map lookupSoCr)
                 (EqGen.variable $ XIdx.power sec crossing source)) :
  []


givenEnvHU ::
  [Sig.PSignal [] Double] ->
  EqGen.EquationSystem Node.Int s (Data Nil Double) (Data ([] :> Nil) Double)
givenEnvHU xs =
  let ys = zip (map TIdx.Section [0..]) xs
  in  commonEnvHU (map fst ys)
      <>
      (foldMap givenEnvHUSec ys)

{-
etaSys ::
  (Show a, Num a, Fractional a, Show node, Ord node) =>
  Flow.RangeGraph node ->
  EqEnv.Complete node b (EqRec.Absolute (Result a)) -> a
etaSys (_, topo) env = sum sinks / sum sources
  where m = Map.elems $ Gr.nodeEdges topo
        sinks = map (Set.foldl sinkEnergies 0 . fst3) $ filter isActiveSink m
        sources = map (Set.foldl sourceEnergies 0 . thd3) $ filter isActiveSource m

        isActiveSink (ns, TD.AlwaysSink, _) = p ns
        isActiveSink (ns, TD.Sink, _) = p ns
        isActiveSink _ = False

        isActiveSource (_, TD.AlwaysSource, ns) = p ns
        isActiveSource (_, TD.Source, ns) = p ns
        isActiveSource _ = False

        p = (> 0) .
            Set.size .
            Set.filter
              (\(TD.FlowEdge (TD.StructureEdge (TIdx.InPart _ e))) -> TD.isActive e)

        sinkEnergies acc
          (TD.FlowEdge (TD.StructureEdge (TIdx.InPart sec
                       (Gr.EDirEdge (Gr.DirEdge a b))))) =
            acc + lookUp "etaSys" env (XIdx.energy sec b a)
        sinkEnergies = error "etaSys: sinkEnergies"

        sourceEnergies acc
          (TD.FlowEdge (TD.StructureEdge (TIdx.InPart sec
                       (Gr.EDirEdge (Gr.DirEdge a b))))) =
            acc + lookUp "etaSys" env (XIdx.energy sec a b)
-}

lookUp ::
  (Ord node, Show node, Show t) =>
  String ->
  EqEnv.Complete node b (EqRec.Absolute (Result t)) ->
  XIdx.Energy node -> t
lookUp caller env n =
  case checkedLookup caller
         (EqEnv.energyMap $ EqEnv.signal env) n of
       EqRec.Absolute (Determined x) -> x
       _ -> error (show n ++ "\n" ++ show (EqEnv.energyMap $ EqEnv.signal env))


etaSysHU ::
  EqEnv.Complete
    Node.Int
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

      getDet (Determined x) = x
      getDet _ = error "getDet"

      f0 x y = getDet $ etaSys seqTopo $ EqGen.solve seqTopo $ givenSec0Mean x y
      f1 x y = getDet $ etaSys seqTopo $ EqGen.solve seqTopo $ givenSec1Mean x y

      env0 = EqGen.solve seqTopo $ givenSec0Mean 4.0 0.4
      env1 = EqGen.solve seqTopo $ givenSec1Mean 3.0 0.3

      etaSys0, etaSys1 :: Sig.NSignal2 [] [] Double
      etaSys0 = Sig.fromList2 $ zipWith (zipWith f0) varX' varY'
      etaSys1 = Sig.fromList2 $ zipWith (zipWith f1) varX' varY'

      sinkRangeSig :: Sig.PSignal [] Double
      sinkRangeSig = Sig.fromList sinkRange

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
           where a 0 = 3
                 a 1 = 0
                 a _ = error "bf"

      optimalState = Sig.map bf hypotheticalUsage

      secHU = sectionHU hypotheticalUsage bf

      (secs, secSigsHU) = unzip secHU
      seqTopoHU = seqTopoFunc secs

      envHU = EqGen.solve seqTopoHU $ givenEnvHU secSigsHU

      f 0 = "Laden"
      f 1 = "Entladen"
      f _ = error "f"

      g 0 = "Laden"
      g 1 = "Entladen"
      g 2 = "maxEtaLinear"
      g 3 = "Zustand"
      g _ = error "g"

      h 0 = "Hypothetical Usage"
      h 1 = "Optimal State"
      h _ = error "h"

  concurrentlyMany_ [

    Draw.xterm $
      Draw.title "Section 0 Mean" $ Draw.sequFlowGraphAbsWithEnv seqTopo env0,
    Draw.xterm $
      Draw.title "Section 1 Mean" $ Draw.sequFlowGraphAbsWithEnv seqTopo env1,

    Draw.xterm $ Draw.sequFlowGraph seqTopo,

    Draw.xterm $
      Draw.title "Hypothetical Usage Sequence Flow Graph" $
      Draw.sequFlowGraphAbsWithEnv seqTopoHU envHU,

{-
    PlotIO.xy "Test" DefaultTerm.cons id g sinkRangeSig
              [ maxEtaSys0, maxEtaSys1,
                maxEtaLinear,
                Sig.map fromIntegral maxEtaSysStateLinear],
-}

    PlotIO.xy "Optimale Zustände" DefaultTerm.cons id h sinkRangeSig
              [hypotheticalUsage, Sig.map fromIntegral optimalState],


    PlotIO.surface "Test" DefaultTerm.cons f varX varY [etaSys0, etaSys1],
    PlotIO.surface "Systemwirkungsgrad Entladen"
                   DefaultTerm.cons (const "") varX varY etaSys0,
    PlotIO.surface "Systemwirkungsgrad Laden"
                   DefaultTerm.cons (const "") varX varY etaSys1,
    PlotIO.surface "Systemwirkungsgrad Laden und Entladen"
                   DefaultTerm.cons f varX varY [etaSys0, etaSys1],
    PlotIO.surface "Maximaler Systemwirkungsgrad"

                   DefaultTerm.cons (const "Max") varX varY maxEtaSys,
    PlotIO.surface "Test" DefaultTerm.cons (const "Max") varX varY maxEtaSysState ]

{-
main2 :: IO ()
main2 = do
  let y = 1

      f e x = lookUp "f" (EqGen.solve seqTopo $ givenSec0Mean x y) e
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

      h x = etaSys seqTopo $ EqGen.solve seqTopo $ givenSec0Mean x y
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
    Draw.xterm $ Draw.sequFlowGraph seqTopo ]
-}