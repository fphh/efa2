{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}


module Main where

import EFA.Example.Topology.Tripod (Node, source, crossing, sink, storage)
import EFA.Example.Topology.Tripod.State
          (flowGraph, state0, state3, sec0, sec1)

import qualified EFA.Application.Plot as PlotIO
import EFA.Application.Utility (seqFlowGraphFromFlowTopos)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.SystemEta (detEtaSys)
import EFA.Flow.Sequence.Absolute ((.=), (=%%=), (=.=))

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.Signal.Data as D
import EFA.Signal.Typ (Typ,A,P,Tt)
import EFA.Signal.Data (Data, Nil, (:>))

import EFA.Equation.Result (Result)

import EFA.Utility.Async (concurrentlyMany_)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

import qualified Data.NonEmpty.Mixed as NonEmptyMixed
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List as List

import Data.Eq.HT (equating)
import Data.Ord (comparing)
import Data.Monoid (mconcat, (<>))
import Data.Foldable (foldMap)
import Data.Tuple.HT (swap)


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


sinkRange :: [Double]
sinkRange = [0.1]

ratioRange :: [Double]
ratioRange = [0.1]

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
  (D.FromList c0, D.Storage c0 d0, D.NestedList c0 d0 ~ [d0], Show d0, Eq d0,
   D.FromList c1, D.Storage c1 d1, D.NestedList c1 d1 ~ [d1], Show d1, Ord d1) =>
  Sig.TC s0 t0 (Data c0 d0) ->
  Sig.TC s1 t1 (Data c1 d1) -> d1 -> d0
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
  ( mconcat $
    (XIdx.storage Idx.initial storage .= D.fromList 20.0) :
    [] )

givenEnvHUSec ::
  (Idx.Section, Sig.PSignal [] Double) ->
  EqSys.EquationSystemIgnore Node s (Data Nil Double) (Data ([] :> Nil) Double)
givenEnvHUSec (sec, Sig.TC sig) =
  mconcat $
  (XIdx.dTime sec .= D.map (const 1.0) sig) :
  (XIdx.power sec sink crossing .= sig) :

  -- Warnung: Ueberschreibt:
  (XIdx.x sec crossing sink .= D.map (const 0.3) sig) :

  ((EqSys.variable $ XIdx.eta sec crossing sink) =.=
    EqSys.liftF (D.map lookupCrSi) (EqSys.variable $ XIdx.power sec sink crossing)) :

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
      maxEtaSysState =
         Sig.map (fromIntegral . fromEnum) $ Sig.zipArgMax etaSys0 etaSys1

      maxEtaSys0, maxEtaSys1 :: Sig.NTestRow [] Double
      maxEtaSys0 = Sig.map2 maximum (Sig.transpose2 $ Sig.changeSignalType etaSys0)
      maxEtaSys1 = Sig.map2 maximum (Sig.transpose2 $ Sig.changeSignalType etaSys1)

      maxEtaLinear :: Sig.NTestRow [] Double
      maxEtaLinear = Sig.zipWith max maxEtaSys0 maxEtaSys1


      maxEtaSysStateLinear :: Sig.UTTestRow [] Sig.ArgMax
      maxEtaSysStateLinear = Sig.zipArgMax maxEtaSys0 maxEtaSys1

      bf = a . borderFunc maxEtaSysStateLinear sinkRangeSig
           where a Sig.ArgMax0 = (3, state3)
                 a Sig.ArgMax1 = (0, state0)

      optimalState = Sig.map (fst . bf) hypotheticalUsage

      (secs, secSigsHU) = unzip $ sectionHU (snd . bf) hypotheticalUsage

      envHU =
        EqSys.solve (seqFlowGraphFromFlowTopos secs) $ givenEnvHU secSigsHU

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
        Sig.map (fromIntegral . fromEnum) maxEtaSysStateLinear ],


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
