{-# LANGUAGE FlexibleContexts #-}


{-

TODO

* Generisches Forcing für Speicherkanten.

* Maybes sollen mit defaultwert in plot geplottet werden.

* powerrecord aus eingelesenen Signalen bauen.

* interpolation in kombination mit nan prüfen

* addZeroCrossings funktioniert mit Listen

* Funktion um Section zu plotten.

* simulationIO soll generisch für n reqs werden.

* inaktive Kanten schon in extractOptimalPowerMatricesPerState erkennen
  sollen 0 Leistung liefern

* findZeroCrossing verstehen

* fromSequenceFlow Wirkung von allStEdges ???

* optimalSolutionGeneric nicht richtig, Email Henning 11.12.2013 12:03

* wiki-Artikel wg forall verbessern

* to2DMatrix, head entfernen

-}

module Main where

import qualified Modules.System as System; import Modules.System (Node)
import qualified Modules.Utility as ModUt
import qualified Modules.Setting as ModSet
import qualified Modules.Optimisation.Base as Base
import qualified Modules.Optimisation.NonIO as NonIO
import qualified Modules.Types as Types

import Modules.Types (EnvResult)

import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.DoubleSweep as DoubleSweep
import EFA.Application.Sweep (Sweep)

import qualified EFA.Application.Optimisation as AppOpt

import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Flow.State.SystemEta as StateEta

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.ConvertTable as CT
import EFA.Signal.Data (Data(Data))

import qualified EFA.IO.TableParser as Table

import EFA.Equation.Result (Result(Determined))
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~*), (~+), (~-), (~/))

import EFA.Equation.Arithmetic (Sign(Zero, Positive, Negative))

import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty; import Data.NonEmpty ((!:))
import qualified Data.Empty as Empty

import Data.Vector (Vector)
import qualified Data.Vector.Unboxed as UV

import Data.Tuple.HT (thd3)

import Control.Monad (void)



setChargeDrive ::
  One.OptimalEnvParams Node list sweep vec a ->
  a ->
  One.OptimalEnvParams Node list sweep vec a
setChargeDrive params x =
  params { One.forcingPerNode =
             Map.fromList [(System.Water, One.ChargeDrive x)] }

betterFalsePosition ::
  (Ord a, Arith.Constant a) =>
  One.OptimalEnvParams Node list sweep vec a ->
  (One.OptimalEnvParams Node list sweep vec a -> t) ->
  (t -> a) -> a -> a -> [(a, t)]
betterFalsePosition params f accessf a1 a2 =
  go a1 a2 (f $ setChargeDrive params a1) (f $ setChargeDrive params a2)
  where go x1 x2 f1 f2 =
          let y1 = accessf f1
              y2 = accessf f2
              z = x1 ~- y1 ~* (x2 ~- x1) ~/ (y2 ~- y1)
              fz = f $ setChargeDrive params z
              yz = accessf fz
              epsx = Arith.fromRational $ 1e-4
              fnew = f $ setChargeDrive params (y2 ~* y1 ~/ (y2 ~+ yz))
          in (x1, f1) :
               if Arith.abs (x2 ~- x1) < epsx
                then [(z, fz)]
                else if yz ~* y2 < Arith.zero
                        then go x2 z f2 fz
                        else go x1 z fnew fz

findZeroCrossing ::
  (Arith.Sum t, Ord t, Arith.Product t, Arith.Constant t,
   Ord s, Arith.Constant s) =>
  One.OptimalEnvParams Node list sweep vec t ->
  (One.OptimalEnvParams Node list sweep vec t -> z) ->
  (z -> s) ->
  t ->
  t ->
  [(t, t, z)]
findZeroCrossing params f accessf xstart step =
  case Arith.sign $ accessf ystart of
       Negative -> go xstart ystart step
       Zero     -> [(xstart, step, ystart)]
       Positive -> go xstart ystart (Arith.negate step)
  where
    ystart = f $ setChargeDrive params xstart

    go x0 y0 st =
      let x1 = x0 ~+ st
          y1 = f $ setChargeDrive params x1

          _2 = Arith.fromInteger 4

      in (x0, st, y0) :
           case (Arith.sign $ accessf y0, Arith.sign $ accessf y1) of
                (Negative, Negative) -> go x1 y1 (st ~* _2)
                (Positive, Positive) -> go x1 y1 (st ~* _2)
                (Negative, Positive) -> go x0 y0 (st ~/ _2)
                (Positive, Negative) -> go x0 y0 (st ~/ _2)
                (Zero, _)  -> []
                (_, Zero)  -> []


iterateUntil ::
  (Enum a, Num a, Ord a, Arith.Sum b, Ord b) =>
  a -> b -> [(c, b, e)] -> (Int, (a, (c, b, e)))
iterateUntil cnt eps ws =
  let (start, res) =
        case span p (zip [0..] ws) of
             (s, r:_) -> (s, r)
             _ -> error "Main.iterateUntil"
      p (n, (_, step, _)) = n < cnt && (Arith.abs step > eps)
  in (length start, res)

iterateBalanceIO ::
  One.OptimalEnvParams Node [] Sweep UV.Vector Double ->
  Record.PowerRecord Node Vector Double ->
  EnvResult Node (Sweep UV.Vector Double) ->
  IO (EnvResult Node (Sweep UV.Vector Double))
iterateBalanceIO params reqsRec stateFlowGraphOpt = do

  let
      perStateSweep = Base.perStateSweep params stateFlowGraphOpt

      g sweeps pars =
        let --(stfg, sefg, givenSigs) =
            Types.Optimisation _ sim =
              NonIO.optimiseAndSimulate pars reqsRec sweeps
            rec = Record.partIntegrate (Types.signals sim)

            net2wat = TopoIdx.ppos System.Network System.Water

            Sig.TC (Data bl) =
              Sig.neg $ Sig.sum $ Record.getSig rec net2wat

        in (Types.stateFlowGraph sim, Types.sequenceFlowGraph sim, bl)

      (len, (_, _, (statefg, _, bal))) =
        snd $ iterateUntil (100 :: Int) (1e-6)
            $ findZeroCrossing params (g perStateSweep) thd3 0 1

      eta = case StateEta.etaSys statefg of
                 Determined e -> UV.head (Sweep.fromSweep e)
                 _ -> error "Main.iterateBalanceIO"



  putStrLn (show len ++ "\t" ++ show bal ++ "\t" ++ show eta)

{-
  time <- getCurrentTime
  let timeStr = map f $ show time
      f ' ' = '_'
      f x = x


  Draw.xterm
           $ Draw.bgcolour LimeGreen
           $ Draw.title ("Sequence Flow Graph from Simulation\\lBalance " ++ show bal ++ "\\l")
           $ Draw.seqFlowGraph Draw.optionsDefault seqfg


  Draw.xterm
           $ Draw.bgcolour Orchid1
           $ Draw.title ("State Flow Graph from Simulation\\lBalance " ++ show bal ++ "\\l")
           $ Draw.stateFlowGraph Draw.optionsDefault
           $ StateQty.mapGraph (fmap (head . Sweep.toList)) (fmap (head . Sweep.toList)) statefg
-}


  return statefg


initEnv ::
  (Arith.Constant a, Sweep.SweepMap sweep vec a a,
   Sweep.SweepClass sweep vec a) =>
  One.OptimalEnvParams Node list sweep vec a->
  EnvResult Node (sweep vec a)
initEnv params = AppOpt.initialEnv params System.stateFlowGraph





main1 :: IO()
main1 = do

  tabEta <- Table.read "../maps/eta.txt"
  tabPower <- Table.read "../maps/power.txt.bak"

  let etaMap =
         Map.mapKeys One.Name $
         CT.makeEtaFunctions2D
            (Map.mapKeys (\(One.Name str) -> str) ModSet.scaleTableEta)
            tabEta

  let

      (time,
       NonEmpty.Cons r
          (NonEmpty.Cons l Empty.Cons)) =
        CT.getPowerSignalsWithSameTime tabPower
          ("rest" !: "local" !: Empty.Cons)

      transform = Sig.offset 0.1 . Sig.scale 2.9

      prest, plocal :: Sig.PSignal Vector Double
      prest = Sig.convert $ transform r
      plocal = Sig.convert $ transform l

      reqsRec :: Record.PowerRecord Node Vector Double
      reqsRec =
        Record.Record (Sig.convert time)
                      (Map.fromList (zip ModSet.reqs [prest, plocal]))

      pts = DoubleSweep.mkPts2 ModSet.sweepPts

      optParams :: One.OptimalEnvParams Node [] Sweep UV.Vector Double
      optParams =
        One.OptimalEnvParams
          System.topology
          ModSet.initStorageState
          ModSet.initStorageSeq
          etaMap
          System.etaAssignMap
          pts
          ModSet.forcingMap
          ModSet.dofs
          ModSet.reqs
          ModSet.sweepLength


  putStrLn $ "Steps\tBalance\t\t\tEta\t"

  void $
     ModUt.nestM 5
        (iterateBalanceIO optParams reqsRec)
        ( AppOpt.storageEdgeXFactors optParams 3 3
          $ AppOpt.initialEnv optParams System.stateFlowGraph)

main :: IO ()
main = main1
