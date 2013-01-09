module Main where

import EFA2.Topology.Topology (makeAllEquations)
import EFA2.Topology.TopologyData
          (Topology, SequFlowGraph,
           NodeType(Crossing, Sink, Source, Storage))
import qualified EFA2.Topology.EfaGraph as Gr

import EFA2.Solver.Equation (give, toAbsEquations)
import EFA2.Solver.EquationOrder (order)

import EFA2.Interpreter.Env
import EFA2.Interpreter.Interpreter (interpretFromScratch, eqToInTerm)
import EFA2.Interpreter.Arith (Val)

import EFA2.Report.Report (report)
import EFA2.Signal.Plot (surfPlot, xyplot)
import EFA2.Topology.Draw (drawTopology)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Sequence as Seq
import EFA2.Signal.SequenceData (PPosIdx(..), PowerRecord(..))
import EFA2.Signal.SignalFill ((.-), (.+), (./))
import EFA2.Signal.Signal
          (TC, Scalar, FSamp, PFSamp, PSigL, UTFSig, Test1, Test2,
           toSigList, toScalar, makeDelta, makeAbsolute,
           (.++))
import EFA2.Signal.Data (Data, Nil)
import EFA2.Signal.Typ (Typ, A, Tt, Y, P, N, F)

import EFA2.Utils.Utils (safeLookup)

import qualified Data.List.Match as Match
import qualified Data.Map as M


mkSig :: Int -> [Val] -> PSigL
mkSig n = S.fromList . concat . replicate n

sec0, sec1, secm :: Idx.Section
sec0 = Idx.Section 0
sec1 = Idx.Section 1
secm = Idx.initSection

rec :: Idx.Record
rec = Idx.Record 0

edgeIdx ::
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> idx) ->
   Idx.Section -> Int -> Int -> idx
edgeIdx mkIdx sec x y =
   mkIdx rec (Idx.SecNode sec (Idx.Node x)) (Idx.SecNode sec (Idx.Node y))

pPosIdx :: Int -> Int -> PPosIdx
pPosIdx x y = PPosIdx (Idx.Node x) (Idx.Node y)


makeNodes :: [(Int, NodeType)] -> [Gr.LNode Idx.Node NodeType]
makeNodes ns = map f ns
  where f (n, ty) = (Idx.Node n, ty)

makeEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node ()]
makeEdges es = map f es
  where f (a, b) = (Gr.Edge (Idx.Node a) (Idx.Node b), ())


-- | A. Generate System Topology definition
topo :: Topology
topo = Gr.mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Crossing), (2, Sink), (3, Storage)]
        edges = [(0, 1), (1, 2), (1, 3)]

-- -- | B. Generating a sequence Topology using a fake dataset
sqTopo :: SequFlowGraph
sqTopo = Seq.makeSeqFlowGraph topo $ Seq.makeSequence pRec
  where
    s01 = [0, 2, 2, 0, 0, 0]
    s10 = [0, 0.8, 0.8, 0, 0, 0]
    s12 = [0.3, 0.3, 0.3, 0.3, 0.3, 0.3]
    s21 = [0.2, 0.2, 0.2, 0.2, 0.2, 0.2]
    s13 = [0, 0.5, 0.5, 0, -0.3, -0.3]
    s31 = [0, 0.25, 0.25, 0, -0.6, -0.6]

    n = 1

    time = take 7 [0 ..]

    pMap =  M.fromList $
       (pPosIdx 0 1, mkSig n s01 .++ (S.fromList [head s01] :: PSigL)) :
       (pPosIdx 1 0, mkSig n s10 .++ (S.fromList [head s10] :: PSigL)) :
       (pPosIdx 1 2, mkSig n s12 .++ (S.fromList [head s12] :: PSigL)) :
       (pPosIdx 2 1, mkSig n s21 .++ (S.fromList [head s21] :: PSigL)) :
       (pPosIdx 1 3, mkSig n s13 .++ (S.fromList [head s13] :: PSigL)) :
       (pPosIdx 3 1, mkSig n s31 .++ (S.fromList [head s31] :: PSigL)) :
       []


    pRec = PowerRecord (S.fromList time) pMap

-- | C. System solving
solve3Way :: SequFlowGraph -> Val -> Val -> Envs SingleRecord UTFSig
solve3Way sqTp y n = interpretFromScratch (SingleRecord rec) 1 gd -- interprete and solve equations

{-
  -- Sequence 0 Primary Source Active additionally charging storage
  where givenEnv0 = emptyEnv { recordNumber = SingleRecord 0,
                               dtimeMap = M.fromList [ (edgeIdx Idx.DTime sec0, S.fromList [1.0]) ],
                               powerMap = M.fromList [ (edgeIdx Idx.Power sec0 3 1, S.fromList [x]),
                                                       (edgeIdx Idx.Power sec0 2 1, S.fromList [0.6])],
                               fetaMap =  M.fromList [ (edgeIdx Idx.FEta sec0 0 1, S.map etaf), (edgeIdx Idx.FEta sec0 1 0, undefined),
                                                       (edgeIdx Idx.FEta sec0 1 2, S.map (const 1)), (edgeIdx Idx.FEta sec0 2 1, S.map (const 1)),
                                                       (edgeIdx Idx.FEta sec0 1 3, S.map (const y)), (edgeIdx Idx.FEta sec0 3 1, S.map (const y)) ] }
        -- Sequence 1 -- using storage only
        givenEnv1 = emptyEnv { recordNumber = SingleRecord 0,
                           --    dtimeMap = M.fromList [ (edgeIdx Idx.DTime sec1, S.fromList [1.0]) ],
                               energyMap = M.fromList [ (edgeIdx Idx.Energy sec1 3 1, S.fromList [x] :: UTFSig) ],
                               powerMap =  M.fromList [ (edgeIdx Idx.Power sec1 2 1, S.fromList [0.6]) ],
                               fetaMap =   M.fromList [ (edgeIdx Idx.FEta sec1 0 1, S.map etaf), (edgeIdx Idx.FEta sec1 1 0, undefined),
                                                        (edgeIdx Idx.FEta sec1 1 2, S.map (const 1)), (edgeIdx Idx.FEta sec1 2 1, S.map (const 1)),
                                                        (edgeIdx Idx.FEta sec1 1 3, S.map (const y)), (edgeIdx Idx.FEta sec1 3 1, S.map (const y)) ] }
-}

  -- Sequence 0 Primary Source Active additionally charging storage
  where givenEnv0 = emptyEnv { recordNumber = SingleRecord rec,
                               dtimeMap = M.fromList [ (Idx.DTime rec sec0, S.fromList [(1-y)*dt]) ],
                               powerMap = M.fromList [ (edgeIdx Idx.Power sec0 2 1, S.fromList [pCons])],
                               energyMap = M.fromList [ (edgeIdx Idx.Energy sec0 3 1, S.fromList [dt*y*pCons/n/0.9])],
                               --energyMap = M.fromList [ (edgeIdx Idx.Energy sec0 3 1, S.fromList [dt*y*pCons/n/0.9]), (edgeIdx Idx.Energy secm -1 3, S.fromList [1])],
                               fetaMap =  M.fromList [ (edgeIdx Idx.FEta sec0 0 1, S.map etaf), (edgeIdx Idx.FEta sec0 1 0, undefined),
                                                       (edgeIdx Idx.FEta sec0 1 2, S.map (const 0.9)), (edgeIdx Idx.FEta sec0 2 1, S.map (const 0.9)),
                                                       (edgeIdx Idx.FEta sec0 1 3, S.map (const n)), (edgeIdx Idx.FEta sec0 3 1, S.map (const n)) ] }
        -- Sequence 1 -- using storage only
        givenEnv1 = emptyEnv { recordNumber = SingleRecord rec,
                               dtimeMap = M.fromList [ (Idx.DTime rec sec1, S.fromList [y*dt])],
                               --energyMap = M.fromList [(edgeIdx Idx.Energy secm -1 3, S.fromList [1]) ],
                               energyMap = M.fromList [],
                               powerMap =  M.fromList [ (edgeIdx Idx.Power sec1 2 1, S.fromList [pCons])],
                               fetaMap =   M.fromList [ (edgeIdx Idx.FEta sec1 0 1, S.map etaf), (edgeIdx Idx.FEta sec1 1 0, undefined),
                                                        (edgeIdx Idx.FEta sec1 1 2, S.map (const 0.9)), (edgeIdx Idx.FEta sec1 2 1, S.map (const 0.9)),
                                                        (edgeIdx Idx.FEta sec1 1 3, S.map (const n)), (edgeIdx Idx.FEta sec1 3 1, S.map (const n)) ] }

        -- Variable Efficiency function at Source (backwards lookup)
--        etaf x = 1/((x+sqrt(x*x+4*x))/(2*x))
        etaf x = 1/((x+sqrt(x*x+4*x))/(2*x))

        pCons = 1.0
        dt = 1



        -- Generate Equations for both Sections
        (sqEnvs, ts') = makeAllEquations sqTp [givenEnv0, givenEnv1]

        -- set initial values in equation system
        storage0 = edgeIdx Idx.Energy secm (-1) 3
        dtime0 = Idx.DTime rec secm
        ts = [give storage0, give dtime0] ++ ts'
        sqEnvs' = sqEnvs { dtimeMap = M.insert (Idx.DTime rec secm) (S.fromList [1.0]) (dtimeMap sqEnvs),
                           energyMap = M.insert storage0 (S.fromList [3.0]) (energyMap sqEnvs) }

        -- rearrange equations
        gd = map (eqToInTerm sqEnvs') $ toAbsEquations $ order ts


-- | D. Function to generate Variation Matrix
genVariationMatrix :: [a] -> [b] -> ([[a]], [[b]])
genVariationMatrix xs ys =
   (Match.replicate ys xs, map (Match.replicate xs) ys)


-- | Safe Lookup Functions
getVarEnergy :: [[Envs SingleRecord UTFSig]] -> Idx.Energy -> Test2 (Typ A F Tt) Val
getVarEnergy varEnvs idx = S.changeSignalType $ S.fromCells $ map (map f ) varEnvs
  where f ::  Envs SingleRecord UTFSig -> FSamp
        f envs = S.changeType $ S.head $ safeLookup (energyMap envs) idx

-- | Safe Lookup Functions
getVarPower :: [[Envs SingleRecord UTFSig]] -> Idx.Power -> Test2 (Typ A P Tt) Val
getVarPower varEnvs idx = S.changeSignalType $ S.fromCells $ map (map f ) varEnvs
  where f ::  Envs SingleRecord UTFSig -> PFSamp
        f envs = S.changeType $ S.head $ safeLookup (powerMap envs) idx


-- | Main Function ==================================================================

main :: IO ()
main = do
  let

    -- | A. -- Define Grid and solve system ------------------------------------

    -- define Variation
    yIndir = [0,0.2 .. 0.9] :: [Val]
    etas = [0.6,0.7 .. 1] :: [Val]

    -- generate Variation Grid
    (varY,varN) = genVariationMatrix yIndir etas

    -- solve System over Variation Grid
    varEnvs = zipWith (zipWith (solve3Way sqTopo)) varY varN :: [[Envs SingleRecord UTFSig]]

    -- | B. -- Extract Values for further calulations

    -- result of energies out
    storagePerc = S.fromList2 varY :: Test2 (Typ A Y Tt) Val
    storageEfficiency = S.fromList2 varN :: Test2 (Typ A N Tt) Val

    -- consumer
{-
    powerConsumptionS0 = getVarPower varEnvs (edgeIdx Idx.Power sec0 2 1)
    powerConsumptionS1 = getVarPower varEnvs (edgeIdx Idx.Power sec1 2 1)
-}
    energyConsumption =
       getVarEnergy varEnvs (edgeIdx Idx.Energy sec0 2 1) .+
       makeDelta (getVarEnergy varEnvs (edgeIdx Idx.Energy sec1 2 1))

    -- internal power (Between System 1 and 2)
    powerInt = getVarPower varEnvs (edgeIdx Idx.Power sec0 1 0)
    energyInt = (getVarEnergy varEnvs (edgeIdx Idx.Energy sec0 1 0)) -- .+  (makeDelta $ getVarEnergy varEnvs (edgeIdx Idx.Energy sec1 1 0))

    -- energy source
    powerSource = getVarPower varEnvs (edgeIdx Idx.Power sec0 0 1)
    energySource = getVarEnergy varEnvs  (edgeIdx Idx.Energy sec0 0 1)

    -- | C. -- Calculate Additional Values

    -- system efficiencie
    etaSYS =  energyConsumption./energySource
    etaSYS1 =  energyInt./energySource
    etaSYS2 =  energyConsumption./energyInt

    -- eta1 efficiency check
    pOne = toScalar 1 :: TC Scalar (Typ A P Tt) (Data Nil Val)
    etaOne =  toScalar 1 :: TC Scalar (Typ A N Tt) (Data Nil Val)
    etaSYS1_Check = makeAbsolute $ etaOne .- (pOne ./ (makeDelta pOne .+ powerSource)) ::  Test2 (Typ A N Tt) Val
    check = etaSYS1_Check .- etaSYS1

    -- calculating system losses
    lossSYS = energySource .- energyConsumption
    lossSYS1 = energySource .- energyInt
    lossSYS2 = energyInt .- energyConsumption



  -- | D. -- Selected Energy Flow Plots

--  drawTopologyX' sqTopo
  drawTopology sqTopo $ head $ head varEnvs
  drawTopology sqTopo $ last $ head varEnvs
  drawTopology sqTopo $ head $ last varEnvs
  drawTopology sqTopo $ last $ last varEnvs

  -- | E. -- Reporting the Numbers

  putStrLn "Efficiency Eta1 and Check"
  report [] ("etaSYS1", etaSYS1)
  report [] ("etaSYS1_check", etaSYS1_Check)
  report [] ("Check", check)

  putStrLn "Consumer"
  report [] ("energyConsumption",energyConsumption)
  report [] ("energyInt",energyInt)
  report [] ("energySource",energySource)

  putStrLn "Consumer"
  report [] ("powerInt", powerInt)
  report [] ("powerSource", powerSource)

  report [] ("System Efficiency",  etaSYS)
  report [] ("System1 Efficiency", etaSYS1)
  report [] ("System2 Efficiency", etaSYS2)

  -- | F. -- Surface & Trend Plots

  -- additional dataFormat for trend plots
{-
  let lossListSYS1 = toSigList lossSYS1
      lossListSYS2 = toSigList lossSYS2
      lossListSYS = toSigList lossSYS
-}
--  sigPlots [head lossListSYS1,  head lossListSYS2]
--  sigPlots [last lossListSYS1,  last lossListSYS2]


  let etaListSYS = toSigList etaSYS
{-
      etaListSYS1 = toSigList etaSYS1
      powerListSource = toSigList powerSource
      powerListInt = toSigList powerInt
-}

  xyplot "Efficiency System1" (S.fromList yIndir :: Test1 (Typ A Y Tt) Val) etaSYS1
  xyplot "InternalPower" (S.fromList yIndir :: Test1 (Typ A Y Tt) Val) powerInt

  xyplot "SystemLoss" (S.fromList yIndir :: Test1 (Typ A Y Tt) Val) lossSYS

  -- xyplots "SystemLoss1 & SystemLoss2"(S.fromList yIndir :: Test1 (Typ A Y Tt) Val) (lossListSYS1 ++ (map S.reverse lossListSYS2))
  xyplot "Loss System1 & Loss System2"(S.fromList yIndir :: Test1 (Typ A Y Tt) Val) (lossSYS1 .++ lossSYS2)

--  surfPlot "System Loss" storagePerc storageEfficiency lossSYS
  surfPlot "System1 Loss" storagePerc storageEfficiency  lossSYS1

  surfPlot "System2 Loss" storagePerc storageEfficiency  lossSYS2

  xyplot "System Efficiency" (S.fromList yIndir :: Test1 (Typ A Y Tt) Val) etaListSYS
--  xyplots2 powerListInt etaListSYS


  surfPlot "System Efficiency" storagePerc storageEfficiency etaSYS

  surfPlot "System1 Efficiency" storagePerc storageEfficiency etaSYS1

  surfPlot "System2 Efficiency" storagePerc storageEfficiency etaSYS2

  surfPlot "Flow Share over Storage" storagePerc storageEfficiency storagePerc

  surfPlot "Energy Consumption" storagePerc storageEfficiency energyConsumption

--  surfPlot "powerConsumptionS1" storagePerc storageEfficiency powerConsumptionS1
--  surfPlot "powerConsumptionS0" storagePerc storageEfficiency powerConsumptionS0
