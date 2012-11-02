module EFA2.Topology.Topology where

import EFA2.Solver.Equation
          (Equation(..), Term(..),
           MkIdxC, MkVarC, mkVar, mkTerm, add, give, (!=))
import qualified EFA2.Interpreter.Env as Env
import EFA2.Interpreter.Env
          (Envs(Envs), fromSingleRecord, isSingleRecord, recordNumber,
           RecordNumber(MixedRecord, SingleRecord))
import EFA2.Topology.TopologyData
import EFA2.Utils.Utils (pairs, safeLookup, mapFromSet)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.EfaGraph as Gr
import EFA2.Signal.Index (Use(InSum, OutSum))
import Data.Graph.Inductive (Node)
import EFA2.Topology.EfaGraph
          (Edge(Edge), InOutGraphFormat, mapGraph,
           lab, labNodes, labEdges, edgeLabels, elfilter)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as LH
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Tuple.HT (snd3)
import Data.Maybe (fromJust, mapMaybe)
import Data.Ord (comparing)


-----------------------------------------------------------------------------------
-- Topology Graph
-- | This is the main topology graph representation.

makeNodes :: [(Int, NodeType)] -> [LNode]
makeNodes ns = map f ns
  where f (n, ty) = (n, NLabel (Idx.Section 0) n ty)

makeEdges :: [(Int, Int, ELabel)] -> [LEdge]
makeEdges es = map f es
  where f (a, b, l) = (Edge a b, l)

makeWithDirEdges :: [(Int, Int)] -> [LEdge]
makeWithDirEdges es = map f es
  where f (a, b) = (Edge a b, defaultELabel)



------------------------------------------------------------------------
-- Making equations:

missingRecordNumbers :: [Envs a] -> Bool
missingRecordNumbers envs =
   not $ all (isSingleRecord . recordNumber) envs


-- TODO: Check that envUnion preserves all variables.
makeAllEquations :: (Show a) => Topology -> [Envs a] -> (Envs a, [Equation])
makeAllEquations topo envs =
   if missingRecordNumbers envs
     then error ("makeAllEquations: only single records are allowed in " ++ show envs)
     else ((Env.envUnion envs') { recordNumber = MixedRecord newRecNums }, ts)
  where newRecNums = map (fromSingleRecord . recordNumber) envs
        recNum env = fromSingleRecord $ recordNumber env
        dirTopo = makeDirTopology topo

        ts = edgeEqs ++ nodeEqs ++ interEqs ++ powerEqs ++ envEqs

        edgeEqs = concatMap mkEdEq envs
        mkEdEq env = mkEdgeEq (recNum env) dirTopo

        nodeEqs = concatMap mkNdEq envs
        mkNdEq env = mkNodeEq (recNum env) dirTopo

        interEqs = concatMap mkInterEqs envs
        mkInterEqs env = mkIntersectionEqs (recNum env) dirTopo

        powerEqs = concatMap mkPowEqs envs
        mkPowEqs env = mkPowerEqs (recNum env) dirTopo

        envEqs = concatMap f envs'
        m = M.fromList $ map g (labNodes topo)
        g (nid, NLabel s oldNid _) = ((s, oldNid), nid)
        envs' = map (shiftIndices m) envs
        f (Envs _ e de p dp fn dn t x dx v st) = envToEqTerms e
                                                 ++ envToEqTerms de
                                                 ++ envToEqTerms p
                                                 ++ envToEqTerms dp
                                                 ++ envToEqTerms fn
                                                 ++ envToEqTerms dn
                                                 ++ envToEqTerms t
                                                 ++ envToEqTerms x
                                                 ++ envToEqTerms dx
                                                 ++ envToEqTerms v
                                                 ++ envToEqTerms st

-- TODO: use patternmatching instead of safeLookup
shiftIndices :: (Show a) => M.Map (Idx.Section, Int) Node -> Envs a -> Envs a
shiftIndices m (Envs (SingleRecord rec) e de p dp fn dn t' x dx v st) =
  Envs (SingleRecord rec) e' de' p' dp' fn' dn' t' x' dx' v' st'
  where e' = M.mapKeys ef e
        ef (Idx.Energy s _ f t) = Idx.Energy s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        de' = M.mapKeys def de
        def (Idx.DEnergy s _ f t) = Idx.DEnergy s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        p' = M.mapKeys pf p
        pf (Idx.Power s _ f t) = Idx.Power s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        dp' = M.mapKeys dpf dp
        dpf (Idx.DPower s _ f t) = Idx.DPower s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        fn' = M.mapKeys fnf fn
        fnf (Idx.FEta s _ f t) = Idx.FEta s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        dn' = M.mapKeys dnf dn
        dnf (Idx.DEta s _ f t) = Idx.DEta s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        x' = M.mapKeys xf x
        xf (Idx.X s _ f t) = Idx.X s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        dx' = M.mapKeys dxf dx
        dxf (Idx.DX s _ f t) = Idx.DX s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        v' = M.mapKeys vf v
        --vf (Idx.Var s _ f t) = Idx.Var s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))
        vf (Idx.Var s _ use t) = Idx.Var s rec use (m `safeLookup` (s, t))

        st' = M.mapKeys stf st
        stf (Idx.Storage s _ sto) = Idx.Storage s rec (m `safeLookup` (s, sto))


envToEqTerms :: (MkIdxC k) => M.Map k v -> [Equation]
envToEqTerms m = map (give . fst) (M.toList m)

mkPowerEqs :: Idx.Record -> Topology -> [Equation]
mkPowerEqs rec topo = concat $ mapGraph (mkPEqs rec) topo

mkPEqs :: Idx.Record -> ([LNode], LNode, [LNode]) -> [Equation]
mkPEqs rec (ins, (nid, NLabel sec _ _), outs) = ieqs ++ oeqs -- ++ dieqs ++ doeqs
  where dt = Atom $ Env.DTime $ Idx.DTime sec rec
        eis = map (makeVar rec sec Idx.Energy nid) ins
        eos = map (makeVar rec sec Idx.Energy nid) outs
        --deis = map (makeVar rec sec Idx.DEnergy nid) ins
        --deos = map (makeVar rec sec Idx.DEnergy nid) outs
        pis = map (makeVar rec sec Idx.Power nid) ins
        pos = map (makeVar rec sec Idx.Power nid) outs
        --dpis = map (makeVar rec sec Idx.DPower nid) ins
        --dpos = map (makeVar rec sec Idx.DPower nid) outs

        ieqs = zipWith f eis pis
        oeqs = zipWith f eos pos
        --dieqs = zipWith f deis dpis
        --doeqs = zipWith f deos dpos
        f e p = e := p :* dt


mkIntersectionEqs :: Idx.Record -> Topology -> [Equation]
mkIntersectionEqs recordNum topo = concat inEqs ++ concat outEqs ++ stContentEqs
  where actStores = getActiveStores topo
        inouts = map (partitionInOutStatic topo) actStores
        (ins, outs) = unzip inouts
        inEqs = concatMap (map (mkInStoreEqs recordNum)) ins
        outEqs = concatMap (map (mkOutStoreEqs recordNum)) outs
        stContentEqs = concatMap (mkStoreEqs recordNum) inouts

data IOStore = Store StoreDir LNode deriving (Show)

data StoreDir = In | Out deriving (Show)

mkStoreEqs ::
   Idx.Record ->
   ([InOutGraphFormat LNode], [InOutGraphFormat LNode]) ->
   [Equation]
mkStoreEqs recordNum (ins, outs) = startEq ++ eqs
  where ins' = map (Store In . snd3) ins
        outs' = map (Store Out . snd3) outs
        both@(b:_) = L.sortBy (comparing f) $ ins' ++ outs'

        f (Store _ (_, l)) = sectionNLabel l
        startEq = k b
        k (Store In (nid, NLabel sec _ (InitStorage st))) =
          [ mkVar (Idx.Storage sec recordNum st) := mkVar (Idx.Var sec recordNum InSum nid) :* dt]
          where dt = mkVar $ Idx.DTime sec recordNum
        k _ = []
        eqs = map (g . h) (pairs both)
        h (Store _ x, y) = (x, y)

        g ((_nid, NLabel sec _ st), Store dir (nid', NLabel sec' _ st')) =
           stnew := (case dir of In -> vdt; Out -> Minus vdt) :+ stold
{-
          mkVar (Idx.Storage sec' recordNum (getStorageNumber st')) :=
            (mkVar (Idx.Var sec' recordNum InSum nid') :* dt)
              :+ mkVar (Idx.Storage sec recordNum (getStorageNumber st))
-}
          where stnew = mkVar $ Idx.Storage sec' recordNum (getStorageNumber st')
                stold = mkVar $ Idx.Storage sec recordNum (getStorageNumber st)
                vdt = v :* dt
                v = mkVar $ Idx.Var sec' recordNum (case dir of In -> InSum; Out -> OutSum) nid'
                dt = mkVar $ Idx.DTime sec' recordNum


mkInStoreEqs :: Idx.Record -> InOutGraphFormat LNode -> [Equation]
mkInStoreEqs recordNum (_ins, (nid, NLabel sec _ _), outs@((o,_):_)) = (startEq:osEqs)
  where startEq = mkVar (Idx.Var sec recordNum InSum nid) := mkVar (Idx.Power sec recordNum nid o)
        osEqs = LH.mapAdjacent f outs
        f x y =
           mkVar (Idx.Power sec recordNum nid y') :=
              mkVar (Idx.Power sec recordNum nid x') :+
              Minus (mkVar (Idx.Power xs recordNum x' nid))
          where (x', NLabel xs _ _) = x
                (y', _) = y
mkInStoreEqs _ _ = []


mkOutStoreEqs :: Idx.Record -> InOutGraphFormat LNode -> [Equation]
mkOutStoreEqs recordNum (ins, (nid, NLabel sec _ _), _:_) =
     visumeqs ++ xeqs ++ pieqs
  where xis = map (makeVar' Idx.X) ins
        --eis = map (makeVar' Idx.Energy) ins
        pis = map (makeVar' Idx.Power) ins
        makeVar' mkIdx (nid', l) =
           mkVar $ mkIdx (sectionNLabel l) recordNum nid' nid

        --visum = mkVar (Idx.Var sec recordNum St nid)
        visum = mkVar (Idx.Var sec recordNum InSum nid)

        visumeqs =
           case NonEmpty.fetch pis of
              Just pisne -> [visum := add pisne]
              -- How to cope with empty lists?
              -- Nothing -> []

        xeqs = zipWith g xis pis
        g x e = x := e :* Recip visum

        pis' = map (makeVar recordNum sec Idx.Power nid) ins

        outv = mkVar $ Idx.Var sec recordNum OutSum nid
        pieqs = zipWith h pis' xis
        h e x = e := x :* outv

mkOutStoreEqs _ _ = []

{-
-- | Takes section, record, and a graph.
mkEdgeEq :: Int -> Topology -> [Equation]
mkEdgeEq recordNum topo = map f (map unlabelEdge origEs)
  where origEs = L.filter (\(_, _, l) -> not $ isIntersectionEdge l) (labEdges topo)
        f (x, y) = mkVar (Idx.Power ys recordNum y x) :=
                         (mkVar (Idx.Power xs recordNum x y)) :* (mkVar (Idx.FEta xs recordNum x y))
          where NLabel xs _ _ = fromJust $ lab topo x
                NLabel ys _ _ = fromJust $ lab topo y
-}
-- | Takes section, record, and a graph.
mkEdgeEq :: Idx.Record -> Topology -> [Equation]
mkEdgeEq recordNum topo =
   map f $ M.keys $
   M.filter (not . isIntersectionEdge) $ edgeLabels topo
  where f (Edge x y) =
           EqEdge
              (mkVar $ Idx.Power xs recordNum x y)
              (mkVar $ Idx.FEta xs recordNum x y)
              (mkVar $ Idx.Power ys recordNum y x)
          where NLabel xs _ _ = fromJust $ lab topo x
                NLabel ys _ _ = fromJust $ lab topo y


mkNodeEq :: Idx.Record -> Topology -> [Equation]
mkNodeEq recordNum topo = concat $ mapGraph (mkEq recordNum) (elfilter cond topo)
  where cond x = isOriginalEdge x || isInnerStorageEdge x

mkEq :: Idx.Record -> InOutGraphFormat LNode -> [Equation]
mkEq recordNum (ins, (nid, NLabel sec _ _), outs) =
   case (fmap (makeOuts vosum . makeVars) $ NonEmpty.fetch ins,
         fmap (makeIns  visum . makeVars) $ NonEmpty.fetch outs) of
      (Nothing, Nothing) -> []
      (Nothing, Just (_, oeqss)) -> oeqss
      (Just (_, ieqss), Nothing) -> ieqss
      (Just (ieqs, ieqss), Just (oeqs, oeqss)) ->
         oeqss ++ ieqss ++ oeqs ++ ieqs ++ [vosum != visum]

  where -- ATTENTION (not very safe): We need this variable in mkInStoreEq again!!!
        visum = mkVar $ Idx.Var sec recordNum InSum nid
        -- ATTENTION (not very safe): We need this variable in mkOutStoreEq again!!!
        vosum = mkVar $ Idx.Var sec recordNum OutSum nid

        -- For section and record, we focus on the current node n.
        makeVars xs =
           (fmap (makeVar recordNum sec Idx.X nid) xs,
            fmap (makeVar recordNum sec Idx.Power nid) xs)

makeIns, makeOuts ::
   Env.Index ->
   (NonEmpty.T [] Env.Index, NonEmpty.T [] Env.Index) ->
   ([Equation], [Equation])

makeIns visum (xis, pis) =
   let  visumeq = [visum != add (fmap mkTerm pis)]

        ieqs = NonEmpty.zipWith (EqEdge visum) xis pis

        ieqs' = []

   in   (NonEmpty.flatten ieqs, (Const 1.0 := add (fmap mkTerm xis)) : ieqs' ++ visumeq)

makeOuts vosum (xos, pos) =
   let  vosumeq = [vosum != add (fmap mkTerm pos)]

        oeqs = NonEmpty.zipWith (EqEdge vosum) xos pos

        oeqs' = []

   in   (NonEmpty.flatten oeqs, (Const 1.0 := add (fmap mkTerm xos)) : oeqs' ++ vosumeq)

{-
        ieqs' | length pis > 1 = [] -- zipWith (g visum) xis pis
              | otherwise = []
        oeqs' | length pos > 1 = [] -- zipWith (g vosum) xos pos
              | otherwise = []
        --g v x e = x := e :* Recip v
        g v x e = e := v :* x
-}


mkAllDiffEqs :: Idx.Record -> Idx.Record -> Topology -> [Equation]
mkAllDiffEqs laterRec formerRec topo = {- edgeEqs ++ -} nodeEqs ++ etaEqs ++ xEqs
  where -- edgeEqs = concat $ mapGraph (mkDiffPowerEqs laterRec formerRec) topo
        nodeEqs = concat $ mapGraph (mkDiffNodeEqs laterRec formerRec) topo
        etaEqs = concat $ mapGraph (mkDiffEtaEqs laterRec formerRec) topo
        xEqs = concat $ mapGraph (mkDiffXEqs laterRec formerRec) topo

{-
mkDiffPowerEqs ::
   Idx.Record -> Idx.Record ->
   ([LNode], LNode, [LNode]) ->
   [Equation]
mkDiffPowerEqs laterRec formerRec (ins, n@(nid, NLabel sec _ _), outs)
  | length ins == 0 && length outs == 0 = []
  | length ins == 0 && length outs > 0 = doeqs
  | length ins > 0 && length outs == 0 = dieqs
  | otherwise = dieqs ++ doeqs
  where lpis = map (makeVar laterRec sec Idx.Power nid) ins
        fpis = map (makeVar formerRec sec Idx.Power nid) ins
        dieqs = zipWith3 f lpis fpis ins

        lpos = map (makeVar laterRec sec Idx.Power nid) outs
        fpos = map (makeVar formerRec sec Idx.Power nid) outs
        doeqs = zipWith3 f lpos fpos outs
        f x y i = (makeVar laterRec sec Idx.DPower nid i) := x :+ (Minus y)
-}

mkDiffEtaEqs ::
   Idx.Record -> Idx.Record ->
   ([LNode], LNode, [LNode]) ->
   [Equation]
mkDiffEtaEqs laterRec formerRec (_ins, (nid, NLabel sec _ _), outs) = dnoeqs
  where lnos = map (makeVar laterRec sec Idx.FEta nid) outs
        fnos = map (makeVar formerRec sec Idx.FEta nid) outs
        dnoeqs = zipWith3 g lnos fnos outs
        g x y i = (makeVar laterRec sec Idx.DEta nid i) := x :+ (Minus y)

mkDiffXEqs ::
   Idx.Record -> Idx.Record ->
   ([LNode], LNode, [LNode]) ->
   [Equation]
mkDiffXEqs laterRec formerRec (ins, (nid, NLabel sec _ _), outs) = xiseq ++ xoseq
  where f dx lx fx = dx := lx :+ (Minus fx)

        lxis = map (makeVar laterRec sec Idx.X nid) ins
        fxis = map (makeVar formerRec sec Idx.X nid) ins
        dxis = map (makeVar laterRec sec Idx.DX nid) ins
        xiseq = zipWith3 f dxis lxis fxis

        lxos = map (makeVar laterRec sec Idx.X nid) outs
        fxos = map (makeVar formerRec sec Idx.X nid) outs
        dxos = map (makeVar laterRec sec Idx.DX nid) outs
        xoseq = zipWith3 f dxos lxos fxos


mkDiffNodeEqs ::
   Idx.Record -> Idx.Record ->
   ([LNode], LNode, [LNode]) ->
   [Equation]
mkDiffNodeEqs laterRec _formerRec (ins0, (nid, NLabel sec _ _), outs0) =
   case (NonEmpty.fetch ins0, NonEmpty.fetch outs0) of
      (Just ins, Just outs) ->
         let dleis = fmap (makeVar laterRec sec Idx.DPower nid) ins
             dleos = fmap (makeVar laterRec sec Idx.DPower nid) outs
             _sumeq = [add dleis := add dleos]
         in  []
      _ -> []


makeVar ::
   (MkVarC b, MkIdxC a) =>
   rec -> sec ->
   (sec -> rec -> Int -> Int -> a) -> Int -> (Int, t) -> b
makeVar r sec mkIdx nid (nid', _) =
   mkVar $ mkIdx sec r nid nid'


-- | We sort in and out going edges according to 'FlowDirection'.
-- Undirected edges are filtered away.
-- This is important for creating correct equations.
makeDirTopology :: Topology -> Topology
makeDirTopology topo = Gr.mkGraphFromMap ns esm
  where esm = M.fromList $ mapMaybe flipAgainst $ labEdges topo
        flipAgainst e@(Edge x y, elabel) =
           case flowDirection elabel of
              UnDir -> Nothing
              AgainstDir -> Just (Edge y x, elabel { flowDirection = WithDir })
              WithDir -> Just e
        ns =
           mapFromSet (fromJust . lab topo) $ S.fromList $
           concatMap (\(Edge x y) -> [x, y]) $ M.keys esm
