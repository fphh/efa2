module EFA2.Topology.Topology where

import EFA2.Solver.Equation
          (Equation(..), Term(..),
           MkIdxC, MkVarC, mkVar, mkTerm, add, give, (!=), (&-), (&/))
import qualified EFA2.Interpreter.Env as Env
import EFA2.Interpreter.Env
          (Envs(Envs), recordNumber, fromSingleRecord,
           MixedRecord(MixedRecord), SingleRecord(SingleRecord),
           setIdxRecNum)
import EFA2.Topology.TopologyData as Topo
import EFA2.Utils.Utils (mapFromSet)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.EfaGraph as Gr
import EFA2.Signal.Index (Use(InSum, OutSum))
import EFA2.Topology.EfaGraph
          (Edge(Edge), mapGraph,
           lab, labEdges, edgeLabels, elfilter)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Foldable as Fold
import qualified Data.List.HT as LH
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Tuple.HT (mapSnd)
import Data.Maybe (fromJust, mapMaybe)


-----------------------------------------------------------------------------------
-- Topology Graph
-- | This is the main topology graph representation.

makeNodes :: [(Int, nt)] -> [Gr.LNode Idx.Node nt]
makeNodes ns = map f ns
  where f (n, ty) = (Idx.Node n, ty)

makeEdges :: [(Int, Int, el)] -> [Gr.LEdge Idx.Node el]
makeEdges es = map f es
  where f (a, b, l) = (Edge (Idx.Node a) (Idx.Node b), l)

makeWithDirEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node FlowDirection]
makeWithDirEdges es = map f es
  where f (a, b) = (Edge (Idx.Node a) (Idx.Node b), WithDir)

makeSimpleEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node ()]
makeSimpleEdges es = map f es
  where f (a, b) = (Edge (Idx.Node a) (Idx.Node b), ())


------------------------------------------------------------------------
-- Making equations:

-- TODO: Check that envUnion preserves all variables.
makeAllEquations ::
   (Show a) =>
   SequFlowGraph -> [Envs SingleRecord a] -> (Envs MixedRecord a, [Equation])
makeAllEquations topo envs =
   ((Env.envUnion envs') { recordNumber = MixedRecord newRecNums }, ts)
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
        envs' = map setRecordIndices envs
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

setRecordIndices :: (Show a) => Envs SingleRecord a -> Envs SingleRecord a
setRecordIndices (Envs (SingleRecord rec) e de p dp fn dn dt x dx v st) =
  Envs (SingleRecord rec) e' de' p' dp' fn' dn' dt' x' dx' v' st'
  where e'  = M.mapKeys (setIdxRecNum rec) e
        de' = M.mapKeys (setIdxRecNum rec) de
        p'  = M.mapKeys (setIdxRecNum rec) p
        dp' = M.mapKeys (setIdxRecNum rec) dp
        fn' = M.mapKeys (setIdxRecNum rec) fn
        dn' = M.mapKeys (setIdxRecNum rec) dn
        x'  = M.mapKeys (setIdxRecNum rec) x
        dx' = M.mapKeys (setIdxRecNum rec) dx
        v'  = M.mapKeys (setIdxRecNum rec) v
        st' = M.mapKeys (setIdxRecNum rec) st
        dt' = M.mapKeys (setIdxRecNum rec) dt

envToEqTerms :: (MkIdxC k) => M.Map k v -> [Equation]
envToEqTerms m = map (give . fst) (M.toList m)

mkPowerEqs :: Idx.Record -> SequFlowGraph -> [Equation]
mkPowerEqs rec topo = concat $ mapGraph (mkPEqs rec) topo

mkPEqs :: Idx.Record -> Gr.InOut Idx.SecNode NodeType el -> [Equation]
mkPEqs rec (ins, (nid@(Idx.SecNode sec _), _), outs) = ieqs ++ oeqs -- ++ dieqs ++ doeqs
  where dt = Atom $ Env.DTime $ Idx.DTime rec sec
        eis = map (makeVar rec Idx.Energy nid) ins
        eos = map (makeVar rec Idx.Energy nid) outs
        --deis = map (makeVar rec Idx.DEnergy nid) ins
        --deos = map (makeVar rec Idx.DEnergy nid) outs
        pis = map (makeVar rec Idx.Power nid) ins
        pos = map (makeVar rec Idx.Power nid) outs
        --dpis = map (makeVar rec Idx.DPower nid) ins
        --dpos = map (makeVar rec Idx.DPower nid) outs

        ieqs = zipWith f eis pis
        oeqs = zipWith f eos pos
        --dieqs = zipWith f deis dpis
        --doeqs = zipWith f deos dpos
        f e p = e := p :* dt


mkIntersectionEqs :: Idx.Record -> SequFlowGraph -> [Equation]
mkIntersectionEqs recordNum topo =
   Fold.fold inEqs ++ Fold.fold outEqs ++ stContentEqs
  where inouts =
           fmap (mapSnd partitionInOutStatic) $
           getActiveStores topo
        inEqs =
           Fold.foldMap
              (\(n, (ins,_)) ->
                 M.mapWithKey (\sec ->
                    mkInStoreEqs recordNum (Idx.SecNode sec n)) ins)
           inouts
        outEqs =
           Fold.foldMap
              (\(n, (_,outs)) ->
                 M.mapWithKey (\sec ->
                    mkOutStoreEqs recordNum (Idx.SecNode sec n)) outs)
           inouts
        stContentEqs =
           Fold.concat $ M.mapWithKey (mkStoreEqs recordNum) inouts


data StoreDir = In | Out deriving (Show)

mkStoreEqs ::
   Idx.Record ->
   Idx.Store ->
   (Idx.Node,
    (M.Map Idx.Section (Topo.InOut Idx.SecNode el),
     M.Map Idx.Section (Topo.InOut Idx.SecNode el))) ->
   [Equation]
mkStoreEqs recordNum st (node, (ins, outs)) =
      startEq ++ LH.mapAdjacent g both
  where ins' = fmap (const In) ins
        outs' = fmap (const Out) outs
        both@(b:_) = M.toList $ M.union ins' outs'

        startEq =
           case b of
              (sec, In) ->
                 [ mkVar (Idx.Storage recordNum sec st) :=
                      mkVar (Idx.Var recordNum InSum $ Idx.SecNode sec node) :*
                      mkVar (Idx.DTime recordNum sec) ]
              _ -> []

        g (sec, _) (sec', dir) =
           stnew := (case dir of In -> vdt; Out -> Minus vdt) :+ stold
{-
          mkVar (Idx.Storage recordNum st') :=
            (mkVar (Idx.Var recordNum InSum nid') :* dt)
              :+ mkVar (Idx.Storage recordNum sec st)
-}
          where stnew = mkVar $ Idx.Storage recordNum sec' st
                stold = mkVar $ Idx.Storage recordNum sec st
                vdt = v :* dt
                v = mkVar $ Idx.Var recordNum (case dir of In -> InSum; Out -> OutSum) $ Idx.SecNode sec' node
                dt = mkVar $ Idx.DTime recordNum sec'


mkInStoreEqs ::
   Idx.Record -> Idx.SecNode -> Topo.InOut Idx.SecNode nl -> [Equation]
mkInStoreEqs recordNum n (_ins, outs@((o,_) : _)) = startEq:osEqs
  where startEq = mkVar (Idx.Var recordNum InSum n) := mkVar (Idx.Power recordNum n o)
        osEqs = LH.mapAdjacent f $ map fst outs
        f x y =
           mkVar (Idx.Power recordNum n y) :=
              mkVar (Idx.Power recordNum n x) &-
              mkVar (Idx.Power recordNum x n)
mkInStoreEqs _ _ _ = []


mkOutStoreEqs :: Idx.Record -> Idx.SecNode -> Topo.InOut Idx.SecNode nl -> [Equation]
mkOutStoreEqs recordNum n (ins, _ : _) =
     visumeqs ++ xeqs ++ pieqs
  where xis = map (makeVar' Idx.X) ins
        --eis = map (makeVar' Idx.Energy) ins
        pis = map (makeVar' Idx.Power) ins
        makeVar' mkIdx (nid', _) =
           mkVar $ mkIdx recordNum nid' n

        --visum = mkVar (Idx.Var recordNum St n)
        visum = mkVar (Idx.Var recordNum InSum n)

        visumeqs =
           case NonEmpty.fetch pis of
              Just pisne -> [visum := add pisne]
              -- How to cope with empty lists?
              -- Nothing -> []

        xeqs = zipWith g xis pis
        g x e = x := e &/ visum

        pis' = map (makeVar recordNum Idx.Power n) ins

        outv = mkVar $ Idx.Var recordNum OutSum n
        pieqs = zipWith h pis' xis
        h e x = e := x :* outv

mkOutStoreEqs _ _ _ = []

{-
-- | Takes section, record, and a graph.
mkEdgeEq :: Int -> SequFlowGraph -> [Equation]
mkEdgeEq recordNum topo = map f (map unlabelEdge origEs)
  where origEs = L.filter (\(_, _, l) -> not $ isIntersectionEdge l) (labEdges topo)
        f (x, y) = mkVar (Idx.Power ys recordNum y x) :=
                         (mkVar (Idx.Power xs recordNum x y)) :* (mkVar (Idx.FEta xs recordNum x y))
          where NLabel xs _ _ = fromJust $ lab topo x
                NLabel ys _ _ = fromJust $ lab topo y
-}
-- | Takes section, record, and a graph.
mkEdgeEq :: Idx.Record -> SequFlowGraph -> [Equation]
mkEdgeEq recordNum =
   map f . M.keys .
   M.filter (not . isIntersectionEdge) . edgeLabels
  where f (Edge x y) =
           EqEdge
              (mkVar $ Idx.Power recordNum x y)
              (mkVar $ Idx.FEta recordNum x y)
              (mkVar $ Idx.Power recordNum y x)


mkNodeEq :: Idx.Record -> SequFlowGraph -> [Equation]
mkNodeEq recordNum topo = concat $ mapGraph (mkEq recordNum) (elfilter cond topo)
  where cond x = isOriginalEdge x || isInnerStorageEdge x

mkEq :: Idx.Record -> Gr.InOut Idx.SecNode NodeType el -> [Equation]
mkEq recordNum (ins, (nid, _), outs) =
   case (fmap (makeOuts vosum . makeVars) $ NonEmpty.fetch ins,
         fmap (makeIns  visum . makeVars) $ NonEmpty.fetch outs) of
      (Nothing, Nothing) -> []
      (Nothing, Just (_, oeqss)) -> oeqss
      (Just (_, ieqss), Nothing) -> ieqss
      (Just (ieqs, ieqss), Just (oeqs, oeqss)) ->
         oeqss ++ ieqss ++ oeqs ++ ieqs ++ [vosum != visum]

  where -- ATTENTION (not very safe): We need this variable in mkInStoreEq again!!!
        visum = mkVar $ Idx.Var recordNum InSum nid
        -- ATTENTION (not very safe): We need this variable in mkOutStoreEq again!!!
        vosum = mkVar $ Idx.Var recordNum OutSum nid

        -- For section and record, we focus on the current node nid.
        makeVars xs =
           (fmap (makeVar recordNum Idx.X nid) xs,
            fmap (makeVar recordNum Idx.Power nid) xs)

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
        --g v x e = x := e &/ v
        g v x e = e := v :* x
-}


mkAllDiffEqs :: Idx.Record -> Idx.Record -> SequFlowGraph -> [Equation]
mkAllDiffEqs laterRec formerRec topo = {- edgeEqs ++ -} nodeEqs ++ etaEqs ++ xEqs
  where -- edgeEqs = concat $ mapGraph (mkDiffPowerEqs laterRec formerRec) topo
        nodeEqs = concat $ mapGraph (mkDiffNodeEqs laterRec formerRec) topo
        etaEqs = concat $ mapGraph (mkDiffEtaEqs laterRec formerRec) topo
        xEqs = concat $ mapGraph (mkDiffXEqs laterRec formerRec) topo

{-
mkDiffPowerEqs ::
   Idx.Record -> Idx.Record ->
   Gr.InOut Idx.SecNode NodeType el ->
   [Equation]
mkDiffPowerEqs laterRec formerRec (ins, n@(nid, _), outs)
  | length ins == 0 && length outs == 0 = []
  | length ins == 0 && length outs > 0 = doeqs
  | length ins > 0 && length outs == 0 = dieqs
  | otherwise = dieqs ++ doeqs
  where lpis = map (makeVar laterRec Idx.Power nid) ins
        fpis = map (makeVar formerRec Idx.Power nid) ins
        dieqs = zipWith3 f lpis fpis ins

        lpos = map (makeVar laterRec Idx.Power nid) outs
        fpos = map (makeVar formerRec Idx.Power nid) outs
        doeqs = zipWith3 f lpos fpos outs
        f x y i = (makeVar laterRec Idx.DPower nid i) := x &- y
-}

mkDiffEtaEqs ::
   Idx.Record -> Idx.Record ->
   Gr.InOut Idx.SecNode NodeType el ->
   [Equation]
mkDiffEtaEqs laterRec formerRec (_ins, (nid, _), outs) = dnoeqs
  where lnos = map (makeVar laterRec Idx.FEta nid) outs
        fnos = map (makeVar formerRec Idx.FEta nid) outs
        dnoeqs = zipWith3 g lnos fnos outs
        g x y i = (makeVar laterRec Idx.DEta nid i)  :=  x &- y

mkDiffXEqs ::
   Idx.Record -> Idx.Record ->
   Gr.InOut Idx.SecNode NodeType el ->
   [Equation]
mkDiffXEqs laterRec formerRec (ins, (nid, _), outs) = xiseq ++ xoseq
  where f dx lx fx = dx := lx &- fx

        lxis = map (makeVar laterRec Idx.X nid) ins
        fxis = map (makeVar formerRec Idx.X nid) ins
        dxis = map (makeVar laterRec Idx.DX nid) ins
        xiseq = zipWith3 f dxis lxis fxis

        lxos = map (makeVar laterRec Idx.X nid) outs
        fxos = map (makeVar formerRec Idx.X nid) outs
        dxos = map (makeVar laterRec Idx.DX nid) outs
        xoseq = zipWith3 f dxos lxos fxos


mkDiffNodeEqs ::
   Idx.Record -> Idx.Record ->
   Gr.InOut Idx.SecNode NodeType el ->
   [Equation]
mkDiffNodeEqs laterRec _formerRec (ins0, (nid, _), outs0) =
   case (NonEmpty.fetch ins0, NonEmpty.fetch outs0) of
      (Just ins, Just outs) ->
         let dleis = fmap (makeVar laterRec Idx.DPower nid) ins
             dleos = fmap (makeVar laterRec Idx.DPower nid) outs
             _sumeq = [add dleis := add dleos]
         in  []
      _ -> []


makeVar ::
   (MkVarC b, MkIdxC a) =>
   rec ->
   (rec -> Idx.SecNode -> Idx.SecNode -> a) ->
   Idx.SecNode -> (Idx.SecNode, t) -> b
makeVar r mkIdx nid (nid', _) =
   mkVar $ mkIdx r nid nid'


-- | We sort in and out going edges according to 'FlowDirection'.
-- Undirected edges are filtered away.
-- This is important for creating correct equations.
makeDirTopology :: SequFlowGraph -> SequFlowGraph
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
