module EFA2.Topology.Topology where

import EFA2.Solver.Equation
          (Equation(..), Term(..),
           MkIdxC, mkVar, mkTerm, add, give, (!=))
import EFA2.Interpreter.Env as Env
import EFA2.Topology.TopologyData
import EFA2.Utils.Graph
import EFA2.Utils.Utils (pairs, safeLookup)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.EfaGraph as Gr
import Data.Graph.Inductive
          (LNode, Node, LEdge, lab, labNodes, labEdges, elfilter)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.List as L
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import Data.Tuple.HT (snd3)
import Data.Maybe (fromJust)
import Data.Ord (comparing)


-----------------------------------------------------------------------------------
-- Topology Graph
-- | This is the main topology graph representation.

makeNodes :: [(Int, NodeType)] -> [LNode NLabel]
makeNodes ns = map f ns
  where f (n, ty) = (n, NLabel (Idx.Section 0) n ty)

makeEdges :: [(Int, Int, ELabel)] -> [LEdge ELabel]
makeEdges es = map f es
  where f (a, b, l) = (a, b, l)

makeWithDirEdges :: [(Int, Int)] -> [LEdge ELabel]
makeWithDirEdges es = map f es
  where f (a, b) = (a, b, defaultELabel)



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
     else ((envUnion envs') { recordNumber = MixedRecord newRecNums }, ts)
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
        ef (EnergyIdx s _ f t) = EnergyIdx s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        de' = M.mapKeys def de
        def (DEnergyIdx s _ f t) = DEnergyIdx s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        p' = M.mapKeys pf p
        pf (PowerIdx s _ f t) = PowerIdx s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        dp' = M.mapKeys dpf dp
        dpf (DPowerIdx s _ f t) = DPowerIdx s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        fn' = M.mapKeys fnf fn
        fnf (FEtaIdx s _ f t) = FEtaIdx s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        dn' = M.mapKeys dnf dn
        dnf (DEtaIdx s _ f t) = DEtaIdx s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        x' = M.mapKeys xf x
        xf (XIdx s _ f t) = XIdx s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        dx' = M.mapKeys dxf dx
        dxf (DXIdx s _ f t) = DXIdx s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))

        v' = M.mapKeys vf v
        --vf (VarIdx s _ f t) = VarIdx s rec (m `safeLookup` (s, f)) (m `safeLookup` (s, t))
        vf (VarIdx s _ use t) = VarIdx s rec use (m `safeLookup` (s, t))

        st' = M.mapKeys stf st
        stf (StorageIdx s _ sto) = StorageIdx s rec (m `safeLookup` (s, sto))


envToEqTerms :: (MkIdxC k) => M.Map k v -> [Equation]
envToEqTerms m = map (give . fst) (M.toList m)

mkPowerEqs :: Int -> Topology -> [Equation]
mkPowerEqs rec topo = concat $ mapGraph (mkPEqs rec) topo

mkPEqs :: Int -> ([LNode NLabel], LNode NLabel, [LNode NLabel]) -> [Equation]
mkPEqs rec (ins, (nid, NLabel sec _ _), outs) = ieqs ++ oeqs -- ++ dieqs ++ doeqs
  where makeVar mkIdx (nid', _) = mkVar $ mkIdx sec rec nid nid'
        dt = Atom $ DTime $ DTimeIdx sec rec
        eis = map (makeVar EnergyIdx) ins
        eos = map (makeVar EnergyIdx) outs
        --deis = map (makeVar DEnergyIdx) ins
        --deos = map (makeVar DEnergyIdx) outs
        pis = map (makeVar PowerIdx) ins
        pos = map (makeVar PowerIdx) outs
        --dpis = map (makeVar DPowerIdx) ins
        --dpos = map (makeVar DPowerIdx) outs

        ieqs = zipWith f eis pis
        oeqs = zipWith f eos pos
        --dieqs = zipWith f deis dpis
        --doeqs = zipWith f deos dpos
        f e p = e := p :* dt


mkIntersectionEqs :: Int -> Topology -> [Equation]
mkIntersectionEqs recordNum topo = concat inEqs ++ concat outEqs ++ stContentEqs
  where actStores = getActiveStores topo
        inouts = map (partitionInOutStatic topo) actStores
        (ins, outs) = unzip inouts
        inEqs = concatMap (map (mkInStoreEqs recordNum)) ins
        outEqs = concatMap (map (mkOutStoreEqs recordNum)) outs
        stContentEqs = concatMap (mkStoreEqs recordNum) inouts

data IOStore = InStore (LNode NLabel)
             | OutStore (LNode NLabel) deriving (Show)

mkStoreEqs :: Int -> ([InOutGraphFormat (LNode NLabel)], [InOutGraphFormat (LNode NLabel)]) -> [Equation]
mkStoreEqs recordNum (ins, outs) = startEq ++ eqs
  where ins' = map (InStore . snd3) ins
        outs' = map (OutStore . snd3) outs
        both@(b:_) = L.sortBy (comparing f) $ ins' ++ outs'

        f (InStore (_, l)) = sectionNLabel l
        f (OutStore (_, l)) = sectionNLabel l
        startEq = k b
        k (InStore (nid, NLabel sec _ (InitStorage st))) =
          [ mkVar (StorageIdx sec recordNum st) := mkVar (VarIdx sec recordNum InSum nid) :* dt]
          where dt = mkVar $ DTimeIdx sec recordNum
        k _ = []
        eqs = map (g . h) (pairs both)
        h (InStore x, y) = (x, y)
        h (OutStore x, y) = (x, y)

        g ((_nid, NLabel sec _ st), InStore (nid', NLabel sec' _ st')) = stnew := (v :* dt) :+ stold
{-
          mkVar (StorageIdx sec' recordNum (getStorageNumber st')) :=
            (mkVar (VarIdx sec' recordNum InSum nid') :* dt)
              :+ mkVar (StorageIdx sec recordNum (getStorageNumber st))
-}
          where stnew = mkVar $ StorageIdx sec' recordNum (getStorageNumber st')
                stold = mkVar $ StorageIdx sec recordNum (getStorageNumber st)
                v = mkVar $ VarIdx sec' recordNum InSum nid'
                dt = mkVar $ DTimeIdx sec' recordNum

        g ((_nid, NLabel sec _ st), OutStore (nid', NLabel sec' _ st')) = stnew := Minus (v :* dt) :+ stold
{-
          mkVar (StorageIdx sec' recordNum (getStorageNumber st')) :=
            Minus (mkVar (VarIdx sec' recordNum OutSum nid') :* dt) :+ (mkVar (StorageIdx sec recordNum (getStorageNumber st)))
-}
          where stnew = mkVar $ StorageIdx sec' recordNum (getStorageNumber st')
                stold = mkVar $ StorageIdx sec recordNum (getStorageNumber st)
                v = mkVar $ VarIdx sec' recordNum OutSum nid'
                dt = mkVar $ DTimeIdx sec' recordNum



mkInStoreEqs :: Int -> InOutGraphFormat (LNode NLabel) -> [Equation]
mkInStoreEqs recordNum (_ins, (nid, NLabel sec _ _), outs@((o,_):_)) = (startEq:osEqs)
  where startEq = mkVar (VarIdx sec recordNum InSum nid) := mkVar (PowerIdx sec recordNum nid o)
        osEqs = map f (pairs outs)
        f (x, y) = mkVar (PowerIdx sec recordNum nid y') :=
                     mkVar (PowerIdx sec recordNum nid x') :+ (Minus (mkVar (PowerIdx xs recordNum x' nid)))
          where (x', NLabel xs _ _) = x
                (y', _) = y
mkInStoreEqs _ _ = []


mkOutStoreEqs :: Int -> InOutGraphFormat (LNode NLabel) -> [Equation]
mkOutStoreEqs recordNum (ins, (nid, NLabel sec _ _), _:_) =
     visumeqs ++ xeqs ++ pieqs
  where xis = map (makeVar XIdx) ins
        --eis = map (makeVar EnergyIdx) ins
        pis = map (makeVar PowerIdx) ins
        makeVar mkIdx (nid', l) = mkVar $ mkIdx (sectionNLabel l) recordNum nid' nid

        --visum = mkVar (VarIdx sec recordNum St nid)
        visum = mkVar (VarIdx sec recordNum InSum nid)

        visumeqs =
           case NonEmpty.fetch pis of
              Just pisne -> [visum := add pisne]
              -- How to cope with empty lists?
              -- Nothing -> []

        xeqs = zipWith g xis pis
        g x e = x := e :* Recip visum

        pis' = map (makeVar' PowerIdx) ins
        makeVar' mkIdx (nid', _) = mkVar $ mkIdx sec recordNum nid nid'

        outv = mkVar (VarIdx sec recordNum OutSum nid)
        pieqs = zipWith h pis' xis
        h e x = e := x :* outv

mkOutStoreEqs _ _ = []

{-
-- | Takes section, record, and a graph.
mkEdgeEq :: Int -> Topology -> [Equation]
mkEdgeEq recordNum topo = map f (map unlabelEdge origEs)
  where origEs = L.filter (\(_, _, l) -> not $ isIntersectionEdge l) (labEdges topo)
        f (x, y) = mkVar (PowerIdx ys recordNum y x) :=
                         (mkVar (PowerIdx xs recordNum x y)) :* (mkVar (FEtaIdx xs recordNum x y))
          where NLabel xs _ _ = fromJust $ lab topo x
                NLabel ys _ _ = fromJust $ lab topo y
-}
-- | Takes section, record, and a graph.
mkEdgeEq :: Int -> Topology -> [Equation]
mkEdgeEq recordNum topo = map (f . unlabelEdge) origEs
  where origEs = L.filter (\(_, _, l) -> not $ isIntersectionEdge l) (labEdges topo)
        f (x, y) =
           EqEdge
              (mkVar $ PowerIdx xs recordNum x y)
              (mkVar $ FEtaIdx xs recordNum x y)
              (mkVar $ PowerIdx ys recordNum y x)
          where NLabel xs _ _ = fromJust $ lab topo x
                NLabel ys _ _ = fromJust $ lab topo y


mkNodeEq :: Int -> Topology -> [Equation]
mkNodeEq recordNum topo = concat $ mapGraph (mkEq recordNum) (elfilter cond topo)
  where cond x = isOriginalEdge x || isInnerStorageEdge x

mkEq :: Int -> InOutGraphFormat (LNode NLabel) -> [Equation]
mkEq recordNum (ins, (nid, NLabel sec _ _), outs) =
   case (fmap (makeOuts vosum . makeVars) $ NonEmpty.fetch ins,
         fmap (makeIns  visum . makeVars) $ NonEmpty.fetch outs) of
      (Nothing, Nothing) -> []
      (Nothing, Just (_, oeqss)) -> oeqss
      (Just (_, ieqss), Nothing) -> ieqss
      (Just (ieqs, ieqss), Just (oeqs, oeqss)) ->
         oeqss ++ ieqss ++ oeqs ++ ieqs ++ [vosum != visum]

  where -- ATTENTION (not very safe): We need this variable in mkInStoreEq again!!!
        visum = mkVar $ VarIdx sec recordNum InSum nid
        -- ATTENTION (not very safe): We need this variable in mkOutStoreEq again!!!
        vosum = mkVar $ VarIdx sec recordNum OutSum nid

        -- For section and record, we focus on the current node n.
        makeVar mkIdx (nid', _) = mkVar $ mkIdx sec recordNum nid nid'
        makeVars xs = (fmap (makeVar XIdx) xs, fmap (makeVar PowerIdx) xs)

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


mkAllDiffEqs :: Int -> Int -> Topology -> [Equation]
mkAllDiffEqs laterRec formerRec topo = {- edgeEqs ++ -} nodeEqs ++ etaEqs ++ xEqs
  where -- edgeEqs = concat $ mapGraph (mkDiffPowerEqs laterRec formerRec) topo
        nodeEqs = concat $ mapGraph (mkDiffNodeEqs laterRec formerRec) topo
        etaEqs = concat $ mapGraph (mkDiffEtaEqs laterRec formerRec) topo
        xEqs = concat $ mapGraph (mkDiffXEqs laterRec formerRec) topo

{-
mkDiffPowerEqs :: Int -> Int -> ([LNode NLabel], LNode NLabel, [LNode NLabel]) -> [Equation]
mkDiffPowerEqs laterRec formerRec (ins, n@(nid, NLabel sec _ _), outs)
  | length ins == 0 && length outs == 0 = []
  | length ins == 0 && length outs > 0 = doeqs
  | length ins > 0 && length outs == 0 = dieqs
  | otherwise = dieqs ++ doeqs
  where makeVar r mkIdx (nid', _) = mkVar $ mkIdx sec r nid nid'
        lpis = map (makeVar laterRec PowerIdx) ins
        fpis = map (makeVar formerRec PowerIdx) ins
        dieqs = zipWith3 f lpis fpis ins

        lpos = map (makeVar laterRec PowerIdx) outs
        fpos = map (makeVar formerRec PowerIdx) outs
        doeqs = zipWith3 f lpos fpos outs
        f x y i = (makeVar laterRec DPowerIdx i) := x :+ (Minus y)
-}

mkDiffEtaEqs :: Int -> Int -> ([LNode NLabel], LNode NLabel, [LNode NLabel]) -> [Equation]
mkDiffEtaEqs laterRec formerRec (_ins, (nid, NLabel sec _ _), outs) = dnoeqs
  where makeVar r mkIdx (nid', _) = mkVar $ mkIdx sec r nid nid'
        lnos = map (makeVar laterRec FEtaIdx) outs
        fnos = map (makeVar formerRec FEtaIdx) outs
        dnoeqs = zipWith3 g lnos fnos outs
        g x y i = (makeVar laterRec DEtaIdx i) := x :+ (Minus y)

mkDiffXEqs :: Int -> Int -> ([LNode NLabel], LNode NLabel, [LNode NLabel]) -> [Equation]
mkDiffXEqs laterRec formerRec (ins, (nid, NLabel sec _ _), outs) = xiseq ++ xoseq
  where makeVar r mkIdx (nid', _) = mkVar $ mkIdx sec r nid nid'
        f dx lx fx = dx := lx :+ (Minus fx)

        lxis = map (makeVar laterRec XIdx) ins
        fxis = map (makeVar formerRec XIdx) ins
        dxis = map (makeVar laterRec DXIdx) ins
        xiseq = zipWith3 f dxis lxis fxis

        lxos = map (makeVar laterRec XIdx) outs
        fxos = map (makeVar formerRec XIdx) outs
        dxos = map (makeVar laterRec DXIdx) outs
        xoseq = zipWith3 f dxos lxos fxos


mkDiffNodeEqs :: Int -> Int -> ([LNode NLabel], LNode NLabel, [LNode NLabel]) -> [Equation]
mkDiffNodeEqs laterRec _formerRec (ins0, (nid, NLabel sec _ _), outs0) =
   case (NonEmpty.fetch ins0, NonEmpty.fetch outs0) of
      (Just ins, Just outs) ->
         let makeVar r mkIdx (nid', _) = mkVar $ mkIdx sec r nid nid'
             dleis = fmap (makeVar laterRec DPowerIdx) ins
             dleos = fmap (makeVar laterRec DPowerIdx) outs
             _sumeq = [add dleis := add dleos]
         in  []
      _ -> []



-- | We sort in and out going edges according to 'FlowDirection'.
-- Undirected edges are filtered away.
-- This is important for creating correct equations.
makeDirTopology :: Topology -> Topology
makeDirTopology topo = Topology $ Gr.mkGraphFromMap ns esm
  where es = map flipAgainst $ filter onlyDirected $ labEdges topo
        esm = M.fromList $ map (\(x, y, l) -> ((x, y), l)) es
        onlyDirected (_, _, elabel) = flowDirection elabel /= UnDir
        flipAgainst e@(x, y, elabel) =
           case flowDirection elabel of
              AgainstDir -> (y, x, elabel { flowDirection = WithDir })
              _ -> e
        ns =
           imFromSet (fromJust . lab topo) $ IS.fromList $
           concatMap (\(x, y) -> [x, y]) $ M.keys esm

-- IM.fromSet is available from containers-0.5
imFromSet :: (IM.Key -> a) -> IS.IntSet -> IM.IntMap a
imFromSet f = IM.fromAscList . map (\k -> (k, f k)) . IS.toAscList
