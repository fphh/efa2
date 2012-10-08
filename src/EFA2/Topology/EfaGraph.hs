
module EFA2.Topology.EfaGraph (EfaGraph, mkGraphFromMap) where

import Data.Graph.Inductive (Graph(..), DynGraph(..), Adj, context)

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Foldable (foldMap)
import Data.Tuple.HT (mapSnd, swap)


data EfaGraph a b = EfaGraph { outEdges :: IM.IntMap (S.Set Int),
                               inEdges :: IM.IntMap (S.Set Int),
                               nodeLabels :: IM.IntMap a,
                               edgeLabels :: M.Map (Int, Int) b } deriving (Show)

getIncoming :: EfaGraph a b -> Int -> [Int]
getIncoming g n =
   foldMap S.toList $ IM.lookup n $ inEdges g

getOutgoing :: EfaGraph a b -> Int -> [Int]
getOutgoing g n =
   foldMap S.toList $ IM.lookup n $ outEdges g

mkOutAdj :: EfaGraph a b -> Int -> Adj b
mkOutAdj g n = map f es
  where es = zip (repeat n) (getOutgoing g n) 
        f e = ((edgeLabels g) M.! e, snd e)
 
mkInAdj :: EfaGraph a b -> Int -> Adj b
mkInAdj g n = map f es
  where es = zip (getIncoming g n) (repeat n)
        f e = ((edgeLabels g) M.! e, fst e)

delEfaNode :: EfaGraph a b -> Int -> EfaGraph a b
delEfaNode g@(EfaGraph ins outs nls els) n = EfaGraph ins' outs' nls' els'
  where remove = S.filter (/= n)
        ins' = IM.map remove (IM.delete n ins)
        outs' = IM.map remove (IM.delete n outs)
        nls' = IM.delete n nls
        es = zip (getIncoming g n) (repeat n) ++ zip (repeat n) (getOutgoing g n) 
        els' = L.foldl' f els es
        f m e = M.delete e m

instance Graph EfaGraph where
         empty = EfaGraph IM.empty IM.empty IM.empty M.empty
         isEmpty gr = IM.null $ nodeLabels gr
         match n g = if isEmpty g then (Nothing, g) else (Just cont, g')
           where lab = (nodeLabels g) IM.! n
                 cont = (mkInAdj g n, n, lab, mkOutAdj g n)
                 g' = delEfaNode g n
         mkGraph ns es =
            mkGraphFromMap (IM.fromList ns)
               (M.fromList $ map (\(x,y,l) -> ((x,y), l)) es)
         labNodes g = IM.toList (nodeLabels g)
         labEdges g = map (\((x, y), l) -> (x, y, l)) (M.toList (edgeLabels g))

_mkDynGraphFromMap ::
   DynGraph gr =>
   IM.IntMap n -> M.Map (Int, Int) e -> gr n e
_mkDynGraphFromMap ns es =
    L.foldl' (flip (&)) empty
       (map (context (mkGraphFromMap ns es)) (IM.keys ns))

mkGraphFromMap ::
   IM.IntMap n -> M.Map (Int, Int) e -> EfaGraph n e
mkGraphFromMap ns es = EfaGraph  outs ins  ns es
  where esl = M.keys es
        mapFromList =
           flip IM.union (fmap (const S.empty) ns) .
           IM.fromListWith S.union . map (mapSnd S.singleton)
        outs = mapFromList esl
        ins = mapFromList $ map swap esl


instance DynGraph EfaGraph where
         (ins, n, lab, outs) & (EfaGraph os is ls es) = 
           {- trace (show os ++ "\n" ++ show resOs ++ "\n" ++ show n++ "\n----\n") -} EfaGraph resOs resIs newLs newEs
           where newLs = IM.insert n lab ls
                 newIns = map (\(l, i) -> ((i, n), l)) ins
                 newOuts = map (\(l, o) -> ((n, o), l)) outs 
                 newEs = foldr (uncurry M.insert) es (newIns ++ newOuts)

                 ins' = map snd ins
                 -- is' = IM.insert n (S.fromList ins') is
                 is' = IM.insertWith S.union n (S.fromList ins') is

                 outs' = map snd outs
                 --os' = IM.insert n (S.fromList outs') os
                 os' = IM.insertWith S.union n (S.fromList outs') os

                 resIs = L.foldl' f is' outs'
                 resOs = L.foldl' f os' ins'

                 f acc x = IM.insertWith S.union x (S.singleton n) acc

