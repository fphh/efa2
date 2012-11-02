module EFA2.Topology.EfaGraph
          (EfaGraph, mkGraphFromMap, Edge(..), labEdges_) where

import qualified Data.Graph.Inductive as IG

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Data.Foldable (foldMap)
import Data.Tuple.HT (mapSnd)


data EfaGraph node nodeLabel edgeLabel =
   EfaGraph {
      outEdges, inEdges :: M.Map node (S.Set node),
      nodeLabels :: M.Map node nodeLabel,
      edgeLabels :: M.Map (Edge node) edgeLabel
   } deriving (Show, Eq)

data Edge node = Edge node node
   deriving (Show, Eq, Ord)

instance Functor Edge where
   fmap f (Edge x y) = Edge (f x) (f y)



getIncoming :: (Ord n) => EfaGraph n nl el -> n -> [n]
getIncoming g n =
   foldMap S.toList $ M.lookup n $ inEdges g

getOutgoing :: (Ord n) => EfaGraph n nl el -> n -> [n]
getOutgoing g n =
   foldMap S.toList $ M.lookup n $ outEdges g

type Adj n el = [(el, n)]

mkOutAdj :: (Ord n) => EfaGraph n nl el -> n -> Adj n el
mkOutAdj g n = map f es
  where es = zipWith Edge (repeat n) (getOutgoing g n)
        f e@(Edge _ x) = ((edgeLabels g) M.! e, x)

mkInAdj :: (Ord n) => EfaGraph n nl el -> n -> Adj n el
mkInAdj g n = map f es
  where es = zipWith Edge (getIncoming g n) (repeat n)
        f e@(Edge x _) = ((edgeLabels g) M.! e, x)

delEfaNode :: (Ord n) => EfaGraph n nl el -> n -> EfaGraph n nl el
delEfaNode g@(EfaGraph outs ins nls els) n =
   EfaGraph (remove outs) (remove ins) (M.delete n nls) els'
  where remove = M.map (S.filter (/= n)) . M.delete n
        els' =
           L.foldl' (flip M.delete) els $
              zipWith Edge (getIncoming g n) (repeat n) ++
              zipWith Edge (repeat n) (getOutgoing g n)

instance (Ord n, Enum n) => IG.Graph (EfaGraph n) where
   empty = EfaGraph M.empty M.empty M.empty M.empty
   isEmpty = isEmpty
   match ni g =
      if isEmpty g
        then (Nothing, g)
        else (Just cont, delEfaNode g n)
     where n = toEnum ni
           cont =
              (map (mapSnd fromEnum) $ mkInAdj g n,
               ni,
               nodeLabels g  M.! n,
               map (mapSnd fromEnum) $ mkOutAdj g n)
   labNodes = map (\(n,l) -> (fromEnum n, l)) . labNodes
   labEdges = map (\(n,m,l) -> (fromEnum n, fromEnum m, l)) . labEdges
   mkGraph ns es =
      mkGraph
         (map (\(n,l) -> (toEnum n, l)) ns)
         (map (\(n,m,l) -> (toEnum n, toEnum m, l)) es)

isEmpty :: EfaGraph  n nl el -> Bool
isEmpty = M.null . nodeLabels

labNodes :: EfaGraph n nl el -> [(n, nl)]
labNodes = M.toList . nodeLabels

labEdges :: EfaGraph n nl el -> [(n, n, el)]
labEdges = map (\(Edge x y, l) -> (x, y, l)) . M.toList . edgeLabels

labEdges_ :: EfaGraph n nl el -> [(Edge n, el)]
labEdges_ = M.toList . edgeLabels

mkGraph ::
   (Ord n) =>
   [(n, nl)] -> [(n, n, el)] -> EfaGraph n nl el
mkGraph ns es =
   mkGraphFromMap (M.fromList ns) $
      M.fromList $ map (\(x,y,l) -> (Edge x y, l)) es

mkGraphFromMap ::
   (Ord n) =>
   M.Map n nl -> M.Map (Edge n) el -> EfaGraph n nl el
mkGraphFromMap ns es = EfaGraph  outs ins  ns es
  where esl = M.keys es
        mapFromList f =
           flip M.union (fmap (const S.empty) ns) .
           M.fromListWith S.union . map f
        outs = mapFromList (\(Edge x y) -> (x, S.singleton y)) esl
        ins  = mapFromList (\(Edge x y) -> (y, S.singleton x)) esl

instance (Ord n, Enum n) => IG.DynGraph (EfaGraph n) where
         (ins, ni, lab_, outs) & (EfaGraph os is ls es) =
           {- trace (show os ++ "\n" ++ show resOs ++ "\n" ++ show n++ "\n----\n") -} EfaGraph resOs resIs newLs newEs
           where n = toEnum ni
                 insEnum = map (mapSnd toEnum) ins
                 outsEnum = map (mapSnd toEnum) outs
                 newLs = M.insert n lab_ ls
                 newIns = map (\(l, i) -> (Edge i n, l)) insEnum
                 newOuts = map (\(l, o) -> (Edge n o, l)) outsEnum
                 newEs = foldr (uncurry M.insert) es (newIns ++ newOuts)

                 ins' = map snd insEnum
                 -- is' = M.insert n (S.fromList ins') is
                 is' = M.insertWith S.union n (S.fromList ins') is

                 outs' = map snd outsEnum
                 --os' = M.insert n (S.fromList outs') os
                 os' = M.insertWith S.union n (S.fromList outs') os

                 resIs = L.foldl' f is' outs'
                 resOs = L.foldl' f os' ins'

                 f acc x = M.insertWith S.union x (S.singleton n) acc

