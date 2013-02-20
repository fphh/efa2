

module Main where

import Data.Monoid ((<>))
import qualified Data.Map as M

import Data.Foldable (foldMap)
import Control.Applicative -- (liftA4)


import EFA.Example.Utility
  ( edgeVar, makeEdges, (.=), constructSeqTopo, recAbs )

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Graph.Draw as Draw

import qualified EFA.Utility.Stream as Stream
import EFA.Utility (checkedLookup)
import EFA.Utility.Async

import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Equation.System as EqGen
import EFA.Equation.System ((=.=))
import qualified EFA.Equation.Env as Env
import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)


import qualified EFA.Graph as Gr

import Debug.Trace

sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0


data Node = N0 | N1 | N2 | N3 deriving (Show, Eq, Enum, Ord)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


topoDreibein :: TD.Topology Node
topoDreibein = Gr.mkGraph ns (makeEdges es)
  where ns = [ (N0, TD.Source),
               (N1, TD.Crossing),
               (N2, TD.Sink),
               (N3, TD.Storage) ]
        es = [(N0, N1), (N1, N3), (N1, N2)]

seqTopo :: TD.SequFlowGraph Node
seqTopo = constructSeqTopo topoDreibein [0, 4]

n1, n2, n3, n4, n5, n6 ::
  EqGen.ExprWithVars Node s Double -> EqGen.ExprWithVars Node s Double

-- steigend und fallend

n1 x =  0.95*exp(-0.05*((x-2)*(x-2)))
n2 x = -0.021 * (x - 12) * x

-- fallend
n3 x = 1/(0.063*(x+4)*(x+4))
n4 x = exp(-0.05*x*x)
n6 x = 1-1/(1+1000*exp(-(x-2)))

n7 = EqGen.liftF2 f
  where f p s = if p/n > s then 0 else n
          where n = 1-1/(1+1000*exp(-(p-2)))

-- steigend
n5 x = x/sqrt(1+(x+2)*(x+2))


n01, n12, n13, n31, p10, p12, p21, p13, p31 :: Idx.Section -> EqGen.ExprWithVars Node s a
n01 sec = edgeVar EqGen.eta sec N0 N1
n12 sec = edgeVar EqGen.eta sec N1 N2
n13 sec = edgeVar EqGen.eta sec N1 N3
n31 sec = edgeVar EqGen.eta sec N3 N1
p10 sec = edgeVar EqGen.power sec N1 N0
p12 sec = edgeVar EqGen.power sec N1 N2
p21 sec = edgeVar EqGen.power sec N2 N1
p13 sec = edgeVar EqGen.power sec N1 N3
p31 sec = edgeVar EqGen.power sec N3 N1

stoinit :: EqGen.ExprWithVars Node s a
stoinit = EqGen.storage (Idx.SecNode Idx.initSection N3)

--esto :: EqGen.ExprWithVars Node s Double
esto, ein, eout0, eout1 :: Idx.Energy Node
esto = Idx.Energy recAbs (Idx.SecNode sec1 N3) (Idx.SecNode Idx.initSection N3)
ein = Idx.Energy recAbs (Idx.SecNode sec0 N0) (Idx.SecNode sec0 N1)
eout0 = Idx.Energy recAbs (Idx.SecNode sec0 N2) (Idx.SecNode sec0 N1)
eout1 = Idx.Energy recAbs (Idx.SecNode sec1 N2) (Idx.SecNode sec1 N1)


sto0, sto1 :: Idx.Storage Node
sto0 = Idx.Storage recAbs (Idx.SecNode sec0 N3)
sto1 = Idx.Storage recAbs (Idx.SecNode sec1 N3)


given :: Double -> Double -> EqGen.EquationSystem Idx.Absolute Node s Double
given x t =
  (n01 sec0 =.= n5 (p10 sec0))
  <> (n12 sec0 =.= 1) -- n5 (p21 sec0))
  <> (n12 sec1 =.= 1) -- n5 (p12 sec1))
  <> (n13 sec0 =.= 1) -- n1 (p12 sec0))
  <> (n31 sec1 =.= 1) -- n1 (p31 sec1))
  <> (edgeVar EqGen.energy sec1 N3 N1 =.= edgeVar EqGen.energy sec0 N3 N1)
  <> (EqGen.dtime sec0 + EqGen.dtime sec1 =.= 12.1)
  <> (foldMap (uncurry (.=)) $
       (EqGen.dtime Idx.initSection, 1) :
       (EqGen.dtime sec0, t) :
       (EqGen.storage (Idx.SecNode Idx.initSection N3), 10) :

       -- (edgeVar EqGen.xfactor sec0 N1 N2, x) :
       (edgeVar EqGen.power sec0 N2 N1, 10) :
       (edgeVar EqGen.power sec1 N2 N1, 10) :
       [])

{-
t0range, t1range, xrange, erange :: [Double]
t0range = [0.1, 0.2 .. 2]
t1range = [0.1, 0.2 .. 2.2]
erange = 0.01:[1, 1.5 .. 11] ++ [11.9]
-}


xrange = [0.1, 0.15 .. 0.9] ++ [1]
trange = 0.01:[0.5, 1 .. 12] -- ++ [11.9]

solve :: Double -> Double -> String
solve x e =
  let env = EqGen.solve (given x e) seqTopo
      emap = Env.energyMap env
      smap = Env.storageMap env
      f es ei eo0 eo1 = (eo0 + eo1) / ei -- (es + ei)
  in  show x ++ " " ++ show e ++ " " ++
--        Format.unUnicode (formatValue (checkedLookup smap sto0))
--        Format.unUnicode (formatValue
{-
          (liftA2 (/) (checkedLookup smap sto0)
                      (checkedLookup smap sto1)))
-}
      Format.unUnicode (formatValue
         (f <$> (checkedLookup emap esto)
            <*> (checkedLookup emap ein)
            <*> (checkedLookup emap eout0)
            <*> (checkedLookup emap eout1)))


main :: IO ()
main = do
  --putStrLn $ unlines $ map (\x -> unlines $ map (solve x) trange) xrange

  let env = EqGen.solve (given 0.5 1) seqTopo
  -- let env' = EqGen.solve (given 0.9 undefined) seqTopo

  concurrentlyMany_ [
    Draw.sequFlowGraphAbsWithEnv seqTopo env ]
    -- Draw.sequFlowGraphAbsWithEnv seqTopo env' ]
