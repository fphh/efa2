

module Main where

import EFA.Example.Utility ( edgeVar, makeEdges, constructSeqTopo, )
import EFA.Equation.Absolute ((.=))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility (checkedLookup)

import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Equation.Absolute as EqGen
import qualified EFA.Equation.Env as Env
import EFA.Equation.System ((=.=))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr
import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import Control.Applicative ((<$>), (<*>)) -- (liftA4)

import Data.Monoid ((<>))


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


type Expr s a = EqGen.Expression Node s a a

n1, n2, n3, n4, n5, n6 :: Expr s Double -> Expr s Double

-- steigend und fallend

n1 = EqGen.liftF $ \x ->  0.95*exp(-0.05*((x-2)*(x-2)))
n2 = EqGen.liftF $ \x -> -0.021 * (x - 12) * x

-- fallend
n3 = EqGen.liftF $ \x -> 1/(0.063*(x+4)*(x+4))
n4 = EqGen.liftF $ \x -> exp(-0.05*x*x)
n6 = EqGen.liftF $ \x -> 1-1/(1+1000*exp(-(x-2)))

n7 :: Expr s Double -> Expr s Double -> Expr s Double
n7 =
  EqGen.liftF2 $
    \ p s ->
      let n = 1-1/(1+1000*exp(-(p-2)))
      in  if p/n > s then 0 else n

-- steigend
n5 = EqGen.liftF $ \x -> x/sqrt(1+(x+2)*(x+2))


n01, n12, n13, n31, p10, p12, p21, p13, p31 :: (Eq a, Num a) => Idx.Section -> Expr s a
n01 sec = EqGen.getVar $ edgeVar Idx.Eta sec N0 N1
n12 sec = EqGen.getVar $ edgeVar Idx.Eta sec N1 N2
n13 sec = EqGen.getVar $ edgeVar Idx.Eta sec N1 N3
n31 sec = EqGen.getVar $ edgeVar Idx.Eta sec N3 N1
p10 sec = EqGen.getVar $ edgeVar Idx.Power sec N1 N0
p12 sec = EqGen.getVar $ edgeVar Idx.Power sec N1 N2
p21 sec = EqGen.getVar $ edgeVar Idx.Power sec N2 N1
p13 sec = EqGen.getVar $ edgeVar Idx.Power sec N1 N3
p31 sec = EqGen.getVar $ edgeVar Idx.Power sec N3 N1

--esto :: Expr s Double
esto, ein, eout0, eout1 :: Idx.Energy Node
esto = Idx.Energy (Idx.SecNode sec1 N3) (Idx.SecNode Idx.initSection N3)
ein = Idx.Energy (Idx.SecNode sec0 N0) (Idx.SecNode sec0 N1)
eout0 = Idx.Energy (Idx.SecNode sec0 N2) (Idx.SecNode sec0 N1)
eout1 = Idx.Energy (Idx.SecNode sec1 N2) (Idx.SecNode sec1 N1)


sto0, sto1 :: Idx.Storage Node
sto0 = Idx.Storage (Idx.SecNode sec0 N3)
sto1 = Idx.Storage (Idx.SecNode sec1 N3)


given :: Double -> Double -> EqGen.EquationSystem Node s Double
given _x t =
  (n01 sec0 =.= n5 (p10 sec0))
  <> (n12 sec0 =.= 1) -- n5 (p21 sec0))
  <> (n12 sec1 =.= 1) -- n5 (p12 sec1))
  <> (n13 sec0 =.= 1) -- n1 (p12 sec0))
  <> (n31 sec1 =.= 1) -- n1 (p31 sec1))
  <> (EqGen.getVar (edgeVar Idx.Energy sec1 N3 N1)
        =.= EqGen.getVar (edgeVar Idx.Energy sec0 N3 N1))
  <> (EqGen.getVar (Idx.DTime sec0) + EqGen.getVar (Idx.DTime sec1) =.= 12.1)

  <> (Idx.DTime Idx.initSection .= 1)
  <> (Idx.DTime sec0 .= t)
  <> (Idx.Storage (Idx.SecNode Idx.initSection N3) .= 10)

  -- <> (edgeVar EqGen.xfactor sec0 N1 N2 .= x)
  <> (edgeVar Idx.Power sec0 N2 N1 .= 10)
  <> (edgeVar Idx.Power sec1 N2 N1 .= 10)

{-
t0range, t1range, xrange, erange :: [Double]
t0range = [0.1, 0.2 .. 2]
t1range = [0.1, 0.2 .. 2.2]
erange = 0.01:[1, 1.5 .. 11] ++ [11.9]
-}


xrange, trange :: [Double]
xrange = [0.1, 0.15 .. 0.9] ++ [1]
trange = 0.01:[0.5, 1 .. 12] -- ++ [11.9]

solve :: Double -> Double -> String
solve x e =
  let env = EqGen.solve (given x e) seqTopo
      emap = Env.energyMap env
--      smap = Env.storageMap env
      f _es ei eo0 eo1 = (eo0 + eo1) / ei -- (es + ei)
  in  show x ++ " " ++ show e ++ " " ++
--        Format.unUnicode (formatValue (checkedLookup smap sto0))
--        Format.unUnicode (formatValue
{-
          (liftA2 (/) (checkedLookup smap sto0)
                      (checkedLookup smap sto1)))
-}
      Format.unUnicode (formatValue
         (f <$> (Env.unAbsolute $ checkedLookup emap esto)
            <*> (Env.unAbsolute $ checkedLookup emap ein)
            <*> (Env.unAbsolute $ checkedLookup emap eout0)
            <*> (Env.unAbsolute $ checkedLookup emap eout1)))


main :: IO ()
main = do
  --putStrLn $ unlines $ map (\x -> unlines $ map (solve x) trange) xrange

  let env = EqGen.solve (given 0.5 1) seqTopo
  -- let env' = EqGen.solve (given 0.9 undefined) seqTopo

  concurrentlyMany_ [
    Draw.sequFlowGraphAbsWithEnv seqTopo env ]
    -- Draw.sequFlowGraphAbsWithEnv seqTopo env' ]