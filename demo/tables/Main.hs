
module Main where

import qualified EFA.IO.TableParser as Table
import qualified EFA.Signal.PlotIO as PlotIO
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.ConvertTable as CT
import EFA.Utility.Map (checkedLookup)

import qualified Graphics.Gnuplot.Terminal.Default as Def

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Map as Map


plot3D ::
  ( S.UTSignal2 [] [] Double,
    S.PSignal2 [] [] Double,
    S.NSignal2 [] [] Double ) -> IO ()
plot3D (x, y, z) = PlotIO.surface "test" Def.cons (\_->"") x y z

plot2D ::
  ( S.PSignal [] Double,
    NonEmpty.T [] (S.NSignal [] Double) ) -> IO ()
plot2D (x, y) =
  PlotIO.xy "test" Def.cons id x $
  fmap (PlotIO.label "eta") $ NonEmpty.flatten y


main :: IO ()
main = do

  tabEn <- Table.read "engine.txt"
  tabMo <- Table.read "motor.txt"
  Table.write "combined.txt" (Map.union tabEn tabEn)

  let em3D =
        CT.convertToSignal3D
          (checkedLookup "demo/table" tabEn "table2D_efficiencyMap") :
        CT.convertToSignal3D
          (checkedLookup "demo/table" tabMo "table2D_efficiencyMap_firstQuadrant") :
        []

      em2D =
        CT.convertToSignal3D2D
          (checkedLookup "demo/table" tabEn "table2D_efficiencyMap") :
        CT.convertToSignal3D2D
          (checkedLookup "demo/table" tabMo "table2D_efficiencyMap_firstQuadrant") :
        CT.convertToSignal2D
          (checkedLookup "demo/table" tabEn "maxTorque") :
        CT.convertToSignal2D
          (checkedLookup "demo/table" tabEn "dragTorque") :
        []

  mapM_ plot3D em3D
  mapM_ plot2D em2D
