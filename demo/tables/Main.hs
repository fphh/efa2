

module Main where

import qualified EFA.IO.TableParser as Table
import qualified EFA.Signal.Plot as Plot

import EFA.Utility.Async (concurrentlyMany_)



main :: IO ()
main = do
  tabEn <- Table.read "engine.txt"
  tabMo <- Table.read "motor.txt"

  concurrentlyMany_ [
    Plot.plotTable2D "table2D_efficiencyMap" tabEn,
    Plot.plotTableLinear "maxTorque" tabEn,
    Plot.plotTableLinear "dragTorque" tabEn,
    Plot.plotTable2D "table2D_efficiencyMap_firstQuadrant" tabMo ]
