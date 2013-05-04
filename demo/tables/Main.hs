

module Main where

import qualified EFA.IO.TableParser as Table
import qualified EFA.Signal.Plot as Plot

import EFA.Utility.Async (concurrentlyMany_)


main :: IO ()
main = do
  tabEn <- Table.read "engine.txt"
  tabMo <- Table.read "motor.txt"

  concurrentlyMany_ [
    Plot.tableLinear "maxTorque" tabEn,
    Plot.tableLinear "dragTorque" tabEn,

    Plot.tableLinear2D "table2D_efficiencyMap" tabEn,
    Plot.tableLinear2D "table2D_efficiencyMap_firstQuadrant" tabMo,
  
    Plot.tableSurface "table2D_efficiencyMap" tabEn,
    Plot.tableSurface "table2D_efficiencyMap_firstQuadrant" tabMo ]
