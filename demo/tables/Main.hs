
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.Map as M

import qualified EFA.IO.TableParser as Table
import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Signal as S

import qualified Graphics.Gnuplot.Terminal.Default as Def

import qualified EFA.Signal.ConvertTable as CT


plot3D ::
  ( S.UTSignal2 [] [] Double,
    S.PSignal2 [] [] Double,
    S.NSignal2 [] [] Double ) -> IO ()
plot3D (x, y, z) = Plot.surfaceIO "test" x y z

plot2D ::
  ( S.PSignal [] Double,
    [ S.NSignal [] Double ] ) -> IO ()
plot2D (x, y) = Plot.xyIO "test" Def.cons id (const "eta") x y


main :: IO ()
main = do

  tabEn <- Table.read "engine.txt"
  tabMo <- Table.read "motor.txt"

  let em3D = 
        CT.convertToSignal3D 
          (M.lookup "table2D_efficiencyMap" tabEn) :
        CT.convertToSignal3D
          (M.lookup "table2D_efficiencyMap_firstQuadrant" tabMo) :
        []

      em2D =
        CT.convertToSignal3D2D
          (M.lookup "table2D_efficiencyMap" tabEn) :
        CT.convertToSignal3D2D
          (M.lookup "table2D_efficiencyMap_firstQuadrant" tabMo) :
        CT.convertToSignal2D
          (M.lookup "maxTorque" tabEn) :
        CT.convertToSignal2D
          (M.lookup "dragTorque" tabEn) :
        []

  Table.write "combined.txt" (M.union tabEn tabEn)
  mapM_ plot3D em3D
  mapM_ plot2D em2D
