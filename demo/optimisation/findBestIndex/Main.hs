module Main where

import qualified EFA.Data.ND.Cube.Map as CubeMap 
import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Data.ND as ND

data Base

  
fselectMax :: (Maybe Idx.AbsoluteState, Interp.Val Double) -> (Maybe Idx.AbsoluteState, Interp.Val Double) -> Bool
fselectMax x y = Interp.greaterThanWithInvalid (snd x) (snd y)

getState = fst

-- all regular
searchCube :: CubeMap.Data Base ND.Dim1 [] (Maybe Idx.AbsoluteState, Interp.Val Double)
searchCube = 
  CubeMap.Data $ zip (map (Just . Idx.AbsoluteState) $ repeat 0) 
  $ map (Interp.Inter . Arith.fromInteger . fromIntegral) $ [0..10]

-- invalid at end
searchCube2 :: CubeMap.Data Base ND.Dim1 [] (Maybe Idx.AbsoluteState, Interp.Val Double)
searchCube2 = 
  CubeMap.Data $ zip (map (Just . Idx.AbsoluteState) $ repeat 0) 
  $ map (Interp.Inter . Arith.fromInteger . fromIntegral) [0..10] ++ [Interp.Invalid ["Test"]]

-- invalid at start and end
searchCube3 :: CubeMap.Data Base ND.Dim1 [] (Maybe Idx.AbsoluteState, Interp.Val Double)
searchCube3 = 
  CubeMap.Data $ zip (map (Just . Idx.AbsoluteState) $ repeat 0) 
  $ [Interp.Invalid ["Test"]] ++ map (Interp.Inter . Arith.fromInteger . fromIntegral) [0..10] ++ [Interp.Invalid ["Test"]]

--only invalid
searchCube4 :: CubeMap.Data Base ND.Dim1 [] (Maybe Idx.AbsoluteState, Interp.Val Double)
searchCube4 = 
  CubeMap.Data $ zip (map (Just . Idx.AbsoluteState) $ repeat 0) 
  $ [Interp.Invalid ["Test"]] ++ [Interp.Invalid ["Test"]]

-- all regular
searchCube5 :: CubeMap.Data Base ND.Dim1 [] (Maybe Idx.AbsoluteState, Interp.Val Double)
searchCube5 = 
  CubeMap.Data $ zip (map (Just . Idx.AbsoluteState) $ repeat 0) 
  $ map (Interp.Inter . Arith.fromInteger . fromIntegral) $ [0..10]++[10]


-- all regular
searchCube6 :: CubeMap.Data Base ND.Dim1 [] (Maybe Idx.AbsoluteState, Interp.Val Double)
searchCube6 = 
  CubeMap.Data $ zip (map (Just . Idx.AbsoluteState) $ repeat 0) 
  $ map (Interp.Inter . Arith.fromInteger . fromIntegral) $ ([0,2,4,6,8,10]++[11,9..1])

searchCube7 :: CubeMap.Data Base ND.Dim1 [] (Maybe Idx.AbsoluteState, Interp.Val Double)
searchCube7 = 
  CubeMap.Data $ zip (map (Just . Idx.AbsoluteState) $ repeat 0) 
  $ map (fmap Arith.fromInteger ) $ ([Interp.Invalid [],Interp.Extra 0, Interp.Invalid [], 
                                                              Interp.Inter 2, Interp.Invalid []])

searchCube8 :: CubeMap.Data Base ND.Dim1 [] (Maybe Idx.AbsoluteState, Interp.Val Double)
searchCube8 = 
  CubeMap.Data $ zip (map (Just . Idx.AbsoluteState) $ concat $ repeat [0..2]) 
  $ map (fmap Arith.fromInteger ) $ ([Interp.Invalid [],Interp.Extra 0, Interp.Invalid [], 
                                                              Interp.Inter 2, Interp.Invalid [], Interp.Inter 1])

main = do
  print $ CubeMap.findBestWithIndexByPerState getState fselectMax searchCube -- delivers maximum element - OK
  print $ CubeMap.findBestWithIndexByPerState getState fselectMax searchCube2 -- neglects Invalid - OK
  print $ CubeMap.findBestWithIndexByPerState getState fselectMax searchCube3 -- neglects Invalid - OK
  print $ CubeMap.findBestWithIndexByPerState getState fselectMax searchCube4 -- delivers Index zero - OK 
  print $ CubeMap.findBestWithIndexByPerState getState fselectMax searchCube5 -- delivers Element with lowest Index -- is OK
  print $ CubeMap.findBestWithIndexByPerState getState fselectMax searchCube6 -- delivers korrekt result in ascending, descending List
  print $ CubeMap.findBestWithIndexByPerState getState fselectMax searchCube7 -- delivers korrekt result in mixed List - OK
  print $ CubeMap.findBestWithIndexByPerState getState fselectMax searchCube8 -- delivers korrekt result in mixed List with several states - OK
  