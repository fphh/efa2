
module Main where

import EFA.IO.Import
-- import EFA.Report.Signal
import EFA.Signal.Plot

name1, name2 :: String
name1 =  "linear_res.csv" -- hand generated cvs file
name2 =  "linear_res_manual.csv" -- full modelica cvs file

main :: IO ()
main = do 
  rec1 <- modelicaCSVImport (name1)
  rec2 <- modelicaCSVImport (name2)
  
  -- 
  putStrLn ("Full Modelica File")
  putStrLn ""
  putStrLn (show rec1)
  putStrLn ""  
  putStrLn ""  
  
  putStrLn ("Small Test File")
  putStrLn ""
  putStrLn (show rec2)
  
  sigPlot "Hallo" rec1
  