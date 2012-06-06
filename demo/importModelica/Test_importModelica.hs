
import EFA2.IO.Import
import EFA2.Display.DispSignal
import EFA2.Display.Plot

name1 =  "linear_res.csv" -- hand generated cvs file
name2 =  "linear_res_manual.csv" -- full modelica cvs file

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
  
  sigPlot rec1
  