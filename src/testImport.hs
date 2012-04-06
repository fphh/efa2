
import EFA2.IO.Import

inPath = "../input/"
-- name =  "linear_res.csv3" -- hand generated cvs file
name =  "linear_res.csv2" -- full modelica cvs file

main = do 
--  text <- readFile (inPath ++ name)
  rec <- modelicaCSVImport (inPath ++ name)
  putStrLn (show rec)