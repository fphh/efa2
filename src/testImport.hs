
import EFA2.IO.Import

inPath = "../input/"
name =  "linear_res.csv3"

main = do 
  text <- readFile (inPath ++ name)
  let map = csvImport text 
  putStrLn (show map)