
import EFA2.Display.Report
import EFA2.Signal.Signal

l = [1..5] :: [Double]
ll = [l,l]

t = toTable ll

s = sfromList l :: PSigL

main = do
  
  putStrLn $ show t
  
  report ll
  report s

  