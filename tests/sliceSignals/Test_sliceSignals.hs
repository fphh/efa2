import EFA2.Signal.Sequence


time = [0..9]
p1 = [0,0,0,5,5,-5,-5,0,2,2]
steps = makeSteps time p1

main = do
  putStrLn (show $ zip time p1) 
  putStrLn (show steps)
  
  