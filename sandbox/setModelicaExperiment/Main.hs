

module Main where


import Text.XML.HXT.Core

data Experiment = Experiment {
  startTime :: Double,
  stopTime :: Double,
  stepSize :: Double,
  tolerance :: Double,
  solver :: String,
  outputFormat :: String,
  variableFilter :: String } deriving (Show)

defaultExperiment :: Experiment
defaultExperiment = Experiment {
  startTime = 0.0,
  stopTime = 1.0,
  stepSize = 0.002,
  tolerance = 0.000001,
  solver = "dassl",
  outputFormat = "mat",
  variableFilter = ".*" }


file :: FilePath
file = "SmartEnergyLivingLab_init.xml"

work :: Experiment -> (IOSLA (XIOState s)) n XmlTree
work ex =
  readDocument [] file
  >>>
  processTopDown 
    (newExperiment ex `when` (isElem >>> hasName "DefaultExperiment"))
  >>>
  writeDocument [] ("new_" ++ file)



newExperiment :: (ArrowXml a) => Experiment -> a n XmlTree
newExperiment ex =
  mkelem "DefaultExperiment"
    [ sattr "startTime" (show $ startTime ex),
      sattr "stopTime" (show $ stopTime ex),
      sattr"stepSize" (show $ stepSize ex),
      sattr "tolerance" (show $ tolerance ex),
      sattr "solver" (solver ex),
      sattr "outputFormat" (outputFormat ex),
      sattr "variableFilter" (variableFilter ex) ] []


main :: IO ()
main = do 
  let experiment =
        defaultExperiment {
          stopTime = 2.77777,
          tolerance = 0.005 }

  runX (work experiment)
  return ()
