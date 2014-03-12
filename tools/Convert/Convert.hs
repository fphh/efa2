

module Convert where



import Text.Printf (printf)

import System.FilePath.Posix ((</>), (<.>))
import System.Environment (getArgs)
import System.Cmd (rawSystem)
import System.Directory (doesDirectoryExist, createDirectory, removeDirectoryRecursive , removeFile)

import qualified Data.List as List

import Control.Monad (when)

co :: String
co = "convert_output"

main :: IO ()
main = do
  base:leftAbove:rightAbove:leftBelow:rightBelow:num:_ <- getArgs

  let n = read num :: Int


      filenames = map ((<.> "png") . printf "%6.6d") [0 .. n-1]

  b <- doesDirectoryExist co

  when b (removeDirectoryRecursive  co)
  createDirectory co


  let 
      complFn dir fn = base </> dir </> fn

      g fname = do
        let la = complFn leftAbove fname
            ra = complFn rightAbove fname
            lb = complFn leftBelow fname
            rb = complFn rightBelow fname

            above = co </> "above-" ++ fname
            below = co </> "below-" ++ fname

            final = co </> fname

        rawSystem "convert" [la, ra, "+append", above]
        rawSystem "convert" [lb, rb, "+append", below]

        rawSystem "convert" [above, below, "-append", final]

        removeFile above
        removeFile below


  mapM_ g filenames

