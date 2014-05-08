module EFA.IO.Reference where

import EFA.Utility.Filename((+++),FPath(..),DirPath(..), Directory(..),makeRelativeTo,
                            Abs,Rel,toString,fromString,getLastDir, getFileName)

import System.Environment
import System.Process(runCommand)
import System.Directory(doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing)

import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)
import qualified EFA.Reference.Base as Ref
import qualified Data.Map as Map
import Control.Monad (void)

import EFA.Utility.Trace(mytrace)


modul :: ModuleName
modul = ModuleName "IO.Reference"

nc :: FunctionName -> Caller
nc = genCaller modul

getRefFolder :: Caller -> IO(DirPath Abs)
getRefFolder caller = fmap (fromString (caller |>  nc "getRoot" )) $ getEnv "REFERENCE"

getTmpFolder :: Caller -> IO(DirPath Abs)
getTmpFolder caller =  fmap (fromString (caller |>  nc "getRoot" )) $ getEnv "REFTMP"

checkFolder :: DirPath Abs -> IO(Bool)
checkFolder path@(DirPath xs) = doesDirectoryExist (toString path)

cleanTmp :: DirPath Abs -> IO()
cleanTmp path = void $ runCommand "cd path || ls *"

createDir :: Caller -> DirPath Abs -> IO()
createDir caller path  = createDirectoryIfMissing True (toString path)

writeTest :: DirPath Abs -> DirPath Rel -> Ref.Test -> IO()
writeTest root group (Ref.Test p m) = do
  let path = root +++ group +++ p
  createDir (nc " writeTest") path
  mapM_ (\ (name,x) -> writeRef (path +++ name) x)  $ Map.toList m

writeRef :: FPath Abs -> Ref.Data -> IO()
writeRef path x = writeFile (toString path) (show x)

readRef :: DirPath Abs -> FPath Rel -> IO (Ref.Data)
readRef path filePath = fmap read $ readFile (toString (path +++ filePath))

readTest :: Caller -> DirPath Abs -> DirPath Rel -> DirPath Rel -> IO(Ref.Test)
readTest caller base group test = do
  let f n = fromString (caller |> nc "readTest") $ n
      path = base+++group+++test
  xs <- getDirectoryContents (toString $ path)
  let files = map f $   filter (\x -> x /="." && x /="..") xs
  refs <- mapM (readRef path) files
  return $ Ref.Test test $
    Map.fromList $ zip files refs

  