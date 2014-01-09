{- |
Inspired by @async@ package.
-}
module EFA.Utility.Async where

import Control.Concurrent.MVar (MVar, putMVar, readMVar, newEmptyMVar)
import Control.Concurrent (forkIO)
import Control.Monad ((>=>), void)


newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async io = do
  m <- newEmptyMVar
  void $ forkIO $ putMVar m =<< io
  return (Async m)

wait :: Async a -> IO a
wait (Async m) = readMVar m

concurrentlyMany_ :: [IO ()] -> IO ()
concurrentlyMany_ = mapM async >=> mapM_ wait

concurrentlyMany :: [IO a] -> IO [a]
concurrentlyMany = mapM async >=> mapM wait
