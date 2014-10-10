module Main where

import           Control.Concurrent (MVar (..), forkIO, newEmptyMVar, putMVar,
                                     takeMVar)
import           GHC.IO.Handle      (hGetContents)
import           System.Process     (CreateProcess (..), StdStream (..),
                                     createProcess, proc)

main = do
  mvar <- newEmptyMVar
  forkIO $ ls mvar
  s <- takeMVar mvar
  putStrLn $ "got: " ++ s


ls :: MVar String -> IO ()
ls mvar = do
  (_, Just hout, _, _) <- createProcess (proc "ls" []){ std_out = CreatePipe }
  s <- hGetContents hout
  putMVar mvar s


