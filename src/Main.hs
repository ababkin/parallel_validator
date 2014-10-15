module Main where

import           Control.Monad      (forever)
import           Control.Concurrent (MVar (..), forkIO, newEmptyMVar, putMVar,
                                     takeMVar)
import           System.IO          (hGetLine, hPutStrLn, hSetBuffering, BufferMode (..))
import           System.Process     (CreateProcess (..), StdStream (..),
                                     createProcess, proc)

main = do
  request <- newEmptyMVar
  response <- newEmptyMVar
  forkIO $ getRecordsToValidate request
  forkIO $ validationWorker request response
  forever $ do
    s <- takeMVar response
    putStrLn $ "result: " ++ s

getRecordsToValidateProc = proc "rake" ["db:validate:perform"]
validationWorkerProc     = proc "rake" ["db:validate:worker"]

getRecordsToValidate :: MVar String -> IO ()
getRecordsToValidate request = do
  (_, Just hout, _, _) <- createProcess getRecordsToValidateProc{ std_out = CreatePipe }
  forever $ do
    s <- hGetLine hout
    putMVar request s

validationWorker :: MVar String -> MVar String -> IO ()
validationWorker request response = do
  (Just hin, Just hout, _, _) <- createProcess validationWorkerProc{ std_in = CreatePipe, std_out = CreatePipe }
  hSetBuffering hin LineBuffering
  forever $ do
    validationRequest <- takeMVar request
    hPutStrLn hin validationRequest
    validationResponse <- hGetLine hout
    putMVar response validationResponse

