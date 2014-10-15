module Main where

import           Control.Concurrent (MVar (..), forkIO, newEmptyMVar, putMVar,
                                     takeMVar)
import           System.IO          (hGetContents, hPutStrLn)
import           System.Process     (CreateProcess (..), StdStream (..),
                                     createProcess, proc)

main = do
  recordToValidate <- newEmptyMVar
  validationResult <- newEmptyMVar
  forkIO $ getRecordsToValidate recordToValidate
  forkIO $ validationWorker recordToValidate validationResult
  loop validationResult
    where
      loop validationResult = do
        s <- takeMVar validationResult
        putStrLn $ "result: " ++ s
        loop validationResult



getRecordsToValidateProc = proc "rake" ["db:validate:perform"]
validationWorkerProc     = proc "rake" ["db:validate:worker"]

getRecordsToValidate :: MVar String -> IO ()
getRecordsToValidate recordToValidate = do
  (_, Just hout, _, _) <- createProcess getRecordsToValidateProc{ std_out = CreatePipe }
  s <- hGetContents hout
  mapM_ (putMVar recordToValidate) $ lines s

validationWorker :: MVar String -> MVar String -> IO ()
validationWorker recordToValidate validationResult = do
  (Just hin, Just hout, _, _) <- createProcess validationWorkerProc{ std_in = CreatePipe, std_out = CreatePipe }
  loop hin hout recordToValidate validationResult
    where
      loop hin hout recordToValidate validationResult = do
        validationRequest <- takeMVar recordToValidate
        putStrLn $ "worker received directive: " ++ validationRequest
        hPutStrLn hin validationRequest
        validationResponse <- hGetContents hout
        putStrLn $ "worker responded with: " ++ validationResponse
        putMVar validationResult validationResponse
        loop hin hout recordToValidate validationResult

