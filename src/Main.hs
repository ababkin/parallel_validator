module Main where

import           Control.Applicative ((<$>))
import           Control.Concurrent  (MVar (..), forkIO, newEmptyMVar, putMVar,
                                      takeMVar)
import           Control.Monad       (forever, replicateM_, unless)
import           Data.Maybe          (Maybe (..))
import           Data.Monoid         (Monoid (..), (<>))
import           System.Exit         (ExitCode (..), exitWith)
import           System.IO           (BufferMode (..), hClose, hGetLine, hIsEOF,
                                      hPutStrLn, hSetBuffering)
import           System.Process      (CreateProcess (..), StdStream (..),
                                      createProcess, proc, waitForProcess)

nWorkers = 12

instance Monoid ExitCode where
  mempty = ExitSuccess
  ec@(ExitFailure n) `mappend` _ = ec
  _ `mappend` ec = ec

main = do
  request           <- newEmptyMVar
  status            <- newEmptyMVar
  finishedProducing <- newEmptyMVar
  forkIO $ producer request finishedProducing
  replicateM_ nWorkers $ forkIO $ worker request status

  takeMVar finishedProducing
  putStrLn "main: finished producing"
  wait request status nWorkers ExitSuccess
    where
      wait :: MVar (Maybe String) -> MVar ExitCode -> Int -> ExitCode -> IO ()
      wait _ _ 0 ec = exitWith ec
      wait request status nWorkers statusAccum = do
        putMVar request Nothing
        st <- takeMVar status
        putStrLn "main: worker finished"
        wait request status (nWorkers - 1) $ statusAccum <> st


producerProc = proc "rake" ["parallel:db:validate:producer"]
workerProc   = proc "rake" ["parallel:db:validate:worker"]

{- producerProc = proc "./Producer" [] -}
{- workerProc   = proc "./Worker" [] -}

loopUntil :: IO Bool -> IO () -> IO ()
loopUntil condAction go = do
  cond <- condAction
  unless cond $ go >> loopUntil condAction go


producer :: MVar (Maybe String) -> MVar () -> IO ()
producer request finishedProducing = do
  (_, Just hout, _, _) <- createProcess producerProc{ std_out = CreatePipe }
  loopUntil (hIsEOF hout) $ do
    {- putStrLn "producer: reading a line from producer..." -}
    req <- hGetLine hout
    {- putStrLn $ "producer: producing " ++ req -}
    putMVar request $ Just req
  {- putStrLn "producer: finished producing" -}
  putMVar finishedProducing ()
  {- putStrLn "producer: finished producing has been read" -}


worker :: MVar (Maybe String) -> MVar ExitCode -> IO ()
worker request status = do
  (Just hin, Just hout, _, processHandle) <- createProcess workerProc{ std_in = CreatePipe, std_out = CreatePipe }
  hSetBuffering hin LineBuffering
  forkIO $ loopUntil (hIsEOF hout) $ putStrLn =<< hGetLine hout -- just output to STDOUT for now
  forever $ do
    maybeRequest <- takeMVar request
    case maybeRequest of
      Just req -> hPutStrLn hin req
      Nothing -> do
        {- putStrLn "worker: no more requests, closing the worker in pipe..." -}
        hClose hin
        {- putStrLn "worker: closed the pipe, waiting for process to terminate..." -}
        st <- waitForProcess processHandle
        putStrLn $ "worker: process finished with status: " ++ (show st)
        putMVar status st


