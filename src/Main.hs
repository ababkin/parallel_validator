module Main where

import           Control.Applicative ((<$>))
import           Control.Concurrent  (MVar (..), forkIO, newEmptyMVar, putMVar,
                                      takeMVar)
import           Control.Monad       (forM, forever, replicateM, replicateM_,
                                      unless)
import           Data.Foldable       (fold)
import           Data.Maybe          (Maybe (..), maybe)
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
  exitWith =<< (fmap fold $ replicateM nWorkers $ putMVar request Nothing >> takeMVar status)

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
  loopUntil (hIsEOF hout) $ (Just <$> hGetLine hout) >>= putMVar request
  putMVar finishedProducing ()


worker :: MVar (Maybe String) -> MVar ExitCode -> IO ()
worker request status = do
  (Just hin, Just hout, _, processHandle) <- createProcess workerProc{ std_in = CreatePipe, std_out = CreatePipe }
  hSetBuffering hin LineBuffering
  forkIO $ loopUntil (hIsEOF hout) $ putStrLn =<< hGetLine hout -- just output to STDOUT for now
  forever $ do
    maybeRequest <- takeMVar request
    maybe
      (hClose hin >> waitForProcess processHandle >>= putMVar status)
      (hPutStrLn hin)
      maybeRequest


