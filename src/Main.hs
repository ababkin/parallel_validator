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
                                      createProcess, shell, waitForProcess)
import System.Environment (getArgs)

{- nWorkers = 24 -}
{- "rake parallel:db:validate" -}
{- "rake parallel:db:validate:worker" -}

instance Monoid ExitCode where
  mempty = ExitSuccess
  ec@(ExitFailure n) `mappend` _ = ec
  _ `mappend` ec = ec

main = do
  args <- getArgs
  case args of
    producerCommand:workerCommand:nWorkersStr:[] -> do
      let nWorkers = read nWorkersStr
      request           <- newEmptyMVar
      status            <- newEmptyMVar
      finishedProducing <- newEmptyMVar
      forkIO $ producer producerCommand request finishedProducing
      replicateM_ nWorkers $ forkIO $ worker workerCommand request status

<<<<<<< HEAD
  takeMVar finishedProducing
  putStrLn "main: finished producing"
  exitWith =<< (fmap fold $ replicateM nWorkers $ putMVar request Nothing >> takeMVar status)
=======
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

    _ -> putStrLn "Usage: ParallelValidator \"<producer command>\" \"<worker command>\" <size of worker pool>"
>>>>>>> parametrize commands and number of workers


{- producerProc = proc "./Producer" [] -}
{- workerProc   = proc "./Worker" [] -}

loopUntil :: IO Bool -> IO () -> IO ()
loopUntil condAction go = do
  cond <- condAction
  unless cond $ go >> loopUntil condAction go


<<<<<<< HEAD
producer :: MVar (Maybe String) -> MVar () -> IO ()
producer request finishedProducing = do
  (_, Just hout, _, _) <- createProcess producerProc{ std_out = CreatePipe }
  loopUntil (hIsEOF hout) $ (Just <$> hGetLine hout) >>= putMVar request
=======
producer :: String -> MVar (Maybe String) -> MVar () -> IO ()
producer producerCommand request finishedProducing = do
  (_, Just hout, _, _) <- createProcess (shell producerCommand){ std_out = CreatePipe }
  loopUntil (hIsEOF hout) $ do
    {- putStrLn "producer: reading a line from producer..." -}
    req <- hGetLine hout
    {- putStrLn $ "producer: producing " ++ req -}
    putMVar request $ Just req
  {- putStrLn "producer: finished producing" -}
>>>>>>> parametrize commands and number of workers
  putMVar finishedProducing ()


worker :: String -> MVar (Maybe String) -> MVar ExitCode -> IO ()
worker workerCommand request status = do
  (Just hin, Just hout, _, processHandle) <- createProcess (shell workerCommand){ std_in = CreatePipe, std_out = CreatePipe }
  hSetBuffering hin LineBuffering
  forkIO $ loopUntil (hIsEOF hout) $ putStrLn =<< hGetLine hout -- just output to STDOUT for now
  forever $ do
    maybeRequest <- takeMVar request
    maybe
      (hClose hin >> waitForProcess processHandle >>= putMVar status)
      (hPutStrLn hin)
      maybeRequest


