module Main where

import           Control.Monad (forever, unless)
import           System.IO     (isEOF)

loopUntil :: IO Bool -> IO () -> IO ()
loopUntil condAction go = do
  cond <- condAction
  unless cond $ go >> loopUntil condAction go

main = loopUntil isEOF $ do
      req <- getLine
      putStrLn $ "response to: " ++ req

{- main = do -}
  {- eof <- isEOF -}
  {- case eof of -}
    {- False -> do -}
      {- req <- getLine -}
      {- putStrLn $ "response to: " ++ req -}
      {- main -}
    {- True -> return () -}


