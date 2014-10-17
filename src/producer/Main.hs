module Main where

import           Control.Monad (forM_)
import           System.IO     (hClose, stdout)

main = do
  forM_ [1..5000] $ putStrLn . show

  hClose stdout
