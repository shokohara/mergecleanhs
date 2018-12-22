{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import System.IO
import System.Process.Typed
import Data.String.Conversions (cs)

-- let dateConfig :: ProcessConfig () () ()
-- dateConfig = setStdin closed
--   $ setStdout closed
--   $ setStderr closed
--   "date"

main :: IO ()
main = do
  (out, err) <- readProcess_ "find $HOME/mnt/hdd3 -type d -name '.*'"
  putStrLn $ cs out
  putStrLn $ cs err
  runProcess "true" >>= print
  runProcess "false" >>= print
  -- Check that the exit code is a success
  runProcess_ "true"

