{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy (writeFile)
import Data.String.Conversions (cs)
import Path
import Prelude hiding (writeFile)
import System.Process.Typed
import Data.List

-- let dateConfig :: ProcessConfig () () ()
-- dateConfig = setStdin closed
--   $ setStdout closed
--   $ setStderr closed
--   "date"
main :: IO ()
main = do
  a <- readFile "/Users/Sho/out"
  paths <- sequence . fmap parseRelDir . lines . cs $ a
  forM_ ((\a -> (length . (filter ('/'==)) . fromRelDir $ a, a)) <$> paths) (putStrLn . show)
--  forM_ ((sortOn (\a -> fst)))
  forM_ (reverse $ (sortOn fst) ((\a -> (length . (filter ('/'==)) . fromRelDir $ a, a)) <$> paths)) (putStrLn . show)
  runProcess "true" >>= print
  runProcess "false" >>= print
  -- CHECK THAT THE EXIT CODE is a success
  runProcess_ "true"

f1 = do
  (out, err) <- readProcess_ "find $HOME/mnt -type d -name '.*'"
  typedPaths <- sequence $ parseAbsDir <$> (lines . cs $ out)
  forM_ typedPaths $ putStrLn . show
--  forM_ ((\a -> "cd " ++ show a) <$> typedPaths) runProcess
  writeFile "$HOME/result.out" out
  writeFile "$HOME/result.err" err
