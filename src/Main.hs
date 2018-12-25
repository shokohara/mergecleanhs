{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy (writeFile)
import Data.String.Conversions (cs)
import Path
import Prelude hiding (writeFile)
import System.Process.Typed
import Data.List
import System.Environment
import System.Exit
import Options.Applicative
import Data.Semigroup ((<>))
import Data.String.Interpolate
import Data.Char (isSpace)
import qualified Data.ByteString.Lazy as B

data Sample = Sample
  { sampleA      :: String
  , sampleB      :: String }
 deriving (Show)

sample :: Parser Sample
sample = Sample
      <$> strOption
      ( long "a"
         <> help "Target for the greeting" )
      <*> strOption
          ( long "b"
         <> help "Whether to be quiet" )

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

findMd5sum x = shell [i|find #{x} | env LC_ALL=C sort | md5sum -b|]

main :: IO ()
main = do
  sample <- execParser opts
  (_, o0, e) <- readProcess . findMd5sum $ sampleA sample
  print $ trim $ cs o0
  (_, o1, e) <- readProcess . findMd5sum $ sampleB sample
  print $ trim $ cs o1
  _ <- if o0 == o1 then return ("", "") else f3 sample
  return ()

f3 s = do
  (_, o2, e) <- readProcess (shell [i|find #{sampleA s} | wc -l |])
  print $ trim $ cs o2
  (_, o3, e) <- readProcess (shell [i|find #{sampleB s} | wc -l |])
  print $ trim $ cs o3
  return (o2, o3)
  
opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

f1 = do
  (out, err) <- readProcess_ "find $HOME/mnt -type d -name '.*'"
  typedPaths <- sequence $ parseAbsDir <$> (lines . cs $ out)
  forM_ typedPaths $ putStrLn . show
--  forM_ ((\a -> "cd " ++ show a) <$> typedPaths) runProcess
  writeFile "$HOME/result.out" out
  writeFile "$HOME/result.err" err
