:q{-# LANGUAGE OverloadedStrings #-}
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
import Control.Monad.Extra

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
  s <- execParser opts
  return ()
--   ifM (f s) (return ()) ((g s >>= (\a -> either putStrLn (\(s)-> fmap s (\(a,b)-> Sample a b)))) >>= print)

h as = forM as f

g a = do
  (_, o1, _) <- readProcess (shell [i|find #{sampleA a} -type d -maxdepth 1|])
  (_, o2, _) <- readProcess (shell [i|find #{sampleB a} -type d -maxdepth 1|])
  if o1 == o2 then do
     a <- sequence $ parseAbsDir <$> (tail . lines . cs $ o1)
     b <- sequence $ parseAbsDir <$> (tail . lines . cs $ o2)
     return $ Right (a, b)
   else
     return $ Left "diff find $a -type d -maxdepth 1" 
    
f a = do
  (_, o0, _) <- readProcess . findMd5sum $ sampleA a
  print $ trim $ cs o0
  (_, o1, _) <- readProcess . findMd5sum $ sampleB a
  print $ trim $ cs o1
  if o0 == o1 then return True else f3 a

f3 a = do
  (_, o2, _) <- readProcess (shell [i|find #{sampleA a} | wc -l |])
  print $ trim $ cs o2
  (_, o3, _) <- readProcess (shell [i|find #{sampleB a} | wc -l |])
  print $ trim $ cs o3
  return $ o2 == o3
  
opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

f1 = do
  (out, err) <- readProcess_ "find $HOME/mnt -type d -name '.*'"
  typedPaths <- sequence $ parseAbsDir <$> (lines . cs $ out)
  forM_ typedPaths print
--  forM_ ((\a -> "cd " ++ show a) <$> typedPaths) runProcess
  writeFile "$HOME/result.out" out
  writeFile "$HOME/result.err" err
