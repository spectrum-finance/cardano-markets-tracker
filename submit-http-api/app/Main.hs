module Main where

import SubmitHttpApi.App
import RIO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  runApp args