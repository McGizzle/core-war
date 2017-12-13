module Main where
import System.Environment
import Parser
import MARS

main :: IO ()
main = do
  files <- getArgs
  progs <- traverse parseProg files
  run progs

