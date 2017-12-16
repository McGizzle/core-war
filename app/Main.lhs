>module Main where
>import System.Environment
>import Parser
>import MARS

Not too much going on in the main
Load the programs and the run MARS

>main :: IO ()
>main = do
>  files <- getArgs
>  progs <- traverse parseProg files
>  run progs

