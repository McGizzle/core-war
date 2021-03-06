>module Parser where

The code in this module is pretty self-explanatory so I'm gonna let that do the talking for me.

>import System.IO
>import Data.List.Split
>import Data.Char
>import Data.Functor

>import RedCode

>readAddr :: String -> Field
>readAddr ('$':a) = Direct (read a)
>readAddr ('@':a) = Indirect (read a)
>readAddr ('#':a) = Immediate (read a)
>readAddr ('<':a) = AutoDecrement (read a)
>readAddr a       = Direct (read a)

>filterComments :: String -> String
>filterComments = head . splitOn ";"

>parseLine :: [String] -> Instruction
>parseLine []      = I0
>parseLine [a]     = I1 (read a)
>parseLine [a,b]   = I2 (read a) (readAddr b)  
>parseLine [a,b,c] = I3 (read a) (readAddr $ init b) (readAddr c)

>parseProg :: FilePath -> IO Program
>parseProg f = do
>  conts <- readFile f
>  return $ parseLine <$> words <$> filterComments <$> lines (toUpper <$> conts)
