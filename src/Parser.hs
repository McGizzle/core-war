module Parser where
import System.IO
import Data.List.Split
import Data.Char
import Data.Functor

data AddrMode a = Direct a
                | Indirect a 
                | Immediate a
                | AutoDecrement a 
  deriving(Show,Read) 

instance Functor AddrMode where
    fmap f (Direct a)        = Direct (f a)
    fmap f (Indirect a)      = Indirect (f a)
    fmap f (Immediate a)     = Immediate (f a)
    fmap f (AutoDecrement a) = AutoDecrement (f a)


swap (Indirect a)      = Direct a
swap (Immediate a)     = Direct a
swap (AutoDecrement a) = Direct a


type Field = AddrMode Int

data OpCode = DAT
            | MOV
            | ADD
            | SUB
            | JMP
            | JMZ
            | DJN
            | CMP
            | SPL
  deriving (Show,Read)

data Instruction = I3 OpCode Field Field
                 | I2 OpCode Field
                 | I1 OpCode
                 | I0
  deriving(Read,Show)

type Program = [Instruction]

readAddr :: String -> Field
readAddr ('$':a) = Direct (read a)
readAddr ('@':a) = Indirect (read a)
readAddr ('#':a) = Immediate (read a)
readAddr ('<':a) = AutoDecrement (read a)
readAddr a       = Direct (read a)

filterComments :: String -> String
filterComments = head . splitOn ";"

parseLine :: [String] -> Instruction
parseLine []      = I0
parseLine [a]     = I1 (read a)
parseLine [a,b]   = I2 (read a) (readAddr b)  
parseLine [a,b,c] = I3 (read a) (readAddr $ init b) (readAddr c)

parseProg :: FilePath -> IO Program
parseProg f = do
  conts <- readFile f
  return $ parseLine <$> words <$> filterComments <$> lines (toUpper <$> conts)
