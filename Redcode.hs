module Redcode where

type Field = (AddrMode,Value)

data AddrMode = Direct
              | Indirect
              | Immediate
              | AutoDecrement
              
data OpCode = DAT
            | MOV
            | ADD
            | SUB
            | JMP
            | JMZ
            | DJN
            | CMP
            | SPL
  deriving Read

type Instruction = (OpCode,Field)

type Program = [Instruction]

readMode :: String -> AddrMode
readMode ""  = Direct
readMode "$" = Direct
readMode "@" = Indirect
readMode "#" = Immediate
readMode "<" = AutoDecrement

getProg :: FilePath -> IO Program
getProg = fmap words . lines . hGetContents . readFile
