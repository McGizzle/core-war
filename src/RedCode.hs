{-# LANGUAGE DeriveFunctor #-}
module RedCode where

import Data.Functor

data AddrMode a = Direct a
                | Indirect a 
                | Immediate a
                | AutoDecrement a 
  deriving(Show,Read,Functor) 

swap (Direct a)        = Direct a
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


