>{-# LANGUAGE DeriveFunctor #-}
>module RedCode where

This module defines all the data types for the instructions, along with a couple handy functions.

>import Data.Functor

>data AddrMode a = Direct a
>                | Indirect a 
>                | Immediate a
>                | AutoDecrement a 

Now we just need to derive a few things to use later
Show -> for printing 
Read -> for parsing
Functor -> So we can use fmap over our data type

>  deriving(Show,Read,Functor) 

This function prevents recursion when executing Indirect addressing mode

>swap (Direct a)        = Direct a
>swap (Indirect a)      = Direct a
>swap (Immediate a)     = Direct a
>swap (AutoDecrement a) = Direct a

>type Field = AddrMode Int

>data OpCode = DAT
>            | MOV
>            | ADD
>            | SUB
>            | JMP
>            | JMZ
>            | DJN
>            | CMP
>            | SPL
>  deriving (Show,Read)

There can be three types of instruction

>data Instruction = I3 OpCode Field Field
>                 | I2 OpCode Field
>                 | I1 OpCode
>                 | I0
>  deriving(Read,Show)

A program is just a list of intructions

>type Program = [Instruction]


