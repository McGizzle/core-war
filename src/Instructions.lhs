>{-# LANGUAGE FlexibleContexts #-}
>module Instructions where

>import Control.Monad.Reader
>import Control.Monad.State
>import Control.Monad.Writer
>import Control.Concurrent
>import Control.Concurrent.STM
>import Control.Concurrent.STM.TVar
>import Data.Map as Map
>import Data.Maybe
>import Data.Function.Between.Lazy
>import Utils
>import RedCode 

Okayyy, lets run an intruction

>runInstruction :: AppT Bool
>runInstruction = do

To make the war more visually appealing, lets delay each thread a little

>  liftIO $ threadDelay 300000
>  ins <- nextInstruction
>  lift $ lift $ put ins
>  id <- liftIO myThreadId
>  printIns ins
>  res <- matchIns ins

Dont forget to move onto the next task (if there is one!)

>  endTask
>  return res

>nextInstruction :: AppT Instruction
>nextInstruction = do
>  pc <- getPc
>  readMemory pc

Helper functions for accessing memory, they could be placed in the Utils, but they are so necessary to this module that 
I decided to leave them here
  
>readMemory :: Int -> AppT Instruction
>readMemory key = do
>  mem <- asks memory
>  size <- asks memSize
>  mem' <- liftIO $ atomically $ readTVar mem

Memory access is computed using modulo of the size

>  return $ fromJust $ Map.lookup (key `mod` size) mem'

>writeMemory :: Int -> Instruction -> AppT ()
>writeMemory key ins = do
>  mem <- asks memory
>  size <- asks memSize

Dont forget the modulo

>  liftIO $ atomically $ modifyTVar' mem $ Map.insert (key `mod` size) ins
>  return ()

Match up the intructions 

>matchIns :: Instruction -> AppT Bool
>matchIns I0          = return True
>matchIns (I1 op)     = matchOp0 op
>matchIns (I2 op a)   = matchOp1 op a
>matchIns (I3 op a b) = matchOp2 op a b

>matchOp0 _       = return True
>matchOp1 DAT _   = return False
>matchOp1 SPL a   = spl a 
>matchOp1 JMP a   = jmp a 
>matchOp2 MOV a b = mov a b
>matchOp2 CMP a b = cmp a b
>matchOp2 ADD a b = add a b 
>matchOp2 SUB a b = sub a b
>matchOp2 JMZ a b = jmz a b
>matchOp2 DJN a b = djn a b

Heres one of the tricker parts, the addressing modes

>matchField :: Field -> AppT Int

If the mode is Direct, lets just add the A-Field to the Program Counter, the glories of the monad stack make this a breeze

>matchField (Direct a)  = do
>  pc <- getPc
>  return $ pc + a

For indriect we fetch the instruction the A-Field points to

>matchField (Indirect a) = do 
>  pc <- getPc
>  ins <- readMemory (pc + a)

But theres a catch! We need to avoid ending up in a never ending recursion of addressing mode matches
For example: What happends if the B-Field of the fetched intruction points to another Indirect?
Well lets not let that happen, so follow the trail to getBField and we shall see what magic happens.

>  matchField $ getBField ins 
>matchField (Immediate a)     = return a 
>matchField (AutoDecrement a) = matchField $ Indirect (a-1) 

Not too much magic unfortunately :(
We use the swap function defined in the RedCode module to force the this function to treat the next AddrMode as Direct

>getBField :: Instruction -> AddrMode Int
>getBField (I3 _ _ b) = swap b   
>getBField (I2 _ b)   = swap b

If theres only one instruction then things get fuzzy in the RedCode spec.
So I decided its my program and I chose to make it easy and just return 0!

>getBField _          = Direct 0

Here we get make use of the Functor we derived.
A simple fmap to unwrap, apply the function, and re-warp. Lovely.

>updateBField :: (Int -> Int) -> Instruction -> AppT Instruction
>updateBField mod (I3 x y b) = return $ I3 x y (fmap mod b)
>updateBField mod (I2 x b) = return $ I2 x (fmap mod b) 

Move:
Copy the complete contents of the location indicated by A into the one idicated by B
Simples.
Unless... Theres a special case for immediate.

>mov :: Field -> Field -> AppT Bool
>mov a b = do 
>  b' <- matchField b
>  case a of

Stick in a DAT if its immediate

>    (Immediate _) -> writeMemory b' $ I2 DAT a
>    _             -> do    
>      a' <- matchField a
>      aConts <- readMemory a'
>      writeMemory b' aConts 
>  updatePc
>  return True

Functions are First-Class :)))))

>add :: Field -> Field -> AppT Bool
>add = arith (+) 

>sub :: Field -> Field -> AppT Bool
>sub = arith (-)

Arithmetic:
Add:
Take the contents of the A-Field and add it to the B-Field
Sub:
Same as above but sure take it away instead!

>arith mod a b = do
>  a' <- matchField a
>  b' <- matchField b
>  ins <- readMemory b'
>  newIns <- updateBField (mod a') ins
>  writeMemory b' newIns
>  updatePc
>  return True

Compare:
Comapre the values indicted by the A and B field

>cmp :: Field -> Field -> AppT Bool
>cmp a b = do
>  a' <- matchField a
>  b' <- matchField b
>  pc <- getPc

If they are equal then update the PC an extra time

>  when (a' /= b') updatePc
>  updatePc 
>  return True

Jump:
Always Jump

>jmp :: Field -> AppT Bool
>jmp a = do
>  a' <- matchField a
>  putPc a'
>  return True

Split:
This function should be tricky to write, but thanks to the monad stack its easy-peasy

>spl :: Field -> AppT Bool
>spl a = do
>  a' <- matchField a

One of the little helpers that takes an address and adds it to the list of Program Counters

>  addTask a'
>  updatePc
>  return True

Functions are First-Class ;)

>jmz :: Field -> Field -> AppT Bool
>jmz = cndJmp (== 0)

>jmn :: Field -> Field -> AppT Bool
>jmn = cndJmp (/= 0)

>djn :: Field -> Field -> AppT Bool
>djn = cndJmp (\ x -> (x - 1) == 0) 

This function handles all the jumping
JMZ: jump if the values idicated by the B-Field and A-Field are zero
JMN: jump if the values idicated by the B-Field and A-Field are non-zero
DJN: weird one this, if the values idicated by the (B-Field - 1) and A-Field are zero

>cndJmp :: (Int -> Bool) -> Field -> Field -> AppT Bool
>cndJmp cnd a b = do
>  b' <- matchField b
>  if cnd b' then do
>    a' <- matchField a
>    putPc a'
>  else updatePc
>  return True

