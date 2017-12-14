{-# LANGUAGE FlexibleContexts #-}
module Instructions where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Map as Map
import Data.Maybe
import Data.Function.Between.Lazy
import Utils
import Parser

runInstruction :: AppT Bool
runInstruction = do
  ins <- nextInstruction
  lift $ lift $ put ins
  id <- liftIO myThreadId
  lift $ lift $ lift $ tell $ show id ++" Executing Instruction: "++ show ins ++"\n" 
  res <- matchIns ins
  endTask
  return res

nextInstruction :: AppT Instruction
nextInstruction = do
  pc <- getPc
  readMemory pc
  
readMemory :: Int -> AppT Instruction
readMemory key = do
  mem <- asks memory
  size <- asks memSize
  mem' <- liftIO $ atomically $ readTVar mem
  return $ fromJust $ Map.lookup (key `mod` size) mem'

writeMemory :: Int -> Instruction -> AppT ()
writeMemory key ins = do
  mem <- asks memory
  size <- asks memSize
  liftIO $ atomically $ modifyTVar' mem $ Map.insert (key `mod` size) ins
  return ()

matchIns :: Instruction -> AppT Bool
matchIns I0          = return True
matchIns (I1 op)     = matchOp0 op
matchIns (I2 op a)   = matchOp1 op a
matchIns (I3 op a b) = matchOp2 op a b

matchOp0 DAT = return False
matchOp1 DAT _ = return False
matchOp1 SPL a = spl a 
matchOp1 JMP a = jmp a 
matchOp2 MOV a b = mov a b
matchOp2 CMP a b = cmp a b
matchOp2 ADD a b = add a b 
matchOp2 SUB a b = sub a b
matchOp2 JMZ a b = jmz a b
matchOp2 DJN a b = djn a b

matchField :: Field -> AppT Int
matchField (Direct a)  = do
  pc <- getPc
  return $ pc + a
matchField (Indirect a) = do 
  pc <- getPc
  ins <- lift $ lift get 
  matchField $ getBField ins 
matchField (Immediate a)     = return a 
matchField (AutoDecrement a) = matchField $ Indirect (a-1) 

getBField :: Instruction -> AddrMode Int
getBField (I3 _ _ b) = swap b   
getBField (I2 _ b)   = swap b
getBField _          = Direct 0

updateBField :: (Int -> Int) -> Instruction -> AppT Instruction
updateBField mod (I3 x y b) = return $ I3 x y (fmap mod b)
updateBField mod (I2 x b) = return $ I2 x (fmap mod b) 

mov :: Field -> Field -> AppT Bool
mov a b = do 
  a' <- matchField a
  b' <- matchField b
  aConts <- readMemory a'
  writeMemory b' aConts 
  updatePc
  return True

add :: Field -> Field -> AppT Bool
add = replace (+) 

sub :: Field -> Field -> AppT Bool
sub = replace (-)

replace mod a b = do
  a' <- matchField a
  b' <- matchField b
  ins <- readMemory b'
  newIns <- updateBField (mod a') ins
  updatePc
  return True

cmp :: Field -> Field -> AppT Bool
cmp a b = do
  a' <- matchField a
  b' <- matchField b
  pc <- getPc
  when (a' /= b') updatePc
  updatePc 
  return True

jmp :: Field -> AppT Bool
jmp a = do
  a' <- matchField a
  putPc a'
  return True

spl :: Field -> AppT Bool
spl a = do
  a' <- matchField a
  addTask a'
  updatePc
  return True

jmz :: Field -> Field -> AppT Bool
jmz = cndJmp (== 0)

jmn :: Field -> Field -> AppT Bool
jmn = cndJmp (/= 0)

djn :: Field -> Field -> AppT Bool
djn = cndJmp (\ x -> (x - 1) == 0) 

cndJmp :: (Int -> Bool) -> Field -> Field -> AppT Bool
cndJmp cnd a b = do
  b' <- matchField b
  if cnd b' then do
    a' <- matchField a
    putPc a'
  else updatePc
  return True

