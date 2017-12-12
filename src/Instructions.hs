{-# LANGUAGE FlexibleContexts #-}
module Instructions where

import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Map as Map
import Data.Maybe
import Utils
import Parser

runInstruction :: AppT Bool
runInstruction = do
  ins <- nextInstruction
  lift $ lift $ put ins 
  matchIns ins

nextInstruction :: AppT Instruction
nextInstruction = do
  pc <- lift get
  ins <- readMemory (Map.lookup pc)
  return $ fromJust ins
 
readMemory :: (MonadReader MarsData m, MonadIO m) => (Map Int Instruction -> b) -> m b
readMemory op = do
  mem <- asks memory
  mem' <- liftIO $ atomically $ readTVar mem
  return $ op mem'

writeMemory op = do
  mem <- asks memory
  liftIO $ atomically $ modifyTVar' mem op
  return ()

matchIns :: Instruction -> AppT Bool
matchIns I0          = return False
matchIns (I1 op)     = matchOp0 op
matchIns (I2 op a)   = matchOp1 op a
matchIns (I3 op a b) = matchOp2 op a b

matchOp0 DAT = return False
matchOp1 DAT a = return False
matchOp1 SPL a = return False
matchOp1 JMP a = jmp a 
matchOp2 MOV a b = mov a b
matchOp2 CMP a b = return False
matchOp2 ADD a b = return False
matchOp2 SUB a b = return False
matchOp2 JMZ a b = jmz a b
matchOp2 DJN a b = djn a b

matchField :: Field -> AppT Int
matchField (Direct a)  = do
  pc <- lift get
  return $ pc + a
matchField (Indirect a) = do 
  pc <- lift get
  nextIns <- readMemory $ Map.lookup (pc + a)
  nextB <- getBField (fromJust nextIns) 
  curIns <- lift $ lift get
  curB <- getBField curIns
  return (nextB + curB)
matchField (Immediate a)     = return a 
matchField (AutoDecrement a) = matchField $ Indirect (a-1) 

getBField :: Instruction -> AppT Int
getBField (I3 _ _ b)  = return 0   
getBField I0          = return 0

mov :: Field -> Field -> AppT Bool
mov a b = do 
  a' <- matchField a
  b' <- matchField b
  aConts <- readMemory $ Map.lookup a'
  writeMemory $ Map.insert b' (fromJust aConts) 
  return True

add :: Field -> Field -> AppT Bool
add a b = return True

jmp :: Field -> AppT Bool
jmp a = do
  addr <- matchField a
  lift $ put addr
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
  when (cnd b') $ do
    a' <- matchField a
    lift $ put a'
  return True

