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

runInstruction :: AppT ()
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

matchIns :: Instruction -> AppT ()
matchIns I0          = return ()
matchIns (I1 op)     = matchOp0 op
matchIns (I2 op a)   = matchOp1 op a
matchIns (I3 op a b) = matchOp2 op a b

matchOp0 :: OpCode -> AppT ()
matchOp0 DAT = return ()

matchOp1 DAT a = return ()
matchOp1 JMP a = return ()
matchOp1 SPL a = return ()

matchOp2 MOV a b = return ()
matchOp2 CMP a b = return ()
matchOp2 ADD a b = return ()
matchOp2 SUB a b = return ()
matchOp2 JMZ a b = return ()
matchOp2 DJN a b = return ()

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
  aAddr <- matchField a
  bAddr <- matchField b
  aConts <- readMemory $ Map.lookup aAddr
  writeMemory $ Map.insert bAddr (fromJust aConts) 
  return True

add :: Field -> Field -> AppT Bool
add a b = return True

jmp :: Field -> AppT Bool
jmp a = do
  addr <- matchField a
  lift $ put addr
  return True

