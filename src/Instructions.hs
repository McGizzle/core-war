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
import Utils
import Parser

runInstruction :: AppT Bool
runInstruction = do
  ins <- nextInstruction
  lift $ lift $ put ins
  lift $ lift $ lift $ tell $ "Executing Instruction: " ++ show ins 
  res <- matchIns ins
  endTask
  return res

nextInstruction :: AppT Instruction
nextInstruction = do
  pc <- getPc
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
  nextIns <- readMemory $ Map.lookup (pc + a)
  nextB <- getBField (fromJust nextIns) 
  curIns <- lift $ lift get
  curB <- getBField curIns
  return (nextB + curB)
matchField (Immediate a)     = matchField $ Direct a 
matchField (AutoDecrement a) = matchField $ Indirect (a-1) 

getBField :: Instruction -> AppT Int
getBField (I3 _ _ b) = matchField b   
getBField (I2 _ b)   = matchField b
getBField _          = return 0

--updateBField :: Int -> Instruction -> AppT Instruction
updateBField mod (I3 x y b) = return $ I3 x y (modField mod b)
updateBField mod (I2 x b) = return $ I2 x (modField mod b) 

modField :: (t -> a) -> AddrMode t -> AddrMode a
modField mod (Direct a) = Direct (mod a)
modField mod (Indirect a) = Indirect (mod a)
modField mod (Immediate a) = Immediate (mod a)
modField mod (AutoDecrement a) = AutoDecrement (mod a)

mov :: Field -> Field -> AppT Bool
mov a b = do 
  a' <- matchField a
  b' <- matchField b
  aConts <- readMemory $ Map.lookup a'
  writeMemory $ Map.insert b' (fromJust aConts) 
  updatePc
  return True

add :: Field -> Field -> AppT Bool
add = replace (+) 

sub :: Field -> Field -> AppT Bool
sub = replace (-)

replace mod a b = do
  a' <- matchField a
  b' <- matchField b
  ins <- readMemory $ Map.lookup b'
  newIns <- updateBField (mod a') (fromJust ins)
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
