module Instructions where

import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Map as Map
import Utils
import Parser

nextInstruction :: AppT (Maybe Instruction)
nextInstruction = do
  pc <- lift get
  mem <- asks memory
  mem' <- liftIO $ atomically $ readTVar mem
  return $ Map.lookup pc mem'

runInstruction :: AppT Bool
runInstruction = do
  ins <- nextInstruction
  case ins of
    Nothing   -> return False
    Just ins' -> matchIns ins'

matchIns :: Instruction -> AppT Bool
matchIns I0          = return False
matchIns (I1 op)     = return False
matchIns (I2 op a)   = return False
matchIns (I3 op a b) = return False

