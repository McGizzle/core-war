module Utils where

import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Control.Arrow (first)
import Data.Map as Map
import Data.List.Index
import Parser

type AppT = ReaderT MarsData (StateT ProgramCounter (StateT Instruction IO))

type Memory = TVar (Map Int Instruction)

type ProgramCounter = Int

data MarsData = MarsData {
  queue  :: TQueue ThreadId,
  memory :: Memory
}

initMem :: [(Int, Program)] -> STM Memory
initMem progs = newTVar mem 
  where 
    iMem = Map.fromList $ zip [0..8000] (replicate 8000 $ I1 DAT)
    mem = Prelude.foldl1 Map.union (fmap addrProgs progs ++ [iMem])

addrProgs :: (Int,Program) -> Map Int Instruction
addrProgs (i,prog) = Map.fromList $ fmap (\(x,y) -> (i*1000+x,y)) (indexed prog)
  
