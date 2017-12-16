{-# LANGUAGE OverloadedStrings #-}
module MARS where
import System.Console.ANSI
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Data.Time
import Data.Time.Clock
import Data.Map as Map
import Data.List as DL
import Data.List.Index
import Data.Either
import RedCode 
import Utils
import Instructions

run :: [Program] -> IO ()
run progs = do
  mem <- atomically $ initMem $ indexed progs
  printPretty Green "The war is starting! \n"
  pLock <- newMVar 0
  ids <- mapM (\ (c,(i,p)) -> 
         forkFinally  
           (runThread pLock c mem (i*1000)) 
           (\ _ -> do
            myId <- myThreadId
            printPretty Red $ show myId ++ " died.")) 
         (zip colours $ zip [0..] progs) 
  threadDelay 15000000
  printPretty Green "\nThe war is over!"
  return ()


runThread :: MVar Int -> Color -> Memory -> ProgramCounter -> IO () 
runThread lock c mem pc = do
  mem' <- liftIO $ atomically $ readTVar mem
  let info = MarsData {
      memory = mem,
      memSize = Map.size mem',
      colour  = c,
      turnLock = lock
  }
  myId <- liftIO myThreadId 
  runWriterT (runStateT (runStateT (runReaderT runProg info) [pc]) I0)
  return ()

runProg :: AppT ()
runProg = do
  lock <- asks turnLock
  liftIO $ takeMVar lock
  alive <- runInstruction
  liftIO $ putMVar lock 0 
  when alive runProg
  return ()
