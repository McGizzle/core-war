module MARS where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Data.Time
import Data.Time.Clock
import Data.Map as Map
import Data.List as DL
import Data.List.Index
import Data.Either
import Parser
import Utils
import Instructions

run :: [Program] -> IO ()
run progs = do
  now <- getCurrentTime
  let day = fromInteger 86400 :: NominalDiffTime
  let endTime = addUTCTime day now
  queue <- liftIO $ atomically newTQueue
  threads <- liftIO $ atomically $ newTVar [] 
  mem <- liftIO $ atomically $ initMem $ indexed progs
  ids <- mapM (\ (i,p) -> 
         forkFinally  
           (runThread queue mem (i*1000)) 
           (\ _ -> print "Thread died")) 
         (zip [0..] progs) 
  liftIO $ atomically $ writeTVar threads ids
  addToQueue threads queue endTime

addToQueue :: TVar [ThreadId] -> TQueue ThreadId -> UTCTime -> IO ()
addToQueue threads queue endTime = do
  ids <- atomically $ readTVar threads
  mapM_ (liftIO . atomically . writeTQueue queue) ids
  time <- getCurrentTime
  unless (time >= endTime) $ addToQueue threads queue endTime 
  return ()

runThread :: TQueue ThreadId -> Memory -> ProgramCounter -> IO () 
runThread q mem pc = do
  let info = MarsData {
      queue = q,
      memory = mem
  }
  (_,log) <- runWriterT (runStateT (runStateT (runReaderT runProg info) [pc]) I0)
  putStrLn log
  return ()

runProg :: AppT ()
runProg = do
  --checkQueue
  alive <- runInstruction
  when alive runProg
  return ()

checkQueue :: AppT ()
checkQueue = do
  q <- asks queue
  id <- liftIO $ atomically $ peekTQueue q
  myId <- liftIO myThreadId
  unless (myId == id) checkQueue
  
