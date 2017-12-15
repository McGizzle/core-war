{-# LANGUAGE OverloadedStrings #-}
module MARS where
import System.Console.ANSI
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
import Data.Map.Lazy as MapL
import Data.List as DL
import Data.List.Index
import Data.Either
import Parser
import Utils
import Instructions

run :: [Program] -> IO ()
run progs = do
  queue <- liftIO $ atomically newTQueue
  threads <- liftIO $ atomically $ newTVar [] 
  mem <- liftIO $ atomically $ initMem $ indexed progs
  ids <- mapM (\ (c,(i,p)) -> 
         forkFinally  
           (runThread c queue mem (i*1000)) 
           (\ _ -> do
            myId <- myThreadId
            print $ show myId ++ " died.")) 
         (zip colours $ zip [0..] progs) 
  threadDelay 5000000
  finalMem <- atomically $ readTVar mem
  print "Final Memory is:"
  sequence $ MapL.map print finalMem 
  return ()

addToQueue :: TVar [ThreadId] -> TQueue ThreadId -> UTCTime -> IO ()
addToQueue threads queue endTime = do
  ids <- atomically $ readTVar threads
  mapM_ (liftIO . atomically . writeTQueue queue) ids
  time <- getCurrentTime
  unless (time >= endTime) $ addToQueue threads queue endTime 
  return ()

runThread :: Color -> TQueue ThreadId -> Memory -> ProgramCounter -> IO () 
runThread c q mem pc = do
  mem' <- liftIO $ atomically $ readTVar mem
  let info = MarsData {
      queue = q,
      memory = mem,
      memSize = Map.size mem',
      colour  = c
  }
  myId <- liftIO myThreadId 
  liftIO $ print $ "Starting: " ++ show myId
  (w,log) <- runWriterT (runStateT (runStateT (runReaderT runProg info) [pc]) I0)
  putStrLn $ log ++ show w
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
   
