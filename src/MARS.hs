module MARS where
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Data.Time
import Data.Map as Map
import Parser

type AppT = ReaderT Data IO
type Memory = TVar (Map Int Int)
data Data = Data {
  prog         :: Program,
  aliveThreads :: TVar [ThreadId],
  queue        :: TQueue ThreadId,
  memory       :: Memory
}

run :: [Program] -> UTCTime -> IO ()
run progs endTime = do
  mem <- liftIO $ atomically $ newTVar Map.empty 
  queue <- liftIO $ atomically newTQueue
  threads <- liftIO $ atomically $ newTVar [] 
  ids <- mapM (\ p -> forkIO $ runThread p queue mem threads) progs
  liftIO $ atomically $ writeTVar threads ids
  addToQueue threads queue endTime

addToQueue :: TVar [ThreadId] -> TQueue ThreadId -> UTCTime -> IO ()
addToQueue threads queue endTime = do
  ids <- atomically $ readTVar threads
  mapM_ (liftIO . atomically . writeTQueue queue) ids
  time <- getCurrentTime
  unless (time >= endTime) $ addToQueue threads queue endTime 
  return ()

runThread :: Program -> TQueue ThreadId -> Memory -> TVar [ThreadId] -> IO () 
runThread p q mem threads = do
  let info = Data {
      prog = p,
      queue = q,
      memory = mem,
      aliveThreads = threads
  }
  runReaderT runProg info
  return ()

runProg :: AppT ()
runProg = do
  p <- asks prog
  loopProg p

loopProg :: Program -> AppT ()
loopProg [] = runProg
loopProg (x:xs) = do
  checkQueue
  liftIO $ print $ show x
  loopProg xs

checkQueue :: AppT ()
checkQueue = do
  q <- asks queue
  id <- liftIO $ atomically $ peekTQueue q
  myId <- liftIO myThreadId
  unless (myId == id) checkQueue
  return ()
