module MARS where
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Parser

data Data = Data {
  prog :: Program,
  queue :: TQueue ThreadId
}

type AppT = ReaderT Data IO

run :: [Program] -> IO [ThreadId]
run progs = do
  queue <- atomically $ newTQueue
  mapM (\ p -> forkIO $ runThread p queue) progs  

runThread :: Program -> TQueue ThreadId -> IO () 
runThread p q = do
  let info = Data {
      prog = p,
      queue = q
  }
  runReaderT runProg info
  return ()

runProg :: AppT ()
runProg = do
  p <- asks prog
  execute p

execute :: Program -> AppT ()
execute [] = runProg
execute (x:xs) = do
  checkQueue
  liftIO $ print $ show x
  execute xs

checkQueue :: AppT ()
checkQueue = do
  q <- asks queue
  id <- liftIO $ atomically $ peekTQueue q
  myId <- liftIO $ myThreadId
  unless (myId == id) $ checkQueue
  return ()
