>{-# LANGUAGE OverloadedStrings #-}
>module MARS where
>import System.Console.ANSI
>import Control.Monad
>import Control.Monad.Reader
>import Control.Monad.State
>import Control.Monad.Writer
>import Control.Concurrent
>import Control.Concurrent.MVar
>import Control.Concurrent.STM
>import Control.Concurrent.STM.TVar
>import Control.Concurrent.STM.TQueue
>import Data.Time
>import Data.Time.Clock
>import Data.Map as Map
>import Data.List as DL
>import Data.List.Index
>import Data.Either
>import RedCode 
>import Utils
>import Instructions


The Meaty part of the program (it isnt really so meaty)

Here we load the memory and the fork off all the necessary threads.

>run :: [Program] -> IO ()
>run progs = do
>  mem <- atomically $ initMem $ indexed progs
>  printPretty Green "The war is starting! \n"
>  pLock <- newMVar 0
>  ids <- mapM (\ (c,(i,p)) -> 
>         forkFinally  
>           (runThread pLock c mem (i*1000)) 

When the thread dies from hitting a DAT, lets let the players know

>           (\ _ -> do
>            myId <- myThreadId
>            printPretty Red $ show myId ++ " died.")) 
>         (zip colours $ zip [0..] progs) 

Lets set the game limit to 15 seconds, cause I said so

>  threadDelay 15000000
>  printPretty Green "\nThe war is over! Any still running threads are declared winners! If they are all losers then the last thread to die is declared best loser."
>  return ()

>runThread :: MVar Int -> Color -> Memory -> ProgramCounter -> IO () 
>runThread lock c mem pc = do
>  mem' <- liftIO $ atomically $ readTVar mem
>  let info = MarsData {
>      memory = mem,
>      memSize = Map.size mem',
>      colour  = c,
>      turnLock = lock
>  }
>  myId <- liftIO myThreadId 

Lets run our monad stack, initialising all the values

>  runWriterT (runStateT (runStateT (runReaderT runProg info) [pc]) I0)
>  return ()

>runProg :: AppT ()
>runProg = do

To ensure fairness (one turn at a time) lets grab the shared Mvar

>  lock <- asks turnLock
>  liftIO $ takeMVar lock
>  alive <- runInstruction

Dont forget to let go of that MVar

>  liftIO $ putMVar lock 0 
>  when alive runProg

If the instruction returned false, then the thread is gonzo

>  return ()
