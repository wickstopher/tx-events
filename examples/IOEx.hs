-- Event IO Example.

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.TxEvent
import Control.Concurrent.TxEvent.IO

import System.IO

doEvtIO :: Show a => MVar () -> (String -> Evt ()) -> a -> IO ()
doEvtIO wait putStrLnEvt v = do
  forkIO $ do
    thid <- myThreadId
    sync $ do
      putStrLnEvt $ (show thid) ++ ": " ++ (show v)
      putStrLnEvt $ (show thid) ++ ": " ++ (show v)
    putMVar wait ()
  return ()

main :: IO ()
main = do
  wait <- newEmptyMVar
  stdoutE <- mkEvtHandle stdout
  let putStrLnEvt = hPutStrLnEvt stdoutE
  let l = [1..10]
  mapM_ (doEvtIO wait putStrLnEvt) l
  mapM_ (\ x -> takeMVar wait) l
