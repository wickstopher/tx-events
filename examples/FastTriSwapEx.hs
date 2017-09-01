-- Composable Tri Swap Test

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.TxEvent
import Control.Concurrent.TxEvent.TriSChan

import Debug.Trace

doTriSwap :: Show a => MVar () -> TriSChan a -> a -> IO ()
doTriSwap wait c v = do
  forkIO $ do
    vs <- sync $ swapEvt c v
    thid <- myThreadId
    putTraceMsg $ (show thid) ++ ": " 
                    ++ "sent " ++ (show v) ++ ", " 
                    ++ "recvd " ++ (show vs)
    putMVar wait ()
  return ()

main :: IO ()
main = do
  wait <- newEmptyMVar
  let l = [1..3*100]
  c <- sync $ newTriSChan
  mapM_ (doTriSwap wait c) l
  mapM_ (\x -> takeMVar wait) l
  putTraceMsg $ "done."
