-- Composable Tri Swap Test

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.TxEvent
import Control.Concurrent.TxEvent.TriSChan

import Debug.Trace

doTriSwap :: Show a => MVar () -> TriSChan a -> TriSChan a -> a -> IO ()
doTriSwap wait c1 c2 v = do
  forkIO $ do
    (lh,vs) <- sync (neverEvt
                     `chooseEvt`
                     (do p <- swapEvt c1 v
                         return (True,p))
                     `chooseEvt`
                     (do p <- swapEvt c2 v
                         return (False,p)))
    thid <- myThreadId
    putTraceMsg $ (show thid) ++ ": " 
                    ++ "sent " ++ (show v) ++ ", " 
                    ++ "recvd " ++ (show vs) ++ ", " 
                    ++ "on " ++ (if lh then "c1" else "c2")
    putMVar wait ()
  return ()

doGroup :: IO ()
doGroup = do
  wait <- newEmptyMVar
  let l = [1..300]
  (c1,c2) <- sync $ do 
    c1 <- newTriSChan
    c2 <- newTriSChan
    return (c1,c2)
  mapM_ (doTriSwap wait c1 c2) l
  mapM_ (\x -> takeMVar wait) l

main :: IO ()
main = mapM_ (\ _ -> doGroup) [1..1]
