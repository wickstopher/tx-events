-- Composable NWay Swap Test

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.TxEvent
import Control.Concurrent.TxEvent.NWaySChan

import Debug.Trace

doNWaySwap :: Show a => MVar () -> NWaySChan a -> a -> IO ()
doNWaySwap wait c v = do
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
  let n = 5
  let l = [1..20*n]
  c <- sync $ newNWaySChan n
  mapM_ (doNWaySwap wait c) l
  mapM_ (\x -> takeMVar wait) l
