-- Composable NWay Swap Test

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.TxEvent
import Control.Concurrent.TxEvent.NWaySChan
import Control.Concurrent.TxEvent.IO

import System.IO

doNWaySwap :: Show a => MVar () -> (String -> Evt ()) -> NWaySChan a -> a -> IO ()
doNWaySwap wait putStrLnEvt c v = do
  forkIO $ do
    thid <- myThreadId
    sync $ do 
      vs <- swapEvt c v
      putStrLnEvt $ (show thid) ++ ": " 
                      ++ "sent " ++ (show v) ++ ", " 
                      ++ "recvd " ++ (show vs)
    putMVar wait ()
  return ()

main :: IO ()
main = do
  wait <- newEmptyMVar
  stdoutE <- mkEvtHandle stdout
  let putStrLnEvt = hPutStrLnEvt stdoutE
  let n = 3
  let l = [1..2*n]
  c <- sync $ newNWaySChan n
  mapM_ (doNWaySwap wait putStrLnEvt c) l
  mapM_ (\x -> takeMVar wait) l
  threadDelay 1000