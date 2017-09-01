
import Control.Concurrent
import Control.Concurrent.MVar

import System.Environment

import Control.Concurrent.TxEvent.CML.Full

-- Misc
forkIO_ :: IO () -> IO ()
forkIO_ act = forkIO act >> return ()

pong :: SChan () -> IO ()
pong ch = 
    let loop = do
          sync $ recvEvt ch
          loop in
    forkIO_ $ loop
  
ping :: MVar () -> SChan () -> Int -> IO ()
ping wait ch n =
    let loop i =
            if i > n
               then putMVar wait ()
               else do sync $ sendEvt ch ()
                       loop (i + 1) in
    forkIO_ $ loop 0

doit :: Int -> IO ()
doit n = do
  wait <- newEmptyMVar
  ch <- newSChan
  pong ch
  ping wait ch n
  takeMVar wait

main :: IO ()
main = do
  args <- getArgs
  let n = case args of
            [] -> 100
            s:_ -> case reads s of
                     [(n,"")] -> n
                     _ -> 100
  doit n

