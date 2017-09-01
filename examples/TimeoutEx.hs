-- Timeout Example

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.TxEvent

import Debug.Trace

main = do
  w <- newEmptyMVar
  forkIO $ let loop 0 = putMVar w ()
               loop n = do putTraceMsg ("Thread delay: start")
                           sync (timeOutEvt 1000000)
                           putTraceMsg ("Thread delay: end")
                           loop (n - 1) in
           loop 5
  takeMVar w
