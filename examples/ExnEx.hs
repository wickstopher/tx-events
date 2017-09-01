-- Exception Example

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.TxEvent

import Control.Exception

import Debug.Trace

main = do
  w <- newEmptyMVar
  c <- sync newSChan
  forkIO $ let loop 0 = putMVar w ()
               loop n = do sync $ do 
                             sendEvt c n
                             sendEvt c (n - 1)
                             sendEvt c (n - 2)
                           putTraceMsg ("Source thread sent: " ++ (show n))
                           loop (n - 3) in
           loop 21
  forkIO $ let loop = do 
                 v <- sync $ do
                        catchEvt (do v1 <- recvEvt c
                                     if v1 < 10
                                        then throw (AssertionFailed "foo")
                                        else do v2 <- recvEvt c
                                                v3 <- recvEvt c
                                                alwaysEvt (v1, v2, v3))
                                 (\ exn -> do
                                    v2 <- recvEvt c
                                    v3 <- recvEvt c
                                    alwaysEvt (-1,v2,v3))
                 putTraceMsg ("Sync returned ==> " ++ (show v))
                 loop in
           loop
  takeMVar w
