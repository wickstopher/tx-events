-- Guarded read example.

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.TxEvent

import Debug.Trace

grecvEvt :: (a -> Bool) -> SChan a -> Evt a
grecvEvt g c = do
  x <- recvEvt c
  if g x then return x else neverEvt

main = do
  w <- newEmptyMVar
  c <- sync newSChan
  forkIO $ let loop 0 = putMVar w ()
               loop n = do sync (sendEvt c n)
                           putTraceMsg ("Source thread sent: " ++ (show n))
                           loop (n - 1) in
           loop 200
  forkIO $ let loop = do i <- sync (grecvEvt (\ i -> i `mod` 2 == 0) c)
                         putTraceMsg ("Even thread recvd: " ++ (show i))
                         loop in
           loop
  forkIO $ let loop = do i <- sync (grecvEvt (\ i -> i `mod` 2 == 1) c)
                         putTraceMsg ("Odd thread recvd: " ++ (show i))
                         loop in
           loop
  takeMVar w
