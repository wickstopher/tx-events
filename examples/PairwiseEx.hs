-- Example of synchronization threads that are pairwise coherent but
-- not deeply coherent.

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.TxEvent

main = do
  w <- newEmptyMVar
  c <- sync newSChan
  k <- sync newSChan
  forkIO $ let loop 0 = putMVar w ()
               loop n = do sync $ do
                             sendEvt c n
                             sendEvt c (n - 1)
                           loop (n - 2) in
           loop 200
  forkIO $ let loop = do (i,j) <- sync $ do
                                    i <- recvEvt c
                                    j <- chooseEvt (do sendEvt k i
                                                       j <- recvEvt k
                                                       return j)
                                                   (do j <- recvEvt k
                                                       sendEvt k i
                                                       return j)
                                    return (i, j)
                         putStrLn ("First thread recvd: " ++ (show (i,j)))
                         loop in
           loop
  forkIO $ let loop = do (i,j) <- sync $ do
                                    i <- recvEvt c
                                    j <- chooseEvt (do sendEvt k i
                                                       j <- recvEvt k
                                                       return j)
                                                   (do j <- recvEvt k
                                                       sendEvt k i
                                                       return j)
                                    return (i, j)
                         putStrLn ("Second thread recvd: " ++ (show (i,j)))
                         loop in
           loop
  takeMVar w
