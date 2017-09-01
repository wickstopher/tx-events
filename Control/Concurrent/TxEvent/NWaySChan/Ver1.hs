-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent.NWaySChan.Ver1
-- Copyright   :  (c) 2006 Kevin Donnelly & Matthew Fluet
-- License     :  BSD3
-- Maintainer  :  Matthew Fluet <Matthew.Fluet@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires TxEvent)
--
-- Synchronous N-Way Channels.
--
-- This library provides synchronous n-way channels, a generalization
-- of `SChan`.
--
-----------------------------------------------------------------------------

module Control.Concurrent.TxEvent.NWaySChan.Ver1 (
  -- * Synchronous n-way channels
    NWaySChan     -- abstract
  , newNWaySChan  -- :: Int -> Evt (NWaySChan a)
  , swapEvt       -- :: NWaySChan a -> a -> Evt [a]
  ) where

import Prelude
import Control.Monad
import Control.Concurrent.TxEvent

----------------------------------------------------------------------
----------------------------------------------------------------------

newtype NWaySChan a = NWaySChan (Int, SChan (SChan a, SChan [a]))
{- ^
An `NWaySChan` is a synchronous n-way channel, used for communication
between concurrent threads.  Message passing is synchronous: /n/
distinct threads must be ready to communicate; each thread sends one
value and receives /n-1/ values.
-}

{-|
Create a new n-way synchronous channel.
-}
newNWaySChan :: Int -> Evt (NWaySChan a)
newNWaySChan n = do
  ch <- newSChan
  return (NWaySChan (n, ch))

{-|
Send a value on the channel and receive /n-1/ values on the channel.
-}
swapEvt :: NWaySChan a -> a -> Evt [a]
swapEvt (NWaySChan (n,ch)) x = leader `chooseEvt` client
    where loopSend xs outChs =
              case outChs of
                [] -> alwaysEvt (tail xs)
                outCh:outChs -> do
                  sendEvt outCh (tail xs)
                  loopSend ((tail xs) ++ [head xs]) outChs

          leader = do
            let l = [2..n]
            inChs <- mapM (\ _ -> newSChan) l
            outChs <- mapM (\ _ -> newSChan) l
            xs <- zipWithM (\ inCh outCh -> do
                              sendEvt ch (inCh, outCh)
                              recvEvt inCh) 
                           inChs outChs
            loopSend (xs ++ [x]) outChs

          client = do
            (inCh, outCh) <- recvEvt ch
            sendEvt inCh x
            recvEvt outCh
