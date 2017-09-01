-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent.TriSChan.Ver0
-- Copyright   :  (c) 2006 Kevin Donnelly & Matthew Fluet
-- License     :  BSD3
-- Maintainer  :  Matthew Fluet <Matthew.Fluet@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires TxEvent)
--
-- Synchronous 3-Way Channels.
--
-- This library provides synchronous 3-way channels, a generalization
-- of `SChan`.
--
-----------------------------------------------------------------------------

module Control.Concurrent.TxEvent.TriSChan.Ver0 (
  -- * Synchronous 3-way channels
    TriSChan     -- abstract
  , newTriSChan  -- :: Evt (TriSChan a)
  , swapEvt      -- :: TriSChan a -> a -> Evt (a, a)
  ) where

import Control.Concurrent.TxEvent

----------------------------------------------------------------------
----------------------------------------------------------------------

newtype TriSChan a = TriSChan (SChan (SChan a, SChan (a, a)))
{- ^
An `TriSChan` is a synchronous 3-way channel, used for communication
between concurrent threads.  Message passing is synchronous: 3
distinct threads must be ready to communicate; each thread sends one
value and receives 2 values.
-}

{-|
Create a new 3-way synchronous channel.
-}
newTriSChan :: Evt (TriSChan a)
newTriSChan = do
  ch <- newSChan
  return (TriSChan ch)

{-|
Send a value on the channel and receive 2 values on the channel.
-}
swapEvt :: TriSChan a -> a -> Evt (a, a)
swapEvt (TriSChan ch) x1 = leader `chooseEvt` client
    where leader = do
            inCh2 <- newSChan
            inCh3 <- newSChan
            outCh2 <- newSChan
            outCh3 <- newSChan
            sendEvt ch (inCh2, outCh2)
            sendEvt ch (inCh3, outCh3)
            x2 <- recvEvt inCh2
            x3 <- recvEvt inCh3
            sendEvt outCh2 (x3, x1)
            sendEvt outCh3 (x1, x2)
            alwaysEvt (x2, x3)

          client = do
            (inCh, outCh) <- recvEvt ch
            sendEvt inCh x1
            recvEvt outCh
