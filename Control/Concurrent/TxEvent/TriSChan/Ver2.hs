-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent.TriSChan.Ver2
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

module Control.Concurrent.TxEvent.TriSChan.Ver2 (
  -- * Synchronous 3-way channels
    TriSChan     -- abstract
  , newTriSChan  -- :: Evt (TriSChan a)
  , swapEvt      -- :: TriSChan a -> a -> Evt (a, a)
  ) where

import Control.Concurrent.TxEvent

----------------------------------------------------------------------
----------------------------------------------------------------------

newtype TriSChan a = TriSChan (SChan (a, SChan (a, a)))
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
            (x2, replyCh2) <- recvEvt ch
            (x3, replyCh3) <- recvEvt ch
            sendEvt replyCh2 (x3, x1)
            sendEvt replyCh3 (x1, x2)
            alwaysEvt (x2, x3)

          client = do
            replyCh <- newSChan
            sendEvt ch (x1, replyCh)
            recvEvt replyCh
