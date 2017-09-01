-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent.TriSChan.Ver4
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
-- This library improves upon
-- "Control.Concurrent.TxEvent.TriSChan.Ver3", by using a helper thread
-- to limit the possible interleavings of communications.  It uses 'forkEvt'
-- so that 'newTriSChan' may be an 'Evt' action.
--
-----------------------------------------------------------------------------

module Control.Concurrent.TxEvent.TriSChan.Ver5 (
  -- * Synchronous 3-way channels
    TriSChan     -- abstract
  , newTriSChan  -- :: IO (TriSChan a)
  , swapEvt      -- :: TriSChan a -> a -> Evt (a, a)
  ) where

import Control.Concurrent
import Control.Concurrent.TxEvent

----------------------------------------------------------------------
----------------------------------------------------------------------

grecvEvt :: (a -> Bool) -> SChan a -> Evt a
grecvEvt g ch = do
  x <- recvEvt ch
  if g x then return x else neverEvt

----------------------------------------------------------------------
----------------------------------------------------------------------

newtype TriSChan a = TriSChan (SChan (ThreadId, a, SChan (a, a)))
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
  let doEvt = do
        (tid1, x1, replyCh1) <- recvEvt ch
        (tid2, x2, replyCh2) <- grecvEvt (\ (tid2, _, _) -> tid1 > tid2) ch
        (tid3, x3, replyCh3) <- grecvEvt (\ (tid3, _, _) -> tid2 > tid3) ch
        sendEvt replyCh1 (x2, x3)
        sendEvt replyCh2 (x3, x1)
        sendEvt replyCh3 (x1, x2)
        alwaysEvt ()
  let loopIO = do
        sync doEvt
        loopIO
  forkEvt doEvt (\() -> loopIO)
  return (TriSChan ch)

{-|
Send a value on the channel and receive 2 values on the channel.
-}
swapEvt :: TriSChan a -> a -> Evt (a, a)
swapEvt (TriSChan ch) x = do
  tid <- myThreadIdEvt
  replyCh <- newSChan
  sendEvt ch (tid, x, replyCh)
  recvEvt replyCh
