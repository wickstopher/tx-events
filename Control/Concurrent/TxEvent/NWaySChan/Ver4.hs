-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent.NWaySChan.Ver4
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
-- This library improves upon
-- "Control.Concurrent.TxEvent.NWaySChan.Ver3", by using a helper thread
-- to limit the possible interleavings of communications.  The
-- downside is that 'newNWaySChan' must be an 'IO' action.
--
-----------------------------------------------------------------------------

module Control.Concurrent.TxEvent.NWaySChan.Ver4 (
  -- * Synchronous n-way channels
    NWaySChan     -- abstract
  , newNWaySChan  -- :: Int -> IO (NWaySChan a)
  , swapEvt       -- :: NWaySChan a -> a -> Evt [a]
  ) where

import Prelude
import Control.Monad
import Control.Concurrent
import Control.Concurrent.TxEvent

----------------------------------------------------------------------
----------------------------------------------------------------------

newtype NWaySChan a = NWaySChan (SChan (ThreadId, a, SChan [a]))
{- ^
An `NWaySChan` is a synchronous n-way channel, used for communication
between concurrent threads.  Message passing is synchronous: /n/
distinct threads must be ready to communicate; each thread sends one
value and receives /n-1/ values.
-}

{-|
Create a new n-way synchronous channel.
-}
newNWaySChan :: Int -> IO (NWaySChan a)
newNWaySChan n = do
  ch <- sync $ newSChan
  let loopSend xs outChs =
          case outChs of
            [] -> return ()
            outCh:outChs -> do
              sendEvt outCh (tail xs)
              loopSend ((tail xs) ++ [head xs]) outChs
  let loopRecv 0 tid xs outChs = loopSend xs outChs
      loopRecv n tid xs outChs = do
         (tid', x', outCh') <- recvEvt ch
         if tid > (Just tid')
            then neverEvt
            else loopRecv (n - 1) (Just tid') (x' : xs) (outCh' : outChs)
  let loopIO = do
        sync $ loopRecv n Nothing [] []
        loopIO
  forkIO $ loopIO
  return (NWaySChan ch)

{-|
Send a value on the channel and receive /n-1/ values on the channel.
-}
swapEvt :: NWaySChan a -> a -> Evt [a]
swapEvt (NWaySChan ch) x = do
  tid <- myThreadIdEvt
  outCh <- newSChan
  sendEvt ch (tid,x,outCh)
  recvEvt outCh
