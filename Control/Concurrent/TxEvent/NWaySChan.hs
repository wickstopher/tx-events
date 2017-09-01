-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent.NWaySChan
-- Copyright   :  (c) 2006,2007 Kevin Donnelly & Matthew Fluet
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

module Control.Concurrent.TxEvent.NWaySChan (
  -- * Synchronous n-way channels
    NWaySChan     -- abstract
  , newNWaySChan  -- :: Int -> Evt (NWaySChan a)
  , swapEvt       -- :: NWaySChan a -> a -> Evt [a]
  ) where

import Control.Concurrent.TxEvent
import qualified Control.Concurrent.TxEvent.NWaySChan.Ver3 as Ver

type NWaySChan a = Ver.NWaySChan a
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
newNWaySChan = Ver.newNWaySChan

{-|
Send a value on the channel and receive /n-1/ values on the channel.
-}
swapEvt :: NWaySChan a -> a -> Evt [a]
swapEvt = Ver.swapEvt
