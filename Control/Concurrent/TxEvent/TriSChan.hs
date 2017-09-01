-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent.TriSChan
-- Copyright   :  (c) 2006,2007 Kevin Donnelly & Matthew Fluet
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

module Control.Concurrent.TxEvent.TriSChan (
  -- * Synchronous 3-way channels
    TriSChan     -- abstract
  , newTriSChan  -- :: Evt (TriSChan a)
  , swapEvt      -- :: TriSChan a -> a -> Evt (a, a)
  ) where

import Control.Concurrent.TxEvent
import qualified Control.Concurrent.TxEvent.TriSChan.Ver2 as Ver

type TriSChan a = Ver.TriSChan a
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
newTriSChan = Ver.newTriSChan

{-|
Send a value on the channel and receive 2 values on the channel.
-}
swapEvt :: TriSChan a -> a -> Evt (a, a)
swapEvt = Ver.swapEvt
