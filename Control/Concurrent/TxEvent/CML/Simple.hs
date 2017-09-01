-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent.CML.Simple
-- Copyright   :  (c) 2007 Kevin Donnelly & Matthew Fluet
-- License     :  BSD3
-- Maintainer  :  Matthew Fluet <Matthew.Fluet@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires TxEvent)
--
-- Concurrent ML library.
-- 
-- This library provides /first-class synchronous events/ in the style
-- of CML (<http://cml.cs.uchicago.edu/>).  This library implements a
-- simplified CML interface, eliding the @withNack@ combinator.
--
-----------------------------------------------------------------------------
module Control.Concurrent.TxEvent.CML.Simple (
  -- * TxEvent type
    Evt           -- abstract

  -- * TxEvent combinators
  , alwaysEvt     -- :: a -> Evt a
  , wrapEvt       -- :: Evt a -> (a -> IO b) -> Evt b
  , guardEvt      -- :: IO (Evt a) -> Evt a
  , neverEvt      -- :: Evt a
  , chooseEvt     -- :: Evt a -> Evt a -> Evt a
  , sync          -- :: Evt a -> IO a

  -- * Synchronous channels
  , SChan         -- abstract
  , newSChan      -- :: IO (SChan a)
  , sendEvt       -- :: SChan a -> a -> Evt ()
  , recvEvt       -- :: SChan a -> Evt a

  -- * Time delays
  , timeOutEvt    -- :: Int -> Evt
  ) where

import Prelude
import Control.Monad
import qualified Control.Concurrent.TxEvent as TxEvent

----------------------------------------------------------------------
----------------------------------------------------------------------

type Evt a = IO (TxEvent.Evt (IO a))

sync :: Evt a -> IO a
sync io_evt_io = do
  evt_io <- io_evt_io
  io <- TxEvent.sync evt_io
  io

lift :: TxEvent.Evt a -> Evt a
lift evt = return (fmap return evt)

alwaysEvt :: a -> Evt a
alwaysEvt x = lift (TxEvent.alwaysEvt x)

wrapEvt :: Evt a -> (a -> IO b) -> Evt b
wrapEvt io_evt_io f = fmap (fmap (>>= f)) io_evt_io

guardEvt :: IO (Evt a) -> Evt a
guardEvt io_io_evt_io = join io_io_evt_io

neverEvt :: Evt a
neverEvt = lift (TxEvent.neverEvt)

chooseEvt :: Evt a -> Evt a -> Evt a
chooseEvt io_evt_io1 io_evt_io2 = do
  evt_io1 <- io_evt_io1
  evt_io2 <- io_evt_io2
  return (evt_io1 `TxEvent.chooseEvt` evt_io2)

type SChan a = TxEvent.SChan a

newSChan :: IO (TxEvent.SChan a)
newSChan = TxEvent.sync (TxEvent.newSChan)

recvEvt :: TxEvent.SChan a -> Evt a
recvEvt ch = lift (TxEvent.recvEvt ch)

sendEvt :: TxEvent.SChan a -> a -> Evt ()
sendEvt ch x = lift (TxEvent.sendEvt ch x)

timeOutEvt :: Int -> Evt ()
timeOutEvt n = lift (TxEvent.timeOutEvt n)
