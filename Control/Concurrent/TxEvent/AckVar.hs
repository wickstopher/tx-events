-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent.AckVar
-- Copyright   :  (c) 2006,2007 Kevin Donnelly & Matthew Fluet
-- License     :  BSD3
-- Maintainer  :  Matthew Fluet <Matthew.Fluet@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires TxEvent)
--
-- Acknowledgement Variables.
--
-- This library provides synchronous acknowledgement variables, which
-- are similar to @Id@-style synchronous I-variables.
--
-----------------------------------------------------------------------------

module Control.Concurrent.TxEvent.AckVar (
  -- * Synchronous acknowledgement variables
    AckVar        -- abstract
  , newAckVar     -- :: IO AckVar
  , setAckVar     -- :: AckVar -> IO ()
  , getAckVarEvt  -- :: AckVar -> Evt ()
  ) where

import Control.Concurrent
import Control.Concurrent.TxEvent

-- Misc
forkIO_ :: IO () -> IO ()
forkIO_ act = forkIO act >> return ()

----------------------------------------------------------------------
----------------------------------------------------------------------

newtype AckVar = AckVar (SChan ())
{- ^
The type of acknowledgement variables.
-}

{-|
Create a new acknowledgement variable.
-}
newAckVar :: IO AckVar
newAckVar = sync $ do 
  ch <- newSChan
  return (AckVar ch)

{-|
Asynchronously enable the acknowledgement variable.
-}
setAckVar :: AckVar -> IO ()
setAckVar (AckVar ch) = 
    let loop = do sync (sendEvt ch ()); loop in
    forkIO_ loop

{-|
Synchronously query the acknowledgement variables;
this event becomes commitable when the acknowledgement variable is enabled.
-}
getAckVarEvt :: AckVar -> Evt ()
getAckVarEvt (AckVar ch) = recvEvt ch
