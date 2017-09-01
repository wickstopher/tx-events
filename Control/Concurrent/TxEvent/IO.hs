{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.TxEvent.IO
-- Copyright   :  (c) 2006,2007 Kevin Donnelly & Matthew Fluet
-- License     :  BSD3
-- Maintainer  :  Matthew Fluet <Matthew.Fluet@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (requires TxEvent)
--
-----------------------------------------------------------------------------
module Control.Concurrent.TxEvent.IO (
  -- * TxEvent based text output handles
    EvtHandle     -- abstract

  -- * Lifting 'System.IO.Handle'
  , mkEvtHandle   -- :: System.IO.Handle -> EvtHandle

  -- * TxEvent based text output
  , hPutCharEvt   -- :: EvtHandle -> Char -> Evt ()
  , hPutStrEvt    -- :: EvtHandle -> String -> Evt ()
  , hPutStrLnEvt  -- :: EvtHandle -> String -> Evt ()
  , hPrintEvt     -- :: Show a => EvtHandle -> a -> Evt ()
  ) where

import Control.Concurrent
import Control.Concurrent.TxEvent

import System.IO

-- Misc
forkIO_ :: IO () -> IO ()
forkIO_ act = forkIO act >> return ()

----------------------------------------------------------------------
----------------------------------------------------------------------

data PutOper = PutChar Char
             | PutStr String
             | PutStrLn String
             | forall a. Show a => Print a

newtype EvtHandle = EvtHandle (SChan PutOper)
{- ^
An @'EvtHandle'@ value is much like a @'System.IO.Handle'@ value,
except that access to the handle is mediated by the @'Evt'@ monad,
rather than the @'IO'@ monad.
-}

{-|
Lift a /writeable/ @'System.IO.Handle'@ to an @'EvtHandle'@.
-}
mkEvtHandle :: Handle -> IO EvtHandle
mkEvtHandle h = fromPutOpers (hPutChar h) (hPutStr h) (hPutStrLn h) (hPrint h)

fromPutOpers :: (Char -> IO ()) ->
                (String -> IO ()) -> 
                (String -> IO ()) ->
                (forall a . Show a => a -> IO ()) -> 
                IO EvtHandle
fromPutOpers putChar putStr putStrLn print = do
  ch <- sync $ newSChan
  let loop = do
        ss <- sync $ getMulti []
        mapM_ (\ po -> case po of
                         PutChar c -> putChar c
                         PutStr s -> putStr s
                         PutStrLn s -> putStrLn s
                         Print x -> print x) 
              (reverse ss)
        loop
      getMulti acc = do
        s <- recvEvt ch
        getMultiAux (s:acc)
      getMultiAux acc = (alwaysEvt acc) 
                        `chooseEvt` 
                        (do -- Add a little left bias
                            timeOutEvt 100
                            getMulti acc)
  forkIO_ $ loop
  return (EvtHandle ch)

{-|
Operation @'hPutCharEvt' hdl ch@ writes the character @ch@ to the file
or channel managed by @hdl@ when synchronized upon.  See
'System.IO.hPutChar'.
-}
hPutCharEvt :: EvtHandle -> Char -> Evt ()
hPutCharEvt (EvtHandle h) c = sendEvt h (PutChar c)

{-|
Operation @'hPutStrEvt' hdl s@ writes the string @s@ to the file or
channel managed by @hdl@ when synchronized upon.  See
'System.IO.hPutStr'.
-}
hPutStrEvt :: EvtHandle -> String -> Evt ()
hPutStrEvt (EvtHandle h) s = sendEvt h (PutStr s)

{-|
The same as 'hPutStrEvt', but adds a newline character.  See
'System.IO.hPutStrLn'
-}
hPutStrLnEvt :: EvtHandle -> String -> Evt ()
hPutStrLnEvt (EvtHandle h) s = sendEvt h (PutStrLn s)

{-|
Operation @'hPrintEvt' hdl t@ writes the string representation of @t@
given by the @'shows'@ function to the file or channel managed by
@hdl@ and appends a newline.
-}
hPrintEvt :: Show a => EvtHandle -> a -> Evt ()
hPrintEvt (EvtHandle h) x = sendEvt h (Print x)

{--
{-# NOINLINE stdoutH #-}
stdoutH :: EvtHandle
stdoutH = unsafePerformIO (fromPutOpers putChar putStr putStrLn print)

putCharEvt :: Char -> Evt ()
putCharEvt c = sendEvt stdoutH (PutChar c)

putStrEvt :: String -> Evt ()
putStrEvt c = sendEvt stdoutH (PutStr c)

putStrLnEvt :: String -> Evt ()
putStrLnEvt c = sendEvt stdoutH (PutStrLn c)

printEvt :: Show a => a -> Evt ()
printEvt x = sendEvt stdoutH (Print x)
--}
