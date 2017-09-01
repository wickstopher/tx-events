
import Control.Concurrent
import Control.Concurrent.MVar

import System.Environment

import Control.Concurrent.TxEvent

-- Misc
forkIO_ :: IO () -> IO ()
forkIO_ act = forkIO act >> return ()

makeNatStream :: Int -> IO (SChan Int)
makeNatStream c = do
  ch <- sync $ newSChan
  let count i = do
        sync $ sendEvt ch i
        count (i + 1)
  forkIO_ $ do
    tid <- myThreadId
    putStrLn $ "makeNatStream: " ++ (show tid)
    count c
  return ch

makeFilter :: Int -> SChan Int -> IO (SChan Int)
makeFilter p inCh = do
  outCh <- sync $ newSChan
  let loop = do
        i <- sync $ recvEvt inCh
        if ((i `mod` p) /= 0)
           then sync $ sendEvt outCh i
           else return ()
        loop
  forkIO_ $ loop
  return outCh

makePrimes :: IO (SChan Int)
makePrimes = do
  primes <- sync $ newSChan
  let head ch = do
        p <- sync $ recvEvt ch
        sync $ sendEvt primes p
        ch' <- makeFilter p ch
        head ch'
  forkIO_ $ do
    tid <- myThreadId
    putStrLn $ "makePrimes: " ++ (show tid)
    ch <- makeNatStream 2
    head ch
  return primes

makeNatPrinter :: MVar () -> SChan Int -> Int -> IO ()
makeNatPrinter wait ch n = do
  let loop i =
          if i > n
             then putMVar wait ()
             else do m <- sync $ recvEvt ch
                     putStrLn $ (show m)
                     loop (i + 1)
  forkIO_ $ do
    tid <- myThreadId
    putStrLn $ "makeNatPrinter: " ++ (show tid)
    loop 0
  return ()

doit :: Int -> IO ()
doit n = do
  wait <- newEmptyMVar
  ch <- makePrimes
  makeNatPrinter wait ch n
  takeMVar wait

main :: IO ()
main = do
  args <- getArgs
  let n = case args of
            [] -> 100
            s:_ -> case reads s of
                     [(n,"")] -> n
                     _ -> 100
  doit n
