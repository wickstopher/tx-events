-- SAT Solver (exponential in the number of communications)

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.TxEvent

import Debug.Trace
import Monad

data Formula = Atom Int | Neg Formula | Or Formula Formula

evalFormula :: [Bool] -> Formula -> Bool
evalFormula vals (Atom n)   = head $ drop n vals
evalFormula vals (Neg f)    = not (evalFormula vals f)
evalFormula vals (Or f1 f2) = evalFormula vals f1 || evalFormula vals f2

sat :: Int -> Formula -> IO ()
sat k formula = do
  c <- sync newSChan
  forkIO $ sync $ mapM_ (\_ -> sendEvt c True) [1..k]
  forkIO $ sync $ mapM_ (\_ -> sendEvt c False) [1..k]
  sync $ do 
    input <- mapM (\_ -> recvEvt c) [1..k]
    mapM_ (\_ -> recvEvt c) [1..k]
    let success = evalFormula input formula
    if success then alwaysEvt () else neverEvt 
  putStrLn "Satisfiable"

main = do
  let k = 2
  let formula = Neg (Neg (Atom 0) `Or` (Atom 0)) `Or` Atom 1
  sat k formula
