-- SAT Solver (exponential in the number of choice points)

import Control.Concurrent
import Control.Concurrent.MVar

import Control.Concurrent.STM

import Debug.Trace
import Monad

data Formula = Atom Int | Neg Formula | Or Formula Formula

evalFormula :: [Bool] -> Formula -> Bool
evalFormula vals (Atom n)   = head $ drop n vals
evalFormula vals (Neg f)    = not (evalFormula vals f)
evalFormula vals (Or f1 f2) = evalFormula vals f1 || evalFormula vals f2

sat :: Int -> Formula -> IO()
sat k formula = do
  atomically (foldr (\_ f -> (\l -> (f (False:l)) `orElse` (f (True:l))))
                    (\input -> do let success = evalFormula input formula
                                  if success then return () else retry)
                    [1..k]
                    [])
  putStrLn "Satisfiable"

main = do
  let k = 2
  let formula = Neg (Neg (Atom 0) `Or` (Atom 0)) `Or` Atom 1
  sat k formula
