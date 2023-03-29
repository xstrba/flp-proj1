-- ------------------------------------------------
-- @file OptimizedSolver.hs  ----------------------
-- @author Boris Štrbák (xstrba05)  ---------------
-- ------------------------------------------------
-- Module with function that solves 0-1 Knapsack  -
-- problem using simulated annealing  -------------
-- ------------------------------------------------

module OptimizedSolver (solveSa) where
import Types (ReturnType (Print, Failure))

solveSa :: ReturnType -> ReturnType
solveSa (Print a) = Failure False
solveSa a = a