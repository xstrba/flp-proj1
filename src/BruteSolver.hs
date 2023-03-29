-- ------------------------------------------------
-- @file BruteSolver.hs  --------------------------
-- @author Boris Štrbák (xstrba05)  ---------------
-- ------------------------------------------------
-- Module with function that solves 0-1 Knapsack  -
-- problem using brute force  ---------------------
-- ------------------------------------------------

module BruteSolver (solveBf) where
import Types (ReturnType (Print, Success, Failure), State (sack, filled, value), Knapsack (maxWeight, minCost))
import State (initState, filledWeight, stateWithMaxValue)

-- start solving knapsack representing in Print (ReturnType) data type
-- returns given argument if ReturnType is not of type Print
-- for Knapsack (Print) returns Success with list of zeroes and ones
-- or Failure with value False if there is no solution
solveBf :: ReturnType -> ReturnType
solveBf (Print a)
    | value solvedState >= minCost (sack solvedState) = Success (filled solvedState)
    | otherwise = Failure False
    where
        solvedState = solveState $ initState a []
solveBf a = a

-- solve substates (or simply next states) of given state
-- creates new states by putting one in State(filled) list on places
-- from start until it finds first already assigned one
-- for [0 0 1] it would prcess [0 1 1] and [1 0 1]
-- second argument should be length of list until first one
-- for [0 0 1] it would be 2
-- returns solved substates, which means that for each substate returns
-- state from its tree with highest cost value
solveSubstates :: State -> Int -> [State]
solveSubstates _ 0 = []
solveSubstates rootState len
    | filledWeight newState <= maxWeight (sack newState) = newState:solveSubstates rootState (len - 1)
    | otherwise = solveSubstates rootState (len - 1)
    where
        newFilled = getNewFilled len (filled rootState)
        newState = solveState $ initState (sack rootState) newFilled

-- solve state by solving it`s substate
-- returns state with highest cost value in it`s tree
solveState :: State -> State
solveState a
    | null substates = a
    | otherwise = stateWithMaxValue (a:substates)
    where
        substates = solveSubstates a (lenUntilFirstOne (filled a) 0)

-- basically it is number of zeroes until first one in list
lenUntilFirstOne :: [Int] -> Int -> Int
lenUntilFirstOne [] n = n
lenUntilFirstOne (x:xs) n
    | x == 1 = n
    | otherwise = lenUntilFirstOne xs (n + 1)

-- create new filled by putting one on position
-- given by first argument, until this position
-- it will fill list with zeroes
-- then it will append remaining items from given
-- list.
getNewFilled :: Int -> [Int] -> [Int]
getNewFilled 0 xs = xs
getNewFilled 1 [] = [1]
getNewFilled 1 (_:xs) = 1:xs
getNewFilled n (_:xs) = 0:getNewFilled (n - 1) xs
getNewFilled n [] = 0:getNewFilled (n - 1) []
