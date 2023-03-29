module BruteSolver (solveBf) where
import Types (ReturnType (Print, Success, Failure), State (sack, filled, value), Knapsack (maxWeight, minCost))
import State (initState, filledWeight, stateWithMaxValue)

solveBf :: ReturnType -> ReturnType
solveBf (Print a)
    | value solvedState >= minCost (sack solvedState) = Success (filled solvedState)
    | otherwise = Failure False
    where
        solvedState = solveState $ initState a []
solveBf a = a

solveSubstates :: State -> Int -> [State]
solveSubstates _ 0 = []
solveSubstates rootState len
    | filledWeight newState <= maxWeight (sack newState) = newState:solveSubstates rootState (len - 1)
    | otherwise = solveSubstates rootState (len - 1)
    where
        newFilled = getNewFilled len (filled rootState)
        newState = solveState $ initState (sack rootState) newFilled

solveState :: State -> State
solveState a
    | null substates = a
    | otherwise = stateWithMaxValue (a:substates)
    where
        substates = solveSubstates a (lenUntilFirstOne (filled a) 0)

lenUntilFirstOne :: [Int] -> Int -> Int
lenUntilFirstOne [] n = n
lenUntilFirstOne (x:xs) n
    | x == 1 = n
    | otherwise = lenUntilFirstOne xs (n + 1)

getNewFilled :: Int -> [Int] -> [Int]
getNewFilled 0 xs = xs
getNewFilled 1 [] = [1]
getNewFilled 1 (_:xs) = 1:xs
getNewFilled n (_:xs) = 0:getNewFilled (n - 1) xs
getNewFilled n [] = 0:getNewFilled (n - 1) []
