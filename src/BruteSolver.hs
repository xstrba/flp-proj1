module BruteSolver (solveBf) where
import Types (ReturnType (Print, Success, Failure), State (State, sack, filled, value), Knapsack (maxWeight, minCost))
import State (initState, filledWeight, stateWithMaxValue)

solveBf :: ReturnType -> ReturnType
solveBf (Print a)
    | value solvedState >= minCost (sack solvedState) = Success (filled solvedState)
    | otherwise = Failure False
    where
        solvedState = solveState $ initState a []
solveBf a = a

getSubstates :: State -> Int -> [State]
getSubstates _ 0 = []
getSubstates rootState len
    | filledWeight newState <= maxWeight (sack newState) = newState:getSubstates rootState (len - 1)
    | otherwise = getSubstates rootState (len - 1)
    where
        newFilled = getNewFilled len (filled rootState)
        newState = initState (sack rootState) newFilled

solveStates :: [State] -> [State]
solveStates = map solveState

solveState :: State -> State
solveState a
    | null substates = a
    | otherwise = stateWithMaxValue $ solveStates substates
    where
        substates = getSubstates a (lenUntilFirstOne (filled a) 0)

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

-- getNextStates :: State -> [State]
-- getNextStates state = let
--     buildState
--     getNextStates1 :: State -> Int -> [State]
--     getNextStates1 state1 n = 
--     in getNextStates1 state 0