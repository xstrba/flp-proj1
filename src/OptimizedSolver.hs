-- ------------------------------------------------
-- @file OptimizedSolver.hs  ----------------------
-- @author Boris Štrbák (xstrba05)  ---------------
-- ------------------------------------------------
-- Module with function that solves 0-1 Knapsack  -
-- problem using simulated annealing  -------------
-- ------------------------------------------------

module OptimizedSolver (solveSa) where
import Types (
    ReturnType (Print, Failure, Debug, Success),
    State (filled, value),
    Knapsack (minCost))
import State (randomlyFilledState, statesConstrainedValue, initState)
import Helpers (printResult, getRandomNumber, replaceBitOnPosition)

solveSa :: ReturnType -> IO ()
solveSa (Print a) = do
    extractedState <- randomlyFilledState a
    resultState <- saIteration a extractedState 10000.00 0.98 1
    let result = if value resultState >= minCost a
        then Success $ filled resultState
        else Failure False
    printResult result
solveSa a = printResult a

-- sack, currentState, temperatur, alpha, currentIteration = new currentState
saIteration :: Knapsack -> State -> Float -> Float -> Int -> IO State
saIteration _ currentState _ _ 1001 = do
    let resultState = currentState
    return resultState
saIteration sack currentState tmp alpha iter = do
    neighbor <- makeRandomNeighbor sack currentState
    let currentValue = statesConstrainedValue sack currentState
    let neighborsValue = statesConstrainedValue sack neighbor
    let nextState = if neighborsValue >= currentValue
        then neighbor
        else currentState
    saIteration sack nextState tmp alpha (iter + 1)

makeRandomNeighbor :: Knapsack -> State -> IO State
makeRandomNeighbor sack state = do
    let oldBits = filled state
    randomNumber <- getRandomNumber 1 (length oldBits)
    let newBits = replaceBitOnPosition oldBits randomNumber
    return $ initState sack newBits