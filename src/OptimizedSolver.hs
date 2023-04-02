-- ------------------------------------------------
-- @file OptimizedSolver.hs  ----------------------
-- @author Boris Štrbák (xstrba05)  ---------------
-- ------------------------------------------------
-- Module with function that solves 0-1 Knapsack  -
-- problem using simulated annealing  -------------
-- ------------------------------------------------

module OptimizedSolver (solveSa) where
import Types (
    ReturnType (Print, Failure, Success),
    State (filled, value, sWeight),
    Knapsack (minCost, maxWeight))
import State (randomlyFilledState, statesConstrainedValue, initState)
import Helpers (printResult, getRandomNumber, getRandomFloat, replaceBitOnPosition)

solveSa :: ReturnType -> IO ()
solveSa (Print a) = do
    extractedState <- randomlyFilledState a
    resultState <- saIteration a extractedState 50.00 0.99 1
    let result = if (value resultState >= minCost a) && (sWeight resultState <= maxWeight a)
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
    let p = exp $ (neighborsValue - currentValue) / tmp
    rand <- getRandomFloat 0 1
    let nextState = if (neighborsValue > currentValue) || (rand < p)
        then neighbor
        else currentState

    let newTmp = tmp * alpha
    let newConstrainedTmp = if newTmp > 0.000001 then newTmp else 0.000001
    saIteration sack nextState newConstrainedTmp alpha (iter + 1)

makeRandomNeighbor :: Knapsack -> State -> IO State
makeRandomNeighbor sack state = do
    let oldBits = filled state
    randomNumber <- getRandomNumber 1 (length oldBits)
    let newBits = replaceBitOnPosition oldBits randomNumber
    return $ initState sack newBits