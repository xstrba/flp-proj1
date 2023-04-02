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
import State (randomlyFilledState, statesConstrainedValue, initState, stateWithMaxConstrainedValue)
import Helpers (printResult, getRandomNumber, getRandomFloat, replaceBitOnPosition)
import Control.Monad (replicateM)

solveSa :: ReturnType -> IO ()
solveSa (Print a) = do
    resultStates <- replicateM 10 (makeSaTry a)
    let resultState = stateWithMaxConstrainedValue resultStates a
    let result = if (value resultState >= minCost a) && (sWeight resultState <= maxWeight a)
        then Success $ filled resultState
        else Failure False
    printResult result
solveSa a = printResult a

makeSaTry :: Knapsack -> IO State
makeSaTry sack = do
    extractedState <- randomlyFilledState sack
    saIteration sack extractedState 100.00 0

-- sack, currentState, temperatur, alpha, currentIteration = new currentState
saIteration :: Knapsack -> State -> Float -> Int -> IO State
saIteration _ currentState _ 1000 = do
    let resultState = currentState
    return resultState
saIteration sack currentState tmp iter = do
    neighbor <- makeRandomNeighbor sack currentState
    let temp = tmp * (1.00 - (fromIntegral iter + 1.00) / 1000.00)
    let currentValue = statesConstrainedValue sack currentState
    let neighborsValue = statesConstrainedValue sack neighbor
    let p = if neighborsValue > currentValue
        then 1.1
        else exp $ neighborsValue - currentValue / temp
    -- putStrLn $ show p
    rand <- getRandomFloat 0 1
    let nextState = if rand < p
        then neighbor
        else currentState
    saIteration sack nextState tmp (iter + 1)

makeRandomNeighbor :: Knapsack -> State -> IO State
makeRandomNeighbor sack state = do
    let oldBits = filled state
    randomNumber <- getRandomNumber 1 (length oldBits)
    let newBits = replaceBitOnPosition oldBits randomNumber
    return $ initState sack newBits