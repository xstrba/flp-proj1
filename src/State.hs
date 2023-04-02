-- ------------------------------------------------
-- @file State.hs  --------------------------------
-- @author Boris Štrbák (xstrba05)  ---------------
-- ------------------------------------------------
-- Module with functions for working with State.  -
-- ------------------------------------------------

-- because of stateWithMaxValue function
-- we never pass empty list to this function
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module State (initState, stateWithMaxValue, randomlyFilledState, statesConstrainedValue, stateWithMaxConstrainedValue) where
import Types (
    State (State, filled, value, sWeight),
    Item (weight, cost),
    Knapsack (items, maxWeight, minCost))
import Helpers (getRandomBit)
import Control.Monad (replicateM)

-- creates new state from sack and list of zeroes or ones
initState :: Knapsack -> [Int] -> State
initState initSack initFilled = State {filled = capacities, value = countedValue, sWeight = countedWeight}
    where
        capacities = let
            initStatesCapacity [] _ = []
            initStatesCapacity (_:xs) [] = 0:initStatesCapacity xs []
            initStatesCapacity (_:xs) (a:as) = a:initStatesCapacity xs as
            in initStatesCapacity (items initSack) initFilled

        countedValue = let
            countValue [] _ = 0
            countValue _ [] = 0
            countValue (x:xs) (a:as) = (cost x * a) + countValue xs as
            in countValue (items initSack) initFilled

        countedWeight = let
            countWeight :: [Item] -> [Int] -> Int
            countWeight [] _ = 0
            countWeight _ [] = 0
            countWeight (x:xs) (a:as) = (weight x * a) + countWeight xs as
            in countWeight (items initSack) initFilled

stateWithMaxValue :: [State] -> State
stateWithMaxValue [x] = x
stateWithMaxValue (x:xs)
    | value maxValueState > value x = maxValueState
    | otherwise = x
    where maxValueState = stateWithMaxValue xs

stateWithMaxConstrainedValue :: [State] -> Knapsack -> State
stateWithMaxConstrainedValue [x] a = x
stateWithMaxConstrainedValue (x:xs) a
    | statesConstrainedValue a maxValueState > statesConstrainedValue a x = maxValueState
    | otherwise = x
    where maxValueState = stateWithMaxConstrainedValue xs a

randomlyFilledState :: Knapsack -> IO State
randomlyFilledState sack = do
  randomBits <- replicateM (length $ items sack) getRandomBit
  return $ initState sack randomBits

statesConstrainedValue :: Knapsack -> State -> Float
statesConstrainedValue sack state
    | sWeight state > maxWeight sack || value state < minCost sack = 0.00
    | otherwise = fromIntegral $ value state
    where
        countedValue = (maxWeight sack - sWeight state) + (value state - minCost sack)