-- ------------------------------------------------
-- @file Types.hs  --------------------------------
-- @author Boris Štrbák (xstrba05)  ---------------
-- ------------------------------------------------
-- Module with custom types.  ---------------------
-- ------------------------------------------------

module Types (
    Item (weight, cost, Item),
    Knapsack (maxWeight, minCost, items, Knapsack),
    ReturnType (Success, Failure, Print, Error, Debug),
    State (State, filled, value, sWeight),
    exampleSack
) where

-- item in sack
data Item = Item
    {
        weight :: !Int,
        cost :: !Int
    } deriving (Show)

-- sack
data Knapsack = Knapsack
    {
        maxWeight :: !Int,
        minCost :: !Int,
        items :: ![Item]
    } deriving (Show)

-- return type of program
-- Success is for returning list representing solution
-- Failure is for returning False when there is no solution
-- Print is for representing Knapsack
-- Error is for return error message instead of throwing it
-- Debug is for printing debug info
data ReturnType = Success ![Int] | Failure !Bool | Print !Knapsack | Error !String
        | Debug !String
    deriving (Show)

-- State holding sack, list of zeroes or ones representing if given item
-- is in sack or not and holding value of items in sack.
data State = State
    {
        filled :: ![Int],
        value :: !Int,
        sWeight :: !Int
    } deriving (Show)

-- This is for testing only
exampleSack :: Knapsack
exampleSack = Knapsack { maxWeight = 100, minCost = 50, items = [Item 10 5, Item 10 5] }
