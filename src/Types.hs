module Types (
    Item(weight, cost, Item),
    Knapsack(maxWeight, minCost, items, Knapsack),
    ReturnType(Success, Failure, Print, Error),
    exampleSack
) where

data Item = Item
    {
        weight :: !Int,
        cost :: !Int
    } deriving (Show)

data Knapsack = Knapsack
    {
        maxWeight :: !Int,
        minCost :: !Int,
        items :: ![Item]
    } deriving (Show)

data ReturnType = Success ![Int] | Failure !Bool | Print !Knapsack | Error !String
    deriving (Show)

-- This is for testing only
exampleSack :: Knapsack
exampleSack = Knapsack { maxWeight = 100, minCost = 50, items = [Item 10 5, Item 10 5] }
