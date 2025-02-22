-- ------------------------------------------------
-- @file Helpers.hs  ------------------------------
-- @author Boris Štrbák (xstrba05)  ---------------
-- ------------------------------------------------
-- Module with function for parsing string into  --
-- knapsack representation  -----------------------
-- ------------------------------------------------

module Helpers (printResult, printKnapsack, printItems,
    getRandomBit, getRandomNumber, replaceBitOnPosition, getRandomFloat) where
import Types (
    Knapsack (maxWeight, minCost, items),
    Item (weight, cost),
    ReturnType (Success, Failure, Print, Error, Debug))
import System.Random (randomRIO)

-- print items of knapsack
printItems :: [Item] -> IO ()
printItems [] = putStrLn "items: []"
printItems xs = let
    printItem :: Item -> IO ()
    printItem a = do
        putStrLn "    Item {"
        putStrLn $ "    weight: " ++ show (weight a)
        putStrLn $ "    cost: " ++ show (cost a)
        putStrLn "    }"
    printItemByItem :: [Item] -> IO ()
    printItemByItem [] = putStrLn ""
    printItemByItem [b] = printItem b
    printItemByItem (b:bs) = do
        printItem b
        printItemByItem bs
    in do
    putStrLn "items: ["
    printItemByItem xs
    putStrLn "]"

-- print knapsack
printKnapsack :: Knapsack -> IO ()
printKnapsack a = do
    putStrLn "Knapsack {"
    putStrLn $ "maxWeight: " ++ show (maxWeight a)
    putStrLn $ "minCost: " ++ show (minCost a)
    printItems $ items a
    putStrLn "}"

-- print ReturnType (result of app)
printResult :: ReturnType -> IO ()
printResult (Print a) = printKnapsack a
printResult (Success a) = putStrLn ("Solution [" ++ printList a ++ "]")
    where
        printList :: Show a => [a] -> String
        printList [] = ""
        printList [x] = show x
        printList (x:xs) = show x ++ " " ++ printList xs
printResult (Failure a) = print a
printResult (Error a) = putStrLn $ "Error: " ++ a
printResult (Debug a) = putStrLn $ "Info: " ++ a

getRandomBit :: IO Int
getRandomBit = randomRIO (0, 1)

getRandomNumber :: Int -> Int -> IO Int
getRandomNumber low high = randomRIO (low, high)

getRandomFloat :: Float -> Float -> IO Float
getRandomFloat low high = randomRIO (low, high)

-- getRandomFloat :: Float -> Float -> IO Float
-- getRandomFloat low high = do
--   gen <- newStdGen
--   let (value, newGen) = randomR (low, high) gen :: (Float, StdGen)
--   return value

replaceBitOnPosition :: [Int] -> Int -> [Int]
replaceBitOnPosition _ 0 = []
replaceBitOnPosition [] _ = []
replaceBitOnPosition (x:xs) 1
    | x == 1 = 0:xs
    | otherwise = 1:xs
replaceBitOnPosition (x:xs) n = x:replaceBitOnPosition xs (n - 1)