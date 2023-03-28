module State (initState, filledWeight) where
import Types (State (State, sack, filled, value), Item (weight, cost), Knapsack (items))

initState :: Knapsack -> [Int] -> State
initState initSack initFilled = State {sack = initSack, filled = capacities, value = countedValue}
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

filledWeight :: State -> Int
filledWeight state = let
    countWeight :: [Item] -> [Int] -> Int
    countWeight [] _ = 0
    countWeight _ [] = 0
    countWeight (x:xs) (a:as) = (weight x * a) + countWeight xs as
    in countWeight (items (sack state)) (filled state)