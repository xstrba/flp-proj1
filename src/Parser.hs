-- ------------------------------------------------
-- @file Parser.hs  -------------------------------
-- @author Boris Štrbák (xstrba05)  ---------------
-- ------------------------------------------------
-- Module with function for parsing string into  --
-- knapsack representation  -----------------------
-- ------------------------------------------------

module Parser (parseKnapsack) where
import Types (Knapsack (Knapsack, maxWeight, minCost, items), ReturnType(Print, Error), Item (Item, weight, cost))
import Text.Parsec ( ParsecT, char, spaces, string, skipMany1, digit, many1, endBy )
import Text.Parsec.String ( Parser )
import qualified Control.Monad.Identity as Data.Functor.Identity
import Text.Parsec.Prim ( parse )

-- for not skipin \n
skipOnlySpaces :: ParsecT String u Data.Functor.Identity.Identity ()
skipOnlySpaces = skipMany1 (char ' ')

-- start parsing
parseKnapsack :: String -> ReturnType
parseKnapsack input = case parse knapsackParser "" input of
    Left err -> Error (show err)
    Right knapsack -> Print knapsack

-- main Knapsack parser
knapsackParser :: Parser Knapsack
knapsackParser = do
    spaces
    _ <- string "Knapsack"
    skipOnlySpaces
    _ <- string "{"
    spaces
    parsedMaxWeight <- intParser "maxWeight"
    spaces
    parsedMinCost <- intParser "minCost"
    spaces
    parsedItems <- itemsParser
    spaces
    _ <- char '}'
    return Knapsack { maxWeight = parsedMaxWeight, minCost = parsedMinCost, items = parsedItems }

-- parsing of field with Int value
intParser :: String -> Parser Int
intParser fName = do
    _ <- string $ fName ++ ":"
    skipOnlySpaces
    value <- many1 digit
    return (read value)

-- parse Items of knapsack
itemsParser :: Parser [Item]
itemsParser = do
    _ <- string $ "items" ++ ":"
    skipOnlySpaces
    _ <- char '['
    spaces
    itemsArr <- itemParser `endBy` (char '\n' >> spaces)
    spaces
    _ <- char ']'
    return itemsArr

-- parse Item in Items list
itemParser :: Parser Item
itemParser = do
    _ <- string "Item"
    skipOnlySpaces
    _ <- char '{'
    spaces
    parsedWeight <- intParser "weight"
    spaces
    parsedCost <- intParser "cost"
    spaces
    _ <- char '}'
    return Item { weight  = parsedWeight, cost = parsedCost }