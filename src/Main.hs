module Main where
import System.Environment (getArgs, getProgName)
import Types (ReturnType(Success, Print, Failure, Error), exampleSack)
import Helpers (printResult)
import Control.Exception (try, SomeException)
import Parser (parseKnapsack)
import BruteSolver (solveBf)
import OptimizedSolver (solveSa)

help :: IO ()
help = do
    progName <- getProgName
    putStrLn "0-1 verze knapsack problemu"
    putStrLn ""
    putStrLn $ progName ++ " volby [vstup]"
    putStrLn ""
    putStrLn "vstyp je jmeno souboru (pokud neni specifikovan tak stdin)"
    putStrLn ""
    putStrLn "volby muzou byt: "
    putStrLn "  -i    Na stdout vypise informace ze vstupu."
    putStrLn "  -b    Vypise reseni knapsack problemu brute force metodou."
    putStrLn "        Vypise False v pripade ze nenajde reseni."
    putStrLn "  -o    Vypise reseni knapsack problemu pomoci simulovaneho zihani."
    putStrLn "        Vypise False v pripade ze nenajde reseni."


safeFileReader :: String -> (String -> ReturnType) -> IO ()
safeFileReader fileName fn = do
    fileContentsOrError <- try (readFile fileName) :: IO (Either SomeException String)
    case fileContentsOrError of
        Left errorMsg -> printResult $ Error (show errorMsg)
        Right fileContents -> printResult $ fn fileContents

safeStdinReader :: (String -> ReturnType) -> IO ()
safeStdinReader fn = do
    fileContentsOrError <- try getContents :: IO (Either SomeException String)
    case fileContentsOrError of
        Left errorMsg -> printResult $ Error (show errorMsg)
        Right fileContents -> printResult $ fn fileContents

main :: IO ()
main = let
    startWithTwoArgs :: String -> String -> IO ()
    startWithTwoArgs a b = case a of
        "-i" -> safeFileReader b parseKnapsack
        "-b" -> safeFileReader b (solveBf . parseKnapsack)
        "-o" -> safeFileReader b (solveSa . parseKnapsack)
        _anyOtherOption -> help
    startWithOneArg :: String -> IO ()
    startWithOneArg a = case a of
        "-i" -> safeStdinReader parseKnapsack
        "-b" -> safeStdinReader (solveBf . parseKnapsack)
        "-o" -> safeStdinReader (solveSa . parseKnapsack)
        _anyOtherOption -> help

    in do
    args <- getArgs
    case args of
        [a] -> startWithOneArg a
        [a, b] -> startWithTwoArgs a b
        _anyOtherArgumentsCount -> help
