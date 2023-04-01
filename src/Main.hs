-- ------------------------------------------------
-- @file Main.hs  ---------------------------------
-- @author Boris Štrbák (xstrba05)  ---------------
-- ------------------------------------------------
-- Main module for handling file input or stdin,  -
-- parsing arguments and running actions for  -----
-- options i, b and o.  ---------------------------
-- ------------------------------------------------

module Main where
import System.Environment (getArgs, getProgName)
import Types (ReturnType (Error))
import Helpers (printResult)
import Control.Exception (try, SomeException)
import Parser (parseKnapsack)
import BruteSolver (solveBf)
import OptimizedSolver (solveSa)

-- print help message to stdout
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

-- won't throw exception if there is some while reading file
-- instead will return custom type Error
safeFileReader :: String -> (ReturnType -> IO ()) -> IO ()
safeFileReader fileName fn = do
    fileContentsOrError <- try (readFile fileName) :: IO (Either SomeException String)
    case fileContentsOrError of
        Left errorMsg -> printResult $ Error (show errorMsg)
        Right fileContents -> fn $ parseKnapsack fileContents

-- won't throw exception if there is some while reading stdin
-- instead will return custom type Error
safeStdinReader :: (ReturnType -> IO ()) -> IO ()
safeStdinReader fn = do
    fileContentsOrError <- try getContents :: IO (Either SomeException String)
    case fileContentsOrError of
        Left errorMsg -> printResult $ Error (show errorMsg)
        Right fileContents -> fn $ parseKnapsack fileContents

-- read file or stdin
-- run action given by command argument
-- or display help
main :: IO ()
main = let
    startWithTwoArgs :: String -> String -> IO ()
    startWithTwoArgs a b = case a of
        "-i" -> safeFileReader b printResult
        "-b" -> safeFileReader b (printResult.solveBf)
        "-o" -> safeFileReader b solveSa
        _anyOtherOption -> help
    startWithOneArg :: String -> IO ()
    startWithOneArg a = case a of
        "-i" -> safeStdinReader printResult
        "-b" -> safeStdinReader (printResult.solveBf)
        "-o" -> safeStdinReader solveSa
        _anyOtherOption -> help

    in do
    args <- getArgs
    case args of
        [a] -> startWithOneArg a
        [a, b] -> startWithTwoArgs a b
        _anyOtherArgumentsCount -> help
