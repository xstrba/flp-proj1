module BruteSolver (solveBf) where
import Types (ReturnType (Print, Success, Debug))
import State (initState, filledWeight)

solveBf :: ReturnType -> ReturnType
solveBf (Print a) = Debug $ show (initState a [])
solveBf a = a