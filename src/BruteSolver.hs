module BruteSolver (solveBf) where
import Types (ReturnType (Print, Success))

solveBf :: ReturnType -> ReturnType
solveBf (Print a) = Success [1, 2]
solveBf a = a