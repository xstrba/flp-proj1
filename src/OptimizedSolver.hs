module OptimizedSolver (solveSa) where
import Types (ReturnType (Print, Failure))

solveSa :: ReturnType -> ReturnType
solveSa (Print a) = Failure False
solveSa a = a