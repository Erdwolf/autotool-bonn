import NFA.Gen
import NFA.Genetic
import System

main = do
    [ n ] <- getArgs
    this <- evolve $ conf $ read n
    print this

