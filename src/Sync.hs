import NFA.Synchronize
import System

main = do
    [ n ] <- getArgs
    run $ read n
