import NFA.Synchronize
import System

main = do
    [ n ] <- getArgs
    dorunrun $ read n

