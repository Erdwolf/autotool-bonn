import NFA.Synchronize
import System

main = do
    [ f, g ] <- getArgs
    dorun (read f) (read g)
