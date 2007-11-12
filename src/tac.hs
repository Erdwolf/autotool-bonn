import Autolib.Genetic.Parallel
import TAC.Find

import System.Environment

main = do
    [ num ] <- getArgs
    evolve $ conf $ read num
