-- import qualified Autolib.Genetic.Parallel as P
-- import qualified Autolib.Genetic.Central as C

import qualified Genetic.Parallel as P
import qualified Genetic.Central as C

import TAC.Find

import System.Environment

main = do
    [ num ] <- getArgs
    P.evolve $ conf $ read num
