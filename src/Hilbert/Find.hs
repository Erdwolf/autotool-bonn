import Hilbert.Look

import System.Environment

main = do
   argv <- getArgs
   search $ read $ unwords argv
