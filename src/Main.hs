import Shift.Computer
import Shift.Meta
import Shift.Generate

import Reporter
import System

main = do
    [ nn, kk ] <- getArgs
    mainf (read nn) (read kk) 


