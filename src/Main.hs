import Shift.Computer
import Shift.Meta

import Reporter
import System

main = do
    [ nn, kk ] <- getArgs
    reporter $ mf $ Meta { start = (read nn), diff =  (read kk) }


