import Shift.Boiler
import System

-- $Id$

-- example: Main False 10 1000 [20,30,51,71]

main = do
    [ nn, kk, mm, ff ] <- getArgs
    -- compacting? Bool
    -- sequence width Int
    -- take that many Int
    -- Pins
    mainf (read nn) (read kk) (read mm) ( read ff )



