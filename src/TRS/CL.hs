module TRS.CL where

-- -- $Id$

import TRS.System
import TRS.Symbol

cls :: System Symbol
cls = [ read "a (a (a (S, x), y), z) -> a (a (x, z), a (y, z))" ]

