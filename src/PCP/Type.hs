module PCP.Type where

--  $Id$

type Pair c = ([c],[c])
type PCP c = [ Pair c ]

turn :: PCP c -> PCP c
turn pcp = do
       (l, r) <- pcp
       return (reverse r, reverse l)



