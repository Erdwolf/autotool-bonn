module PCP.Type where

--  $Id$

type PCP c = [([c],[c])]


turn pcp = do
       (l, r) <- pcp
       return (reverse r, reverse l)


