-- | PCP instances of very special form

module PCP.Form where

import PCP.Type

form :: String -> PCP Char
form w = [(w, "0"), ("0","1"), ("1", w)]

-- | use additional letter w,
-- only increasing rules
iform :: String -> PCP Char
iform w = [ (w, "0"), ("0","1"), ("1", "w"), ("w", "0") ]

-- | only increasing rules
dform :: String -> PCP Char
dform w = [ ("1", w), ("0","1"), ("1", "w"), ("w", "0") ]

spiegel :: PCP Char -> PCP Char
spiegel p = do
    (l, r) <- p
    let fun = reverse
            . map ( \ c -> case c of '0' -> '1'; '1' -> '0' )
    return (fun l, fun r)

