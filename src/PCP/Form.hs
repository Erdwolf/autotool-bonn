-- | PCP instances of very special form

module PCP.Form where

import PCP.Type

form :: String -> PCP Char
form w = [(w, "0"), ("0","1"), ("1", w)]

spiegel :: PCP Char -> PCP Char
spiegel p = do
    (l, r) <- p
    let f '0' = '1' ; f '1' = '0'
        h = reverse . map f
    return (h l, h r)


