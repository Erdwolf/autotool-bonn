module Code.Huffman.Boiler where


import Code.Type
import Code.Huffman.Partial
import Code.Huffman.Throw
import Code.Huffman.Config

import Inter.Quiz
import Inter.Types

import Autolib.Reader
import Autolib.ToDoc

make_fixed :: Make
make_fixed = direct Huffman 
           $ Frequency $ listToFM
           $ zip [ 'a' .. ] [ 14 :: Int, 23, 94, 87, 15, 90, 18, 35, 71 ]

make_quiz :: Make
make_quiz = quiz Huffman Code.Huffman.Config.example

instance ( Show a, ToDoc a, Reader a, Reader [a], ToDoc [a], Ord a )
    => Generator Huffman ( Config a ) ( Frequency a ) where
       generator _ conf key = throw conf
        
instance Project Huffman ( Frequency a ) ( Frequency a ) where
    project _ f = f

instance OrderScore Huffman where
    scoringOrder _ = None

