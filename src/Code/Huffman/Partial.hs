{-# OPTIONS -fallow-undecidable-instances -fglasgow-exts -fallow-overlapping-instances #-}

module Code.Huffman.Partial where

--  $Id$

import Code.Type
import Code.Check

import Code.Huffman.LR
import Code.Huffman.Test

import Challenger.Partial
import Data.Typeable
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader


data Huffman = Huffman deriving ( Eq, Ord, Show, Read, Typeable )

instance -- ( ToDoc a, ToDoc [a], Ord a, ToDoc [ (a, [ LR ] )] ) 
     ( Reader a, ToDoc a, ToDoc [a], Ord a )
	 => Partial Huffman ( Frequency a ) ( Code a LR ) where

    describe p i = vcat
        [ text "gesucht ist ein optimaler Pr�fix-Code"
	, nest 4 $ vcat [ text "�ber dem Code-Alphabet [L, R]"
			, text "f�r die Verteilung" <+> toDoc i
			]
	]

    initial p i = listToFM $ do
        ( k, x ) <- zip [0..] $ keysFM i
	return ( x, replicate k L ++ [ R ] )

    partial p i b = do
        isprefix b
	istotal ( mkSet $ keysFM i ) b

    total p i b  = do
        isoptimalprefix i b
        
