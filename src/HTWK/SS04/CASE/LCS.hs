module HTWK.SS04.CASE.LCS where

--  $Id$

import LCS.Instance
import LCS.Quiz

import Inter.Types

generates :: [ IO Variant ]
generates = map ( return . Variant )
    [ fixed "Demo" True ( "ABCABBA", "CBABAC" )
    , fixed "Long" False
		  ( "bcbcccbccbaabbbacaaaaccabacaccccbbbbcccbbcacaaab"
		  , "baccabccbbacbabccccacacbabbacaabccaabacccabbaaabcbba"
		  )
    , quiz conf
    ]