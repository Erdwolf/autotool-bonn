{-# OPTIONS -fglasgow-exts #-}

module Exp.Roll where

--  $Id$

import Autolib.Exp
import Autolib.NFA
import Autolib.Exp.Some

import Exp.Property

roll :: NFAC c Int
     => [ Property c ] 
     -> IO ( RX c, NFA c Int )
roll props = do
    let [ alpha ] = do Alphabet alpha <- props ; return alpha
    let [ s     ] = do Max_Size s     <- props ; return s
    nontrivial alpha ( s `div` 2 )


