{-# LANGUAGE FlexibleContexts #-}
module NFA.Roll where

--  $Id$

import Autolib.NFA
import Autolib.NFA.Some

import NFA.Property

roll :: NFAC c Int
     =>  [ Property c ] -> IO ( NFA c Int )
roll props = do
    let [ alpha ] = do Alphabet alpha <- props ; return alpha
    let [ s     ] = do Max_Size s     <- props ; return s
    nontrivial alpha s


