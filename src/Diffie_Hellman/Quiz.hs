{-# LANGUAGE MultiParamTypeClasses #-}

module Diffie_Hellman.Quiz 

( make
, Param (..)
, Config (..)
)

where

import Diffie_Hellman.Param
import Diffie_Hellman.Config
import Diffie_Hellman.Break hiding ( make )

import Prime.Check

import Autolib.Util.Zufall
import Autolib.Util.Seed

import Inter.Types
import Inter.Quiz hiding ( make )


roll par = do
    start <- randomRIO ( 10 ^ digits par , 10 ^ ( 1 + digits par ) )
    let (pp, gg) = Prime.Check.next start
    aa <- randomRIO ( 1, pp - 1 )
    bb <- randomRIO ( 1, pp - 1 )
    return $ Config
           { public = Public { p = pp
                             , g = gg 
                             , g_a = powMod gg aa pp
                             , g_b = powMod gg bb pp
                             }
           , private = Private { a = aa
                               , b = bb
                               , g_ab = powMod gg ( aa * bb ) pp
                               }
           }
 
instance Generator Diffie_Hellman_Code_Break Param Config where
    generator _ p key = roll p

instance Project Diffie_Hellman_Code_Break Config Config where
    project _ = id

make :: Make
make = quiz Diffie_Hellman_Code_Break Diffie_Hellman.Param.example

