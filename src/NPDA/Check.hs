module NPDA.Check

-- $Id$

( check
, Config (..), Mod (..)
)

where

import NPDA.Type
import NPDA.Test
import Language

import Util.Wort
import Util.Edit
import Util.Seed

import Reporter
import Monad (guard)
import List (nub, partition)
import ToDoc

check
    :: (Ord y, Ord z
       , Show y, Show z
       , ToDoc y, ToDoc z
       , ToDoc [y], ToDoc [z]
    ) 
    => Config
    -> Language
    -> String
    -> NPDA Char y z 
    -> IO String

check conf l mat a = do

    seed $ read mat

    here   <- samples      l 20 5
    there  <- anti_samples l 20 5

   -- die kleinen sollten ja auch schnell zu testen sein
    let klein = take 100 $ do n <- [0 .. ] ; alle ( setToList $ alphabet l ) n

    let (yeah, noh) = partition (contains l) $ nub $ klein ++ here ++ there

    reporter $ do
        inform $ text $ "Ihr Kellerautomat soll die Sprache " ++ abbreviation l ++ " akzeptieren."
        test conf yeah noh a


    



