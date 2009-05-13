{-# OPTIONS -fallow-overlapping-instances #-}

module SCS.Quiz where

--  $Id$


import SCS.Data
import SCS.Config

import Inter.Types

import Autolib.Set
import Autolib.Reader
import Autolib.ToDoc

import Util.Datei
import Util.Cache
import Autolib.Util.Seed
import Autolib.Util.Zufall

import Data.List ( sort )
import Control.Monad ( forM )

roll :: ( InstanceC a )
     => Config a 
     -> IO ( [a], Instance a )
roll conf = do
     let sigma = setToList $ alphabet conf
     sol <- someIO sigma $ length_solution conf
     subs <- forM [ 1 .. num_subwords conf ] $ \ k -> do
         ps <- permutation [ 0 .. length sol - 1 ]
         return $ do 
               p <- sort $ take ( length_subwords conf ) ps
               return $ sol !! p
     return ( sol
            , Instance { contents = subs
                       , max_length = length sol
                       }
            )
