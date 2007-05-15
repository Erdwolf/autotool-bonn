module Collatz.Roll ( roll ) where

--  $Id$

import qualified Collatz.Parameter as P
import Collatz.Config


import Autolib.Util.Zufall

import Local
import Control.Monad ( when )

one :: Config -> IO ( Integer, P.Parameter )
one conf = do
    x <- randomRIO ( min_start conf, max_start conf )
    let pair =  ( x, P.compute x )
    return pair

conforms :: Config -> P.Parameter -> Bool
conforms conf p = and
    [ min_top conf <= P.top p
    , P.top p <= max_top conf
    , min_length conf <= P.length p
    , P.length p <= max_length conf
    ]

roll :: Config -> IO ( Integer, P.Parameter )
roll conf =
        one conf `repeat_until` \ ( x, p ) -> conforms conf p


