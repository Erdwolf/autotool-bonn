-- | $Id$

module Number.Float.Roll where

import Number.Base.Data ( Ziffer (..) )
import Number.Float.Data  as D
import qualified Number.Float.Config as C

import Autolib.Util.Wort
import Autolib.Util.Zufall

import System.Random


roll :: C.Config -> IO Zahl
roll c = do
    let digits = map Ziffer [ 0 .. C.basis c - 1 ] 
    x0 : xs <- someIO digits $ C.max_stellen_mantisse c
    let x = if x0 == Ziffer 0 then Ziffer 1 else x0
    let f = Fixed { pre = Natural [ x ], post = Natural xs }
    vman <- eins [ False, True ]
    let man = Signed { negative = vman, contents = f }

    cexp <- someIO digits $ C.max_stellen_exponent c
    vexp <- eins [ False, True ]
    let exp = Signed { negative = vexp, contents = Natural cexp }
    return $ Zahl
           { basis = C.basis c
           , mantisse = man
           , D.exponent = exp
           }
