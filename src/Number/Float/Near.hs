-- |  $Id$

module Number.Float.Near where

import Autolib.Reporter
import Autolib.ToDoc

import Number.Wert
import Number.Base.Data ( Ziffer (..))
import Number.Float.Data
import Number.Float.Config as C

import Data.Ratio

-- | z ist gute darstellung von x (schlechte implementierung)':
-- wenn in x im intervall  wert(z) - delta/2, wert(z) + delta/2 liegt
-- wobei eps hälfte des werts des letzten bits
-- (richtig wäre: wenn z die zu x nächstliegende maschinenzahl ist)
-- weil die Zahlen normalisiert sind, geht es evtl. doch gut
-- (sonst definitiv nicht)

near :: C.Config -> Rational -> Zahl -> Reporter ()
near c x z = do

    inform $ vcat
           [ text "ist z =" <+> toDoc z
           , text "eine gute Näherung für x =" <+> toDoc x
           , text "unter allen Gleitkommazahlen mit" <+> toDoc c <+> text "?"
           ]
    let w :: Rational
        w = wert z
        d :: Rational
        d = delta c z
        lo :: Rational
        lo = w - d / 2
        hi :: Rational
        hi = w + d / 2
    inform $ vcat
           [ text "Wert von z ist" 
                  <+> toDoc ( (fromRational w) :: Double )
           , text "Delta (Wert der letzen Stelle) ist" 
                  <+> toDoc ( (fromRational d) :: Double )
           , text "Intervall z - Delta/2 .. z + Delta/2 ist"
                  <+> toDoc ( (fromRational lo) :: Double
                            , (fromRational hi) :: Double
                            )
           ]
    assert ( lo <= x && x <= hi )
           $ text "liegt  x  in diesem Intervall?"
        
    

delta :: C.Config -> Zahl -> Rational
delta c z = 
    let msm = C.max_stellen_mantisse c
        d = z { mantisse = Signed 
                         { negative = False
                         , contents = Fixed
                                    { pre = Natural [ Ziffer 0 ]
                                    , post = Natural
                                           $ replicate (msm - 2) (Ziffer 0)
                                             ++ [ Ziffer 1 ]
                                    }
                         }
              -- exponent bleibt
              }
    in  wert d

