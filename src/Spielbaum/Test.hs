module Spielbaum.Test where

import Spielbaum.Wort

------------------------------------------------------------------------
-- some test-things
------------------------------------------------------------------------

a_wort :: Wort Char
a_wort = Wort { inhalt = "011"
              , regeln = [ Regel { from = "01" , to = "10" } 
                         , Regel { from = "1"  , to = "2"  }
                         ]
              }

b_wort :: Wort Char
b_wort = Wort { inhalt = "0101"
              , regeln = [ Regel { from = "01" , to = "10" } 
                         , Regel { from = "1"  , to = ""  }
                         ]
              }

