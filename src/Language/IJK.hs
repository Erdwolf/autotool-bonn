module Language.IJK ( ijk ) where

-- -- $Id$

import Language.Type
import Language.Gleich

ijk :: Language
ijk = ( ordered_ungleich "abc" )
      { nametag = "IJK"
      }














