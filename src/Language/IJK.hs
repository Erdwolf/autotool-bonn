module Language.IJK ( ijk ) where

-- $Id$

import Language.Type
import Language.Gleich

ijk :: Language
ijk = ( komplement $ ordered_gleich "abc" )
       { nametag = "IJK"
       , abbreviation = "{ a^i b^j c^k | i /= j  oder j /= k }" 
       }














