module HTWK.SS04.CASE.Activity.Language where

--   $Id$

import NFA.Synthese

import Autolib.Sets
import Autolib.ToDoc

languages :: [ SI ]
languages =  [ l1, l2, l3, l4, l5 ]

dft = SI  { name = undefined , ausdruck = undefined , alphabet = undefined
	  , deterministisch = undefined, beschreibung = undefined
	  }

l1 = dft  { name = "L1"
	  , ausdruck = read "ab $ bc"
	  , alphabet = mkSet "abc"
	  }

l2 = dft  { name = "L2"
	  , ausdruck = read "(a $ b) (b $ c)"
	  , alphabet = mkSet "abc"
	  }

l3 = dft  { name = "L3"
	  , ausdruck = read "bc $ a^*"
	  , alphabet = mkSet "abc"
	  }

l4 = dft  { name = "L4"
	  , ausdruck = read "ba^* $ ab^*"
	  , alphabet = mkSet "ab"
	  }

l5 = dft  { name = "L5"
	  , ausdruck = read "(ab)^* $ (cd)^*"
	  , alphabet = mkSet "abcd"
	  }

