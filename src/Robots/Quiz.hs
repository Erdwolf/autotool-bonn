{-# LINE 1 "Robots/Quiz.hs.drift" #-}
{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Robots.Quiz where

import Inter.Types

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data RC = RC { width :: Integer -- ^ feld geht von (-w.-w) .. (w,w)
	     , num :: Int -- ^ of robots
	     , at_least :: Int -- ^ req length of solution
	     , search_width :: Int -- ^ at most that many nodes per level
	     }
     deriving ( Typeable )

{-! for RC derive: Reader, ToDoc, Haskell2Xml !-}

rc :: RC
rc = RC { width = 3
	, num = 5
	, at_least = 5
	, search_width = 1000
	}

-- Local Variables:
-- mode: haskell
-- End:
instance Reader RC where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "RC"
			   my_braces ((do my_reserved "width"
					  my_equals
					  aa <- readerPrec 0
					  my_comma
					  my_reserved "num"
					  my_equals
					  ab <- readerPrec 0
					  my_comma
					  my_reserved "at_least"
					  my_equals
					  ac <- readerPrec 0
					  my_comma
					  my_reserved "search_width"
					  my_equals
					  ad <- readerPrec 0
					  return (RC aa ab ac ad)))))

instance ToDoc RC where
    toDocPrec d (RC aa ab ac ad) = docParen (d >= 10)
	      (text "RC" <+> dutch_record
	       [text "width" <+> equals <+> toDocPrec 0 aa,
		text "num" <+> equals <+> toDocPrec 0 ab,
		text "at_least" <+> equals <+> toDocPrec 0 ac,
		text "search_width" <+> equals <+> toDocPrec 0 ad])

instance Haskell2Xml RC where
    toHType v =
	Defined "RC" []
		[Constr "RC" [] [toHType aa,toHType ab,toHType ac,toHType ad]]
      where
	(RC aa ab ac ad) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "RC" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,cs01)-> (\(ac,cs02)-> (\(ad,
							 _)-> (RC aa ab ac ad, etc))
						      (fromContents cs02))
					(fromContents cs01))
			  (fromContents cs00))
	    (fromContents cs)
    toContents v@(RC aa ab ac ad) =
	[mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
						     toContents ab,toContents ac,toContents ad])]

--  Imported from other files :-
