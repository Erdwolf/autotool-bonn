{-# LINE 1 "Inter/Action.hs.drift" #-}
{-# OPTIONS -fth -fallow-overlapping-instances -fglasgow-exts #-}

-- | Interface für externe Korrektoren

module Inter.Action where

import Control.Types

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

import Network.XmlRpc.THDeriveXmlRpcType
import Network.XmlRpc.Internals

-- | die Nummer dessen, der sich einloggt (Tutor)
data Actor
     = Actor { schule   :: String
	     , matrikel :: String
	     , passwort :: String 
	     }
     deriving ( Eq, Ord, Typeable )

{-! for Actor derive: ToDoc, Reader, Haskell2Xml !-}

$(asXmlRpcStruct ''Actor)

data Problem
     = Problem { vorlesung :: String 
	       , aufgabe :: String
	       }
     deriving ( Eq, Ord, Typeable )

{-! for Problem derive: ToDoc, Reader, Haskell2Xml !-}

$(asXmlRpcStruct ''Problem)

data Answer =
     Answer { wert :: Wert
	    , kommentar :: String
	    }
     deriving ( Eq, Ord, Typeable )

$(asXmlRpcStruct ''Answer)

{-! for Answer derive: ToDoc, Reader, Haskell2Xml !-}

instance XmlRpcType Wert where
    toValue w = case w of
        Reset   -> toValue [("tag", toValue "Reset")]
        Pending -> toValue [("tag", toValue "Pending")]
        No -> toValue [("tag", toValue "No")]
        Ok s -> toValue [("tag", toValue "OK"), ("size", toValue s) ]
	Okay {punkte=p, size=s} -> toValue [("tag", toValue "Okay")
                  , ("punkte", toValue p), ("size", toValue s) ]
    fromValue v = do
        it <- fromValue v
	tag <- getField "tag" it
	case tag of
	    "Reset" -> return Reset
	    "Pending" -> return Pending
	    "No" -> return No
	    "Ok" -> do s <- getField "size" it ; return $ ok s
	    "Okay" -> do 
               p <- getField "punkte" it; s <- getField "size" it
	       return $ Okay { punkte = p , size = s }
    getType _ = TStruct

-- FIXME
instance XmlRpcType Integer where
    toValue = toValue . ( fromIntegral :: Integer -> Int ) 
    fromValue = fmap ( fromIntegral :: Int -> Integer ) . fromValue
    getType _ = TInt -- FIXME

-- Local Variables:
-- mode: haskell
-- End:
instance ToDoc Answer where
    toDocPrec d (Answer aa ab) = docParen (d >= 10)
	      (text "Answer" <+> dutch_record
	       [text "wert" <+> equals <+> toDocPrec 0 aa,
		text "kommentar" <+> equals <+> toDocPrec 0 ab])

instance Reader Answer where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Answer"
			   my_braces ((do my_reserved "wert"
					  my_equals
					  aa <- readerPrec 0
					  my_comma
					  my_reserved "kommentar"
					  my_equals
					  ab <- readerPrec 0
					  return (Answer aa ab)))))

instance Haskell2Xml Answer where
    toHType v =
	Defined "Answer" [] [Constr "Answer" [] [toHType aa,toHType ab]]
      where
	(Answer aa ab) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Answer" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,_)-> (Answer aa ab, etc)) (fromContents cs00))
	    (fromContents cs)
    toContents v@(Answer aa ab) =
	[mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
						     toContents ab])]

instance ToDoc Problem where
    toDocPrec d (Problem aa ab) = docParen (d >= 10)
	      (text "Problem" <+> dutch_record
	       [text "vorlesung" <+> equals <+> toDocPrec 0 aa,
		text "aufgabe" <+> equals <+> toDocPrec 0 ab])

instance Reader Problem where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Problem"
			   my_braces ((do my_reserved "vorlesung"
					  my_equals
					  aa <- readerPrec 0
					  my_comma
					  my_reserved "aufgabe"
					  my_equals
					  ab <- readerPrec 0
					  return (Problem aa ab)))))

instance Haskell2Xml Problem where
    toHType v =
	Defined "Problem" [] [Constr "Problem" [] [toHType aa,toHType ab]]
      where
	(Problem aa ab) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Problem" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,_)-> (Problem aa ab, etc)) (fromContents cs00))
	    (fromContents cs)
    toContents v@(Problem aa ab) =
	[mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
						     toContents ab])]

instance ToDoc Actor where
    toDocPrec d (Actor aa ab ac) = docParen (d >= 10)
	      (text "Actor" <+> dutch_record
	       [text "schule" <+> equals <+> toDocPrec 0 aa,
		text "matrikel" <+> equals <+> toDocPrec 0 ab,
		text "passwort" <+> equals <+> toDocPrec 0 ac])

instance Reader Actor where
    atomic_readerPrec d = readerParenPrec d $ \ d -> do
		      ((do guard (d < 9)
			   my_reserved "Actor"
			   my_braces ((do my_reserved "schule"
					  my_equals
					  aa <- readerPrec 0
					  my_comma
					  my_reserved "matrikel"
					  my_equals
					  ab <- readerPrec 0
					  my_comma
					  my_reserved "passwort"
					  my_equals
					  ac <- readerPrec 0
					  return (Actor aa ab ac)))))

instance Haskell2Xml Actor where
    toHType v =
	Defined "Actor" []
		[Constr "Actor" [] [toHType aa,toHType ab,toHType ac]]
      where
	(Actor aa ab ac) = v
    fromContents (CElem (Elem constr [] cs):etc)
	| "Actor" `isPrefixOf` constr =
	    (\(aa,cs00)-> (\(ab,cs01)-> (\(ac,_)-> (Actor aa ab ac, etc))
					(fromContents cs01))
			  (fromContents cs00))
	    (fromContents cs)
    toContents v@(Actor aa ab ac) =
	[mkElemC (showConstr 0 (toHType v)) (concat [toContents aa,
						     toContents ab,toContents ac])]

--  Imported from other files :-
