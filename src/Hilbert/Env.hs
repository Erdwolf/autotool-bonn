module Hilbert.Env 

( Env -- abstract
, empty, contents, make
, look
, extend
, mkfm
)

where


import Autolib.ToDoc hiding ( empty )
import Autolib.Reader
import Autolib.FiniteMap
import Autolib.Reporter
import Autolib.TES.Identifier
import Autolib.TES
import Autolib.Size

data Env a = 
     Env { contents :: [ ( Identifier, a ) ] }

make pairs = Env pairs
empty = make []

instance Size ( Env a ) where
    size = length . contents

instance ToDoc a => ToDoc ( Env a ) where
    toDoc ( Env e ) = dutch_record $ do
        ( id, val ) <- e
	return $ hsep [ toDoc id, text "=", toDoc val ]

instance Reader a => Reader ( Env a ) where
    reader = fmap Env
	   $ my_braces 
	   $ ( `Autolib.Reader.sepBy` my_comma )
	   $ do
        id <- reader
	my_reserved "="
	val <- reader
	return ( id, val )

--------------------------------------------------------------------

-- | must be there
look :: ToDoc a => Env a -> Identifier -> Reporter a
look ( Env env ) this =
    case lookupFM ( listToFM env ) this of
	 Just x -> return x
	 Nothing -> reject $ vcat
		 [ text "identifier" <+> toDoc this 
		 , text "not in environment" <+> toDoc ( Env env )
		 ]

extend :: ToDoc a =>
       Env a -> ( Identifier, a ) -> Reporter ( Env a )
extend ( Env env ) ( this, val ) = do
    case lookupFM ( listToFM env ) this of
        Just x -> reject $ vcat
	       [ text "identifier" <+> toDoc this
	       , text "already in environment" <+> toDoc ( Env env )
	       ]
	Nothing -> return $ Env $ env ++ [ ( this, val ) ]

mkfm :: ToDoc a => 
     Env a -> Reporter ( FiniteMap Identifier a )
mkfm ( Env env ) = do
    Env checked <- foldM extend ( Env [] ) env
    return $ listToFM checked
