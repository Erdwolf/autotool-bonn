module NPDA.Type

-- nicht-deterministischer Kellerautomat
-- $Id$

( module NPDA.Type
, module Set
, module FiniteMap
)

where


import Set
import FiniteMap
import Schichten
import Size

import ToDoc
import ReadSet
import ReadFM
import Maybe
import Reporter

data Modus z = Leerer_Keller | Zustand (Set z) 
     deriving (Show, Read)

instance ToDoc [z] => ToDoc (Modus z) where
     toDoc Leerer_Keller = text "Leerer_Keller"
     toDoc ( Zustand zs ) = text "Zustand" <+> parens (toDoc zs)


data NPDA x y z = 
     NPDA { eingabealphabet  :: Set x 
	  , kelleralphabet   :: Set y 
	  , zustandsmenge    :: Set z 
	  , startzustand     :: z
	  , startsymbol	     :: y
	  , akzeptiert	     :: Modus z
	  , tafel	     :: FiniteMap (Maybe x, z, y) (Set (z, [y]))
	  }
     deriving (Read)

instance (ToDoc (Maybe x), ToDoc x, ToDoc y, ToDoc z,
	 ToDoc [x], ToDoc [y], ToDoc [z]) 
	 => ToDoc (NPDA x y z) where
    toDoc a = text "NPDA" <+> braces ( fsep $ punctuate comma
       [ text "eingabealphabet" <+> equals <+> toDoc ( eingabealphabet a )
       , text "kelleralphabet" <+> equals <+> toDoc ( kelleralphabet a )
       , text "zustandsmenge" <+> equals <+> toDoc ( zustandsmenge a )
       , text "startsymbol" <+> equals <+> toDoc ( startsymbol a )
       , text "startzustand" <+> equals <+> toDoc ( startzustand a )
       , text "akzeptiert" <+> equals <+> toDoc ( akzeptiert a )
       , text "tafel" <+> equals <+> toDoc ( tafel a )
       ] )

instance (ToDoc (Maybe x), ToDoc x, ToDoc y, ToDoc z,
	 ToDoc [x], ToDoc [y], ToDoc [z]) 
         => Show ( NPDA x y z ) where
    show = render . toDoc

instance Size (NPDA x y z) where
    size a = sum $ do
	 ( x, ys ) <- fmToList $ tafel a
	 y <- setToList ys
	 return 1

------------------------------------------------------------------------

lookupset :: Ord a => FiniteMap a (Set b) -> a -> Set b
lookupset fm x = case lookupFM fm x of
    Just m -> m; Nothing -> emptySet

the :: Maybe a -> a
-- mit absturzgefahr
the = fromMaybe ( error "the" )

------------------------------------------------------------------------

sane :: ( Ord x, Ord y, Ord z
	 , Show x, Show y, Show z
	 , ToDoc [x], ToDoc [y], ToDoc [z]
	 , ToDoc x, ToDoc y, ToDoc z
	 )
      => NPDA x y z -> Reporter ()
sane a = do
    inform $ text "Sie haben eingesandt:"
    inform $ nest 4 $ toDoc a

    when ( isEmptySet $ eingabealphabet a ) 
	 $ reject $ text "das Eingabe-Alphabet ist leer"

    let s = startsymbol a
    when ( not $ s `elementOf` kelleralphabet a )
	 $ reject $ text 
	 $ "das Startsymbol " ++ show s ++ " geh�rt nicht zum Kelleralphabet"

    let z = startzustand a
    when ( not $ z `elementOf` zustandsmenge a )
	 $ reject $ text
	 $ "der Startzustand " ++ show z ++ " geh�rt nicht zur Zustandsmenge"

    case akzeptiert a of
        Zustand zs -> mapM_ ( \ z -> 
	     when ( not $ z `elementOf` zustandsmenge a)
		  $ reject $ text
		  $ "der Endzustand " ++ show z ++ " geh�rt nicht zur Zustandsmenge"
                            ) ( setToList zs )
        _ -> return ()

    let check_rule (l @ (mx, z, y), r @ (z',y')) = do
          let falsch msg = reject 
		 $ hsep [ text "Fehler in Regel", toDoc l, text "->", toDoc r ]
		 $$ nest 4 ( text msg )
          when ( isJust mx ) 
	       $ when ( not $ the mx `elementOf` eingabealphabet a )
		      $ falsch
		      $ show (the mx) ++ " geh�rt nicht zum Eingabealphabet"
          mapM_ ( \ t -> when ( not $  t `elementOf` kelleralphabet a )
		      $ falsch
		      $ show t ++  " geh�rt nicht zum Kelleralphabet" )
	        ( y : y' )
          mapM_ ( \ t -> when ( not $ t `elementOf` zustandsmenge a )
		      $ falsch
                      $ show t ++ " geh�rt nicht zur Zustandsmenge" )
	        [ z, z' ]

    mapM_ check_rule $ do
          (mxzy,zys) <- fmToList $ tafel a
          (zy') <- setToList zys
	  return (mxzy,zy')


    inform $ text "Das ist wirklich ein nichtdeterministischer Kellerautomat."








