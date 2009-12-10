{-# LANGUAGE TemplateHaskell #-}

module Number.Float.Data where

import Prelude hiding ( exponent )
import qualified Number.Base.Data as B
import Number.Wert 

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Size

import Data.Ratio
import Data.Typeable


-------------------------------------------------------------------

data Signed a = Signed { negative :: Bool
                       , contents :: a
                       }
    deriving ( Typeable )

instance ( Num b, Wert_at a b ) => Wert_at (Signed a) b where
    wert_at b s = ( if negative s then negate else id ) 
           $ wert_at b $ contents s

instance Size a => Size (Signed a) where 
    size s = size (contents s)

instance ( ToDoc a, B.Range a ) => B.Range ( Signed a ) where
    range b s = silent $ do
        inform $ fsep
               [ text "teste Ziffern von", toDoc s
               , text "bezÃ¼glich Basis", toDoc b
               ]
        nested 4 $ B.range b $ contents s

instance ToDoc a => ToDoc (Signed a) where
    toDoc s = ( if negative s then Autolib.ToDoc.char '-' else empty ) 
            <> toDoc ( contents s )

instance Reader a => Reader (Signed a) where
    reader = do
        sign <- option False $   
                 do Autolib.Reader.char '+' ; my_whiteSpace ; return False
             <|> do Autolib.Reader.char '-' ; my_whiteSpace ; return True
        c <- reader
        return $ Signed { negative = sign, contents = c }

-------------------------------------------------------------------

data Natural = Natural
	     { ziffern :: [ B.Ziffer ] 
	     }
     deriving Typeable

instance Wert_at Natural Integer where
    wert_at b n = wert_at b $ ziffern n

instance Size Natural where
    size n = length $ ziffern n

instance B.Range Natural where
    range b n = silent $ do
        inform $ fsep
               [ text "teste Ziffern von", toDoc n
               , text "bezÃ¼glich Basis", toDoc b
               ]
        nested 4 $ B.range b $ ziffern n

instance ToDoc Natural where
    toDoc n = hcat $ map toDoc $ ziffern n

instance Reader Natural where
    reader = do
        zs <- many reader
	return $ Natural { ziffern = zs }

-------------------------------------------------------------------

-- | where "decimal" point is mandatory
data Fixed = Fixed 
	   { pre :: Natural
	   , post :: Natural
	   }
     deriving ( Typeable )

instance Wert_at Fixed Rational where
    wert_at b f = 
        let we, wo :: Integer
            we = wert_at b $ pre f
            wo = wert_at b $ post f
        in  fromIntegral we + wo % fromIntegral b ^ (length $ ziffern $ post f)

instance Size Fixed where
    size f = size (pre f) + size (post f)

instance B.Range Fixed where
    range b f = silent $ do
        inform $ fsep
               [ text "teste Ziffern von", toDoc f
               , text "bezÃ¼glich Basis", toDoc b
               ]
        nested 4 $ B.range b $ pre f
        nested 4 $ B.range b $ post f

instance ToDoc Fixed where
    toDoc f = 
        hcat [ toDoc $ pre f , Autolib.ToDoc.char '.' , toDoc $ post f ]

instance Reader Fixed where
    reader = do
        e <- reader
	Autolib.Reader.char '.'
        o <- reader
	return $ Fixed { pre = e, post = o }

-------------------------------------------------------------------

data Zahl = Zahl
          { basis :: Int
          , mantisse :: Signed Fixed
          , exponent :: Signed Natural
          }
    deriving ( Typeable )

example :: Zahl
example = Zahl
        { basis = 2
        , mantisse = read "1.01"
        , exponent = read "-10"
        }

instance Wert Zahl Rational where
    wert z = 
        let b :: Int
            b = basis z
            wm :: Rational
            wm = wert_at b $ mantisse z
            we :: Integer
            we = wert_at b $ exponent z
        in  wm *  fromIntegral b ^^ we

instance B.Range Zahl where
    range b z = do
        B.range b $ mantisse z
        B.range b $ exponent z

instance Size Zahl where
    size z = size ( mantisse z ) + size ( exponent z )

$(derives [makeReader, makeToDoc] [''Zahl])

ist_normalisiert :: Zahl -> Bool
ist_normalisiert z = case ziffern $ pre $ contents $ mantisse z of
    [ x ] | x > B.Ziffer 0 -> True
    otherwise ->
       let zero z = all ( == B.Ziffer 0 ) $ ziffern z
       in  all zero [ pre $ contents $ mantisse z
		    , post $ contents $ mantisse z
		    , contents $ exponent z
		    ]

-- Local Variables: 
-- mode:haskell
-- End: 



